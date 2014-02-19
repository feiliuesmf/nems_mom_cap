!--------------- MOM5 Ocean solo model -----------------
! This is the MOM5 ocean solo model component that's NUOPC compiant.
! The public ocn_register method sets up all the model services such as
! initialize, run and finalize.
!
! Author:  Fei.Liu@gmail.com
!
! 5/10/13
! This is now acting as a cap/connector between NUOPC driver and GFDL MOM5 code.
! Right now it's working in solo ocean mode where it does not export/import any data
!

module mom_cap_mod
  use constants_mod,            only: constants_init
  use data_override_mod,        only: data_override_init, data_override
  use diag_manager_mod,         only: diag_manager_init, diag_manager_end
  use field_manager_mod,        only: field_manager_init, field_manager_end
  use fms_mod,                  only: fms_init, fms_end, open_namelist_file, check_nml_error
  use fms_mod,                  only: close_file, file_exist, uppercase
  use fms_io_mod,               only: fms_io_exit
  use mpp_domains_mod,          only: domain2d, mpp_get_compute_domain
  use mpp_io_mod,               only: mpp_open, MPP_RDONLY, MPP_ASCII, MPP_OVERWR, MPP_APPEND, mpp_close, MPP_SINGLE
  use mpp_mod,                  only: input_nml_file, mpp_error, FATAL, NOTE, mpp_pe, mpp_npes, mpp_set_current_pelist
  use mpp_mod,                  only: stdlog, stdout, mpp_root_pe, mpp_clock_id
  use mpp_mod,                  only: mpp_clock_begin, mpp_clock_end, MPP_CLOCK_SYNC
  use mpp_mod,                  only: MPP_CLOCK_DETAILED, CLOCK_COMPONENT, MAXPES
  use time_interp_external_mod, only: time_interp_external_init
  use time_manager_mod,         only: set_calendar_type, time_type, increment_date
  use time_manager_mod,         only: set_time, set_date, get_time, get_date, month_name
  use time_manager_mod,         only: GREGORIAN, JULIAN, NOLEAP, THIRTY_DAY_MONTHS, NO_CALENDAR
  use time_manager_mod,         only: operator( <= ), operator( < ), operator( >= )
  use time_manager_mod,         only: operator( + ),  operator( - ), operator( / )
  use time_manager_mod,         only: operator( * ), operator( /= ), operator( > )
  use time_manager_mod,         only: date_to_string
  use time_manager_mod,         only: fms_get_calendar_type => get_calendar_type
  use ocean_model_mod,          only: ocean_model_init , update_ocean_model, ocean_model_end
  use ocean_model_mod,          only: ocean_model_restart, ocean_public_type, ocean_state_type
  use ocean_types_mod,          only: ice_ocean_boundary_type

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS      => routine_SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance

  use time_utils_mod

  implicit none
  private
  public SetServices

  type ocean_internalstate_type
    type(ocean_public_type),       pointer :: ocean_public_type_ptr
    type(ocean_state_type),        pointer :: ocean_state_type_ptr
    type(ice_ocean_boundary_type), pointer :: ice_ocean_boundary_type_ptr
  end type

  type ocean_internalstate_wrapper
    type(ocean_internalstate_type), pointer :: ptr
  end type

  integer   :: import_slice = 1
  integer   :: export_slice = 1

  contains
  !-----------------------------------------------------------------------
  !------------------- Solo Ocean code starts here -----------------------
  !-----------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call model_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP2, phase=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=ocean_model_finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    ! No need to change clock settings
    call ESMF_MethodAdd(gcomp, label=model_label_SetClock, &
      userRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_MethodAdd(gcomp, label=model_label_Advance, &
      userRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call MOM5_BuildImportFieldDictionary(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call MOM5_BuildExportFieldDictionary(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)

    type(ESMF_GridComp)                    :: gcomp
    type(ESMF_State)                       :: importState, exportState
    type(ESMF_Clock)                       :: clock
    integer, intent(out)                   :: rc

    type(ESMF_VM)                          :: vm
    type(ESMF_Time)                        :: MyTime
    type(ESMF_TimeInterval)                :: TINT
    
    type (ocean_public_type),      pointer :: Ocean_sfc   => NULL()
    type (ocean_state_type),       pointer :: Ocean_state => NULL()
    type(ice_ocean_boundary_type), pointer :: Ice_ocean_boundary => NULL()
    type(ocean_internalstate_wrapper)      :: ocean_internalstate

    type(time_type)                        :: Run_len      ! length of experiment 
    type(time_type)                        :: Time        
    type(time_type)                        :: Time_restart
    type(time_type)                        :: DT
    integer                                :: DT_OCEAN
    integer                                :: isc,iec,jsc,jec
    integer                                :: dt_cpld  = 86400
    integer                                :: year=0, month=0, day=0, hour=0, minute=0, second=0
    integer                                :: mpi_comm_mom

    type(ESMF_Grid)                        :: gridIn
    type(ESMF_Grid)                        :: gridOut

    integer                                :: npet, npet_x, npet_y

    rc = ESMF_SUCCESS

    allocate(Ice_ocean_boundary)
    !allocate(Ocean_state) ! ocean_model_init allocate this pointer
    allocate(Ocean_sfc)
    allocate(ocean_internalstate%ptr)
    ocean_internalstate%ptr%ice_ocean_boundary_type_ptr => Ice_ocean_boundary
    ocean_internalstate%ptr%ocean_public_type_ptr       => Ocean_sfc
    ocean_internalstate%ptr%ocean_state_type_ptr        => Ocean_state

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(VM, mpiCommunicator=mpi_comm_mom, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockGet(CLOCK, currTIME=MyTime, TimeStep=TINT,  RC=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeGet (MyTime,                    &
                       YY=YEAR, MM=MONTH, DD=DAY, &
                       H=HOUR,    M =MINUTE,    S =SECOND,  &
                                        RC=rc )
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    CALL ESMF_TimeIntervalGet(TINT, S=DT_OCEAN, RC=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fms_init(mpi_comm_mom)
    call constants_init
    call field_manager_init
    call diag_manager_init
    call set_calendar_type (JULIAN                )
    ! this ocean connector will be driven at set interval
    dt_cpld = DT_OCEAN
    DT = set_time (DT_OCEAN, 0)         
    Time = set_date (YEAR,MONTH,DAY,HOUR,MINUTE,SECOND)

    call ocean_model_init(Ocean_sfc, Ocean_state, Time, Time)
    call data_override_init(Ocean_domain_in = Ocean_sfc%domain)
    call mpp_get_compute_domain(Ocean_sfc%domain, isc, iec, jsc, jec)

    allocate ( Ice_ocean_boundary% u_flux (isc:iec,jsc:jec),          &
               Ice_ocean_boundary% v_flux (isc:iec,jsc:jec),          &
               Ice_ocean_boundary% t_flux (isc:iec,jsc:jec),          &
               Ice_ocean_boundary% q_flux (isc:iec,jsc:jec),          &
               Ice_ocean_boundary% salt_flux (isc:iec,jsc:jec),       &
               Ice_ocean_boundary% lw_flux (isc:iec,jsc:jec),         &
               Ice_ocean_boundary% sw_flux_vis_dir (isc:iec,jsc:jec), &
               Ice_ocean_boundary% sw_flux_vis_dif (isc:iec,jsc:jec), &
               Ice_ocean_boundary% sw_flux_nir_dir (isc:iec,jsc:jec), &
               Ice_ocean_boundary% sw_flux_nir_dif (isc:iec,jsc:jec), &
               Ice_ocean_boundary% lprec (isc:iec,jsc:jec),           &
               Ice_ocean_boundary% fprec (isc:iec,jsc:jec),           &
               Ice_ocean_boundary% runoff (isc:iec,jsc:jec),          &
               Ice_ocean_boundary% calving (isc:iec,jsc:jec),         &
               Ice_ocean_boundary% runoff_hflx (isc:iec,jsc:jec),     &
               Ice_ocean_boundary% calving_hflx (isc:iec,jsc:jec),    &
               Ice_ocean_boundary% mi (isc:iec,jsc:jec),              &
               Ice_ocean_boundary% p (isc:iec,jsc:jec))

    Ice_ocean_boundary%u_flux          = 0.0
    Ice_ocean_boundary%v_flux          = 0.0
    Ice_ocean_boundary%t_flux          = 0.0
    Ice_ocean_boundary%q_flux          = 0.0
    Ice_ocean_boundary%salt_flux       = 0.0
    Ice_ocean_boundary%lw_flux         = 0.0
    Ice_ocean_boundary%sw_flux_vis_dir = 0.0
    Ice_ocean_boundary%sw_flux_vis_dif = 0.0
    Ice_ocean_boundary%sw_flux_nir_dir = 0.0
    Ice_ocean_boundary%sw_flux_nir_dif = 0.0
    Ice_ocean_boundary%lprec           = 0.0
    Ice_ocean_boundary%fprec           = 0.0
    Ice_ocean_boundary%runoff          = 0.0
    Ice_ocean_boundary%calving         = 0.0
    Ice_ocean_boundary%runoff_hflx     = 0.0
    Ice_ocean_boundary%calving_hflx    = 0.0
    Ice_ocean_boundary%mi              = 0.0
    Ice_ocean_boundary%p               = 0.0

    call external_coupler_sbc_init(Ocean_sfc%domain, dt_cpld, Run_len)

    ocean_internalstate%ptr%ocean_state_type_ptr => Ocean_state
    call ESMF_GridCompSetInternalState(gcomp, ocean_internalstate, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !call ESMF_VMGet(vm, petCount=npet, rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

    ! create a Grid object for Fields
    ! we are going to create a single tile tripolar grid from a gridspec
    ! file. We also use the exact decomposition in MOM5 so that the Fields
    ! created can wrap on the data pointers in internal part of MOM5
    !gridIn = ESMF_GridCreate('grid_spec.nc', ESMF_FILEFORMAT_GRIDSPEC, &
    !    (/6, 4/), isSphere=.true., coordNames=(/'gridlon_t', 'gridlat_t'/), &
    !    addCornerStagger=.true., rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

    !gridIn = NUOPC_GridCreateSimpleXY( &
    !  0._ESMF_KIND_R8, 5.75_ESMF_KIND_R8, &
    !  -1.5_ESMF_KIND_R8, 2.0_ESMF_KIND_R8, &
    !  100, 100, rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

    !gridOut = gridIn ! for now out same as in

    call MOM5_AdvertiseImportFields(importState, gridIn, ice_ocean_boundary, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call MOM5_AdvertiseExportFields(exportState, gridOut, Ocean_sfc, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write(*,*) '----- MOM5 initialization phase 1 completed'

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Local Variables
    type(ESMF_VM)                          :: vm
    type(ESMF_Grid)                        :: gridIn
    type(ESMF_Grid)                        :: gridOut
    type (ocean_public_type),      pointer :: Ocean_sfc   => NULL()
    type (ocean_state_type),       pointer :: Ocean_state => NULL()
    type(ice_ocean_boundary_type), pointer :: Ice_ocean_boundary => NULL()
    type(ocean_internalstate_wrapper)      :: ocean_internalstate
    integer                                :: npet
    
    rc = ESMF_SUCCESS

    call ESMF_GridCompGetInternalState(gcomp, ocean_internalstate, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    Ice_ocean_boundary => ocean_internalstate%ptr%ice_ocean_boundary_type_ptr
    Ocean_sfc          => ocean_internalstate%ptr%ocean_public_type_ptr
    Ocean_state        => ocean_internalstate%ptr%ocean_state_type_ptr

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, petCount=npet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! We can check if npet is 24 or some other value to make sure
    ! MOM5 is configured to run on the correct number of processors.

    ! create a Grid object for Fields
    ! we are going to create a single tile tripolar grid from a gridspec
    ! file. We also use the exact decomposition in MOM5 so that the Fields
    ! created can wrap on the data pointers in internal part of MOM5
    gridIn = ESMF_GridCreate('grid_spec.nc', ESMF_FILEFORMAT_GRIDSPEC, &
        (/6, 4/), isSphere=.true., coordNames=(/'gridlon_t', 'gridlat_t'/), &
        addCornerStagger=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    gridOut = gridIn ! for now out same as in

    call MOM5_RealizeImportFields(importState, gridIn, ice_ocean_boundary, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call MOM5_RealizeExportFields(exportState, gridOut, Ocean_sfc, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write(*,*) '----- MOM5 initialization phase 2 completed'

  end subroutine
  
  !-----------------------------------------------------------------------------

  ! Ocean model uses same clock as parent gridComp
  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep, timestep

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeIntervalSet(timestep, m=60, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockSet(clock, timestep=timestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=60, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_GridCompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)                    :: gcomp
    integer, intent(out)                   :: rc
    
    ! local variables
    type(ESMF_Clock)                       :: clock
    type(ESMF_State)                       :: importState, exportState
    type(ESMF_Time)                        :: currTime
    type(ESMF_TimeInterval)                :: timeStep

    type (ocean_public_type),      pointer :: Ocean_sfc          => NULL()
    type (ocean_state_type),       pointer :: Ocean_state        => NULL()
    type(ice_ocean_boundary_type), pointer :: Ice_ocean_boundary => NULL()
    type(ocean_internalstate_wrapper)      :: ocean_internalstate

    ! define some time types 
    type(time_type)                        :: Time        
    type(time_type)                        :: Time_step_coupled

    integer :: dth, dtm, dts, dt_cpld  = 86400
    integer :: nc


    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridCompGetInternalState(gcomp, ocean_internalstate, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    Ice_ocean_boundary => ocean_internalstate%ptr%ice_ocean_boundary_type_ptr
    Ocean_sfc          => ocean_internalstate%ptr%ocean_public_type_ptr
    Ocean_state        => ocean_internalstate%ptr%ocean_state_type_ptr

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    !call NUOPC_ClockPrintCurrTime(clock, &
    !  "------>Advancing OCN from: ", rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    !call NUOPC_TimePrint(currTime + timeStep, &
    !  "--------------------------------> to: ", rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

    call ESMF_TimeIntervalGet(timeStep, h=dth, m=dtm, s=dts, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    Time = esmf2fms_time(currTime)
    Time_step_coupled = esmf2fms_time(timeStep)
    dt_cpld = dth*3600+dtm*60+dts

    call ice_ocn_bnd_from_data(Ice_ocean_boundary, Time, Time_step_coupled)

    call external_coupler_sbc_before(Ice_ocean_boundary, Ocean_sfc, nc, dt_cpld )

    call writeSliceFields(importState, 'field_ocn_import_', import_slice, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    import_slice = import_slice + 1

    call update_ocean_model(Ice_ocean_boundary, Ocean_state, Ocean_sfc, Time, Time_step_coupled)

    call writeSliceFields(exportState, 'field_ocn_export_', export_slice, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    export_slice = export_slice + 1

    call external_coupler_sbc_after(Ice_ocean_boundary, Ocean_sfc, nc, dt_cpld )

    !write(*,*) 'MOM: --- run phase called ---'

  end subroutine 

  subroutine ocean_model_finalize(gcomp, importState, exportState, clock, rc)

    ! input arguments
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type (ocean_public_type),      pointer :: Ocean_sfc          
    type (ocean_state_type),       pointer :: Ocean_state
    type(ocean_internalstate_wrapper)      :: ocean_internalstate
    type(TIME_TYPE)                        :: Time        
    type(ESMF_Time)                        :: currTime
    character(len=64)                      :: timestamp

    rc = ESMF_SUCCESS

    call ESMF_GridCompGetInternalState(gcomp, ocean_internalstate, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    Ocean_sfc          => ocean_internalstate%ptr%ocean_public_type_ptr
    Ocean_state        => ocean_internalstate%ptr%ocean_state_type_ptr

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    Time = esmf2fms_time(currTime)

    call ocean_model_end (Ocean_sfc, Ocean_State, Time)
    call diag_manager_end(Time )
    call field_manager_end

    call fms_io_exit
    call fms_end

    write(*,*) 'MOM: --- completed ---'

  end subroutine

!====================================================================
! get forcing data from data_overide 
  subroutine ice_ocn_bnd_from_data(x, Time, Time_step_coupled)

      type (ice_ocean_boundary_type) :: x
      type(Time_type), intent(in)    :: Time, Time_step_coupled

      type(Time_type)                :: Time_next

      Time_next = Time + Time_step_coupled

      !call data_override('OCN', 't_flux',          x%t_flux         , Time_next)
      !call data_override('OCN', 'u_flux',          x%u_flux         , Time_next)
      !call data_override('OCN', 'v_flux',          x%v_flux         , Time_next)
      !call data_override('OCN', 'q_flux',          x%q_flux         , Time_next)
      !call data_override('OCN', 'salt_flux',       x%salt_flux      , Time_next)
      !call data_override('OCN', 'lw_flux',         x%lw_flux        , Time_next)
      !call data_override('OCN', 'sw_flux_vis_dir', x%sw_flux_vis_dir, Time_next)
      !call data_override('OCN', 'sw_flux_vis_dif', x%sw_flux_vis_dif, Time_next)
      !call data_override('OCN', 'sw_flux_nir_dir', x%sw_flux_nir_dir, Time_next)
      !call data_override('OCN', 'sw_flux_nir_dif', x%sw_flux_nir_dif, Time_next)
      !call data_override('OCN', 'lprec',           x%lprec          , Time_next)
      !call data_override('OCN', 'fprec',           x%fprec          , Time_next)
      !call data_override('OCN', 'runoff',          x%runoff         , Time_next)
      !call data_override('OCN', 'calving',         x%calving        , Time_next)
      !call data_override('OCN', 'p',               x%p              , Time_next)
            
  end subroutine ice_ocn_bnd_from_data


!-----------------------------------------------------------------------------------------
! 
! Subroutines  for enabling coupling to external programs through a third party coupler
! such as OASIS/PRISM.
! If no external coupler then these will mostly be dummy routines.
! These routines can also serve as spots to call other user defined routines
!-----------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------

! Dummy subroutines.

  subroutine external_coupler_mpi_init(mom_local_communicator, external_initialization)
  implicit none
  integer, intent(out) :: mom_local_communicator
  logical, intent(out) :: external_initialization
  external_initialization = .false.
  mom_local_communicator = -100         ! Is there mpp_undefined parameter corresponding to MPI_UNDEFINED?
                                        ! probably wouldn't need logical flag.
  return
  end subroutine external_coupler_mpi_init

!-----------------------------------------------------------------------------------------
  subroutine external_coupler_sbc_init(Dom, dt_cpld, Run_len)
  implicit none
  type(domain2d) :: Dom
  integer :: dt_cpld
  type(time_type) :: Run_len
  return
  end  subroutine external_coupler_sbc_init

  subroutine external_coupler_sbc_before(Ice_ocean_boundary, Ocean_sfc, nsteps, dt_cpld )
  implicit none
  type (ice_ocean_boundary_type), intent(INOUT) :: Ice_ocean_boundary
  type (ocean_public_type) , intent(INOUT)        :: Ocean_sfc
  integer , intent(IN)                       :: nsteps, dt_cpld
  return
  end subroutine external_coupler_sbc_before


  subroutine external_coupler_sbc_after(Ice_ocean_boundary, Ocean_sfc, nsteps, dt_cpld )
  type (ice_ocean_boundary_type) :: Ice_ocean_boundary
  type (ocean_public_type)         :: Ocean_sfc
  integer                        :: nsteps, dt_cpld
  return
  end subroutine external_coupler_sbc_after

  subroutine external_coupler_restart( dt_cpld, num_cpld_calls )
  implicit none
  integer, intent(in)               :: dt_cpld, num_cpld_calls
  return
  end subroutine external_coupler_restart

  subroutine external_coupler_exit
  return
  end subroutine external_coupler_exit

!-----------------------------------------------------------------------------------------
  subroutine external_coupler_mpi_exit(mom_local_communicator, external_initialization)
  implicit none
  integer, intent(in) :: mom_local_communicator
  logical, intent(in) :: external_initialization
  return
  end subroutine external_coupler_mpi_exit
!-----------------------------------------------------------------------------------------
    subroutine writeSliceFields(state, filename_prefix, slice, rc)
      type(ESMF_State)                :: state
      character(len=*)                :: filename_prefix
      integer                         :: slice
      integer, intent(out), optional  :: rc

      integer                         :: n, nfields
      type(ESMF_Field)                :: field
      type(ESMF_StateItem_Flag)       :: itemType
      character(len=40)               :: fileName
      character(len=64),allocatable   :: fieldNameList(:)
      character(len=*),parameter :: subname='(mom_cap:writeSliceFields)'

      if (present(rc)) rc = ESMF_SUCCESS
      
      if (ESMF_IO_PIO_PRESENT .and. &
        (ESMF_IO_NETCDF_PRESENT .or. ESMF_IO_PNETCDF_PRESENT)) then

        call ESMF_StateGet(state, itemCount=nfields, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        allocate(fieldNameList(nfields))
        call ESMF_StateGet(state, itemNameList=fieldNameList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        do n=1, size(fieldNameList)
          call ESMF_StateGet(state, itemName=fieldNameList(n), &
            itemType=itemType, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (itemType /= ESMF_STATEITEM_NOTFOUND) then
            ! field is available in the state
            call ESMF_StateGet(state, itemName=fieldNameList(n), field=field, &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            ! -> output to file
            write (fileName,"(A)") &
              filename_prefix//trim(fieldNameList(n))//".nc"
            call ESMF_FieldWrite(field, file=trim(fileName), &
              timeslice=slice, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              call ESMF_Finalize(endflag=ESMF_END_ABORT)
          endif
        enddo

        deallocate(fieldNameList)

      endif


    end subroutine writeSliceFields

  subroutine MOM5_AdvertiseImportFields(importState, gridIn, Ice_ocean_boundary, rc)

    type(ESMF_State), intent(inout)             :: importState
    type(ESMF_Grid), intent(in)                 :: gridIn
    type(ice_ocean_boundary_type), intent(in)   :: Ice_ocean_boundary
    integer, intent(inout)                      :: rc

    type(ESMF_Field)                            :: field
    
    rc = ESMF_SUCCESS


    call NUOPC_StateAdvertiseField(importState, standardName='mean_zonal_moment_flx', &
      longname='i-directed wind stress into ocean', &
      shortname='u_flux', &
      name='u_flux', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_merid_moment_flx', &
      longname='j-directed wind stress into ocean', &
      shortname='v_flux', &
      name='v_flux', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_sensi_heat_flx', &
      longname='sensible heat flux into the ocean', &
      shortname='t_flux', &
      name='t_flux', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_laten_heat_flx', &
      longname='specific humidity flux', &
      shortname='q_flux', &
      name='q_flux', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_salt_flx', &
      longname='salt flux into ocean', &
      shortname='salt_flux', &
      name='salt_flux', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_down_lw_flx', &
      longname='longwave radiation', &
      shortname='lw_flux', &
      name='lw_flux', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_down_sw_vis_dir_flx', &
      longname='direct visible sw radiation', &
      shortname='sw_flux_vis_dir', &
      name='sw_flux_vis_dir', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_down_sw_vis_dif_flx', &
      longname='diffuse visible sw radiation', &
      shortname='sw_flux_vis_dif', &
      name='sw_flux_vis_dif', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_down_sw_ir_dir_flx', &
      longname='direct near IR sw radiation', &
      shortname='sw_flux_nir_dir', &
      name='sw_flux_nir_dir', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_down_sw_ir_dif_flx', &
      longname='direct near IR sw radiation', &
      shortname='sw_flux_nir_dif', &
      name='sw_flux_nir_dif', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_prec_rate', &
      longname='mass flux of liquid precip', &
      shortname='lprec', &
      name='lprec', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_fprec_rate', &
      longname='mass flux of frozen precip', &
      shortname='fprec', &
      name='fprec', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_runoff_rate', &
      longname='mass flux of liquid runoff', &
      shortname='runoff', &
      name='runoff', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_calving_rate', &
      longname='mass flux of frozen runoff', &
      shortname='calving', &
      name='calving', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_runoff_flx', &
      longname='heat flux, relative to 0C, of liquid land water into ocean', &
      shortname='runoff_hflx', &
      name='runoff_hflx', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mean_calving_flx', &
      longname='heat flux, relative to 0C, of frozen land water into ocean', &
      shortname='calving_hflx', &
      name='calving_hflx', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='inst_pres_height_surface', &
      longname='pressure of overlying sea ice and atmosphere', &
      shortname='p', &
      name='p', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(importState, standardName='mi', &
      longname='mass of overlying sea ice (optional)', &
      shortname='mi', &
      name='mi', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


  end subroutine MOM5_AdvertiseImportFields
  
  subroutine MOM5_RealizeImportFields(importState, gridIn, Ice_ocean_boundary, rc)

    type(ESMF_State), intent(inout)             :: importState
    type(ESMF_Grid), intent(in)                 :: gridIn
    type(ice_ocean_boundary_type), intent(in)   :: Ice_ocean_boundary
    integer, intent(inout)                      :: rc

    type(ESMF_Field)                            :: field
    
    rc = ESMF_SUCCESS

    ! importable field: i-directed wind stress into ocean
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_zonal_moment_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%u_flux, &
      name="u_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="u_flux")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_zonal_moment_flx/u_flux is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_zonal_moment_flx/u_flux is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%u_flux = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"u_flux"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: j-directed wind stress into ocean
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_merid_moment_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%v_flux, &
      name="v_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="v_flux")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_merid_moment_flx/v_flux is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_merid_moment_flx/v_flux is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%v_flux = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"v_flux"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: sensible heat flux into the ocean
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_sensi_heat_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%t_flux, &
      name="t_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="t_flux")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_sensi_heat_flx/t_flux is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_sensi_heat_flx/t_flux is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%t_flux = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"t_flux"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: specific humidity flux
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_laten_heat_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%q_flux, &
      name="q_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="q_flux")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_laten_heat_flx/q_flux is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_laten_heat_flx/q_flux is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%q_flux = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"q_flux"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: salt flux into ocean
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_salt_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%salt_flux, &
      name="salt_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="salt_flux")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_salt_flx/salt_flux is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_salt_flx/salt_flux is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%salt_flux = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"salt_flux"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: longwave radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_down_lw_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%lw_flux, &
      name="lw_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="lw_flux")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_down_lw_flx/lw_flux is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_down_lw_flx/lw_flux is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%lw_flux = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"lw_flux"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: direct visible sw radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_down_sw_vis_dir_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%sw_flux_vis_dir, &
      name="sw_flux_vis_dir", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="sw_flux_vis_dir")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_down_sw_vis_dir_flx/sw_flux_vis_dir is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_down_sw_vis_dir_flx/sw_flux_vis_dir is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%sw_flux_vis_dir = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"sw_flux_vis_dir"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: diffuse visible sw radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_down_sw_vis_dif_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%sw_flux_vis_dif, &
      name="sw_flux_vis_dif", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="sw_flux_vis_dif")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_down_sw_vis_dif_flx/sw_flux_vis_dif is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_down_sw_vis_dif_flx/sw_flux_vis_dif is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%sw_flux_vis_dif = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"sw_flux_vis_dif"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: direct near IR sw radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_down_sw_ir_dir_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%sw_flux_nir_dir, &
      name="sw_flux_nir_dir", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="sw_flux_nir_dir")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_down_sw_ir_dir_flx/sw_flux_nir_dir is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_down_sw_ir_dir_flx/sw_flux_nir_dir is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%sw_flux_nir_dir = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"sw_flux_nir_dir"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: direct near IR sw radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_down_sw_ir_dif_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%sw_flux_nir_dif, &
      name="sw_flux_nir_dif", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="sw_flux_nir_dif")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_down_sw_ir_dif_flx/sw_flux_nir_dif is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_down_sw_ir_dif_flx/sw_flux_nir_dif is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%sw_flux_nir_dif = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"sw_flux_nir_dif"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: mass flux of liquid precip
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_prec_rate
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%lprec, &
      name="lprec", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="lprec")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_prec_rate/lprec is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_prec_rate/lprec is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%lprec = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"lprec"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: mass flux of frozen precip
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_fprec_rate
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%fprec, &
      name="fprec", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="fprec")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_fprec_rate/fprec is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_fprec_rate/fprec is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%fprec = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"fprec"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: mass flux of liquid runoff
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_runoff_rate
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%runoff, &
      name="runoff", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="runoff")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_runoff_rate/runoff is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_runoff_rate/runoff is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%runoff = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"runoff"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: mass flux of frozen runoff
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_calving_rate
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%calving, &
      name="calving", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="calving")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_calving_rate/calving is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_calving_rate/calving is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%calving = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"calving"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: heat flux, relative to 0C, of liquid land water into ocean
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_runoff_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%runoff_hflx, &
      name="runoff_hflx", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="runoff_hflx")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_runoff_flx/runoff_hflx is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_runoff_flx/runoff_hflx is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%runoff_hflx = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"runoff_hflx"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: heat flux, relative to 0C, of frozen land water into ocean
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_calving_flx
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%calving_hflx, &
      name="calving_hflx", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="calving_hflx")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mean_calving_flx/calving_hflx is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mean_calving_flx/calving_hflx is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%calving_hflx = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"calving_hflx"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: pressure of overlying sea ice and atmosphere
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: inst_pres_height_surface
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%p, &
      name="p", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="p")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field inst_pres_height_surface/p is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field inst_pres_height_surface/p is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%p = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"p"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! importable field: mass of overlying sea ice (optional)
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mi
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%mi, &
      name="mi", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(importState, fieldName="mi")) then
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("import Field mi/mi is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("import Field mi/mi is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ice_ocean_boundary%mi = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"mi"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

  end subroutine MOM5_RealizeImportFields
  
  subroutine MOM5_BuildImportFieldDictionary(rc)

    integer, intent(inout)                      :: rc

    rc = ESMF_SUCCESS


    ! importable field: i-directed wind stress into ocean
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_zonal_moment_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_zonal_moment_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_zonal_moment_flx', &
        canonicalUnits='Pa', &
        defaultLongName='i-directed wind stress into ocean', &
        defaultShortName='u_flux', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: j-directed wind stress into ocean
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_merid_moment_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_merid_moment_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_merid_moment_flx', &
        canonicalUnits='Pa', &
        defaultLongName='j-directed wind stress into ocean', &
        defaultShortName='v_flux', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: sensible heat flux into the ocean
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_sensi_heat_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_sensi_heat_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_sensi_heat_flx', &
        canonicalUnits='W/m^2', &
        defaultLongName='sensible heat flux into the ocean', &
        defaultShortName='t_flux', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: specific humidity flux
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_laten_heat_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_laten_heat_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_laten_heat_flx', &
        canonicalUnits='kg/m^2/s', &
        defaultLongName='specific humidity flux', &
        defaultShortName='q_flux', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: salt flux into ocean
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_salt_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_salt_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_salt_flx', &
        canonicalUnits='kg/m^2/s', &
        defaultLongName='salt flux into ocean', &
        defaultShortName='salt_flux', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: longwave radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_down_lw_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_down_lw_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_down_lw_flx', &
        canonicalUnits='W/m^2', &
        defaultLongName='longwave radiation', &
        defaultShortName='lw_flux', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: direct visible sw radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_down_sw_vis_dir_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_down_sw_vis_dir_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_down_sw_vis_dir_flx', &
        canonicalUnits='W/m^2', &
        defaultLongName='direct visible sw radiation', &
        defaultShortName='sw_flux_vis_dir', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: diffuse visible sw radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_down_sw_vis_dif_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_down_sw_vis_dif_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_down_sw_vis_dif_flx', &
        canonicalUnits='W/m^2', &
        defaultLongName='diffuse visible sw radiation', &
        defaultShortName='sw_flux_vis_dif', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: direct near IR sw radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_down_sw_ir_dir_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_down_sw_ir_dir_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_down_sw_ir_dir_flx', &
        canonicalUnits='W/m^2', &
        defaultLongName='direct near IR sw radiation', &
        defaultShortName='sw_flux_nir_dir', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: direct near IR sw radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_down_sw_ir_dif_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_down_sw_ir_dif_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_down_sw_ir_dif_flx', &
        canonicalUnits='W/m^2', &
        defaultLongName='direct near IR sw radiation', &
        defaultShortName='sw_flux_nir_dif', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: mass flux of liquid precip
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: mean_prec_rate
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_prec_rate', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_prec_rate', &
        canonicalUnits='kg/m^2/s', &
        defaultLongName='mass flux of liquid precip', &
        defaultShortName='lprec', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: mass flux of frozen precip
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_fprec_rate
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_fprec_rate', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_fprec_rate', &
        canonicalUnits='kg/m^2/s', &
        defaultLongName='mass flux of frozen precip', &
        defaultShortName='fprec', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: mass flux of liquid runoff
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_runoff_rate
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_runoff_rate', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_runoff_rate', &
        canonicalUnits='kg/m^2/s', &
        defaultLongName='mass flux of liquid runoff', &
        defaultShortName='runoff', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: mass flux of frozen runoff
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_calving_rate
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_calving_rate', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_calving_rate', &
        canonicalUnits='kg/m^2/s', &
        defaultLongName='mass flux of frozen runoff', &
        defaultShortName='calving', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: heat flux, relative to 0C, of liquid land water into ocean
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_runoff_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_runoff_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_runoff_flx', &
        canonicalUnits='W/m^2', &
        defaultLongName='heat flux, relative to 0C, of liquid land water into ocean', &
        defaultShortName='runoff_hflx', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: heat flux, relative to 0C, of frozen land water into ocean
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mean_calving_flx
    if(.not. NUOPC_FieldDictionaryHasEntry('mean_calving_flx', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mean_calving_flx', &
        canonicalUnits='W/m^2', &
        defaultLongName='heat flux, relative to 0C, of frozen land water into ocean', &
        defaultShortName='calving_hflx', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: pressure of overlying sea ice and atmosphere
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: inst_pres_height_surface
    if(.not. NUOPC_FieldDictionaryHasEntry('inst_pres_height_surface', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='inst_pres_height_surface', &
        canonicalUnits='Pa', &
        defaultLongName='pressure of overlying sea ice and atmosphere', &
        defaultShortName='p', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! importable field: mass of overlying sea ice (optional)
    ! Available from GSM atmosphere model: NO
    ! Corresponding GSM atmosphere output field name: mi
    if(.not. NUOPC_FieldDictionaryHasEntry('mi', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='mi', &
        canonicalUnits='kg', &
        defaultLongName='mass of overlying sea ice (optional)', &
        defaultShortName='mi', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

  end subroutine MOM5_BuildImportFieldDictionary
  
  subroutine MOM5_AdvertiseExportFields(exportState, gridOut, Ocean_sfc, rc)

    type(ESMF_State), intent(inout)             :: exportState
    type(ESMF_Grid), intent(in)                 :: gridOut
    type(ocean_public_type), intent(in)         :: Ocean_sfc
    integer, intent(inout)                      :: rc

    type(ESMF_Field)                            :: field
    
    rc = ESMF_SUCCESS


    call NUOPC_StateAdvertiseField(exportState, standardName='sea_surface_temperature', &
      longname='sea surface temperature on t-cell', &
      shortname='t_surf', &
      name='t_surf', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(exportState, standardName='s_surf', &
      longname='sea surface salinity on t-cell', &
      shortname='s_surf', &
      name='s_surf', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(exportState, standardName='u_surf', &
      longname='i-directed surface ocean velocity on u-cell', &
      shortname='u_surf', &
      name='u_surf', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(exportState, standardName='v_surf', &
      longname='j-directed surface ocean velocity on u-cell', &
      shortname='v_surf', &
      name='v_surf', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out



    call NUOPC_StateAdvertiseField(exportState, standardName='sea_lev', &
      longname='sea level', &
      shortname='sea_lev', &
      name='sea_lev', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


  end subroutine MOM5_AdvertiseExportFields
  
  subroutine MOM5_RealizeExportFields(exportState, gridOut, Ocean_sfc, rc)

    type(ESMF_State), intent(inout)             :: exportState
    type(ESMF_Grid), intent(in)                 :: gridOut
    type(ocean_public_type), intent(in)         :: Ocean_sfc
    integer, intent(inout)                      :: rc

    type(ESMF_Field)                            :: field
    
    rc = ESMF_SUCCESS

    ! exportable field: sea surface temperature on t-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: sea_surface_temperature
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridOut, &
      farrayPtr=Ocean_sfc%t_surf, &
      name="t_surf", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(exportState, fieldName="t_surf")) then
      call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("export Field sea_surface_temperature/t_surf is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("export Field sea_surface_temperature/t_surf is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ocean_sfc%t_surf = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(exportState, (/"t_surf"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! exportable field: sea surface salinity on t-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: s_surf
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridOut, &
      farrayPtr=Ocean_sfc%s_surf, &
      name="s_surf", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(exportState, fieldName="s_surf")) then
      call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("export Field s_surf/s_surf is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("export Field s_surf/s_surf is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ocean_sfc%s_surf = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(exportState, (/"s_surf"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! exportable field: i-directed surface ocean velocity on u-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: u_surf
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridOut, &
      farrayPtr=Ocean_sfc%u_surf, &
      name="u_surf", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(exportState, fieldName="u_surf")) then
      call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("export Field u_surf/u_surf is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("export Field u_surf/u_surf is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ocean_sfc%u_surf = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(exportState, (/"u_surf"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! exportable field: j-directed surface ocean velocity on u-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: v_surf
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridOut, &
      farrayPtr=Ocean_sfc%v_surf, &
      name="v_surf", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(exportState, fieldName="v_surf")) then
      call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("export Field v_surf/v_surf is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("export Field v_surf/v_surf is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ocean_sfc%v_surf = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(exportState, (/"v_surf"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! exportable field: sea level
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: sea_lev
    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridOut, &
      farrayPtr=Ocean_sfc%sea_lev, &
      name="sea_lev", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (NUOPC_StateIsFieldConnected(exportState, fieldName="sea_lev")) then
      call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("export Field sea_lev/sea_lev is connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_LogWrite("export Field sea_lev/sea_lev is not connected.", &
        ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Initialize the value in the pointer to 0
      ! The values will stay 0. non-connected Fields for GSM->MOM5 coupling.
      Ocean_sfc%sea_lev = 0.0
      ! remove a not connected Field from State
      call ESMF_StateRemove(exportState, (/"sea_lev"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

  end subroutine MOM5_RealizeExportFields
  
  subroutine MOM5_BuildExportFieldDictionary(rc)

    integer, intent(inout)                      :: rc

    rc = ESMF_SUCCESS


    ! exportable field: sea surface temperature on t-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: sea_surface_temperature
    if(.not. NUOPC_FieldDictionaryHasEntry('sea_surface_temperature', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='sea_surface_temperature', &
        canonicalUnits='K', &
        defaultLongName='sea surface temperature on t-cell', &
        defaultShortName='t_surf', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! exportable field: sea surface salinity on t-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: s_surf
    if(.not. NUOPC_FieldDictionaryHasEntry('s_surf', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='s_surf', &
        canonicalUnits='psu', &
        defaultLongName='sea surface salinity on t-cell', &
        defaultShortName='s_surf', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! exportable field: i-directed surface ocean velocity on u-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: u_surf
    if(.not. NUOPC_FieldDictionaryHasEntry('u_surf', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='u_surf', &
        canonicalUnits='m/s', &
        defaultLongName='i-directed surface ocean velocity on u-cell', &
        defaultShortName='u_surf', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! exportable field: j-directed surface ocean velocity on u-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: v_surf
    if(.not. NUOPC_FieldDictionaryHasEntry('v_surf', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='v_surf', &
        canonicalUnits='m/s', &
        defaultLongName='j-directed surface ocean velocity on u-cell', &
        defaultShortName='v_surf', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif


    ! exportable field: sea level
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: sea_lev
    if(.not. NUOPC_FieldDictionaryHasEntry('sea_lev', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='sea_lev', &
        canonicalUnits='m', &
        defaultLongName='sea level', &
        defaultShortName='sea_lev', &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

  end subroutine MOM5_BuildExportFieldDictionary
  
end module mom_cap_mod
