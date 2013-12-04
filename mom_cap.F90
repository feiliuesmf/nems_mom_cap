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
    !call ESMF_MethodAdd(gcomp, label=model_label_SetClock, &
    !  userRoutine=SetClock, rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out
    
    call ESMF_MethodAdd(gcomp, label=model_label_Advance, &
      userRoutine=ModelAdvance, rc=rc)
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
    Ice_ocean_boundary%p               = 0.0

    call external_coupler_sbc_init(Ocean_sfc%domain, dt_cpld, Run_len)

    ocean_internalstate%ptr%ocean_state_type_ptr => Ocean_state
    call ESMF_GridCompSetInternalState(gcomp, ocean_internalstate, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, petCount=npet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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

    !gridIn = NUOPC_GridCreateSimpleXY( &
    !  0._ESMF_KIND_R8, 5.75_ESMF_KIND_R8, &
    !  -1.5_ESMF_KIND_R8, 2.0_ESMF_KIND_R8, &
    !  100, 100, rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

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

    write(*,*) '----- OCN initialization phase 1 completed'

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    write(*,*) '----- OCN initialization phase 2 completed'

  end subroutine
  
  !-----------------------------------------------------------------------------

  ! Ocean solo model uses same clock as parent gridComp
  !subroutine SetClock(gcomp, rc)
  !  type(ESMF_GridComp)  :: gcomp
  !  integer, intent(out) :: rc
  !  
  !  ! local variables
  !  type(ESMF_Clock)              :: clock
  !  type(ESMF_TimeInterval)       :: stabilityTimeStep

  !  rc = ESMF_SUCCESS
  !  
  !  ! query the Component for its clock, importState and exportState
  !  call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
  !  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !    line=__LINE__, &
  !    file=__FILE__)) &
  !    return  ! bail out
  !    
  !  ! initialize internal clock
  !  ! here: parent Clock and stability timeStep determine actual model timeStep
  !  !TODO: stabilityTimeStep should be read in from configuation
  !  !TODO: or computed from internal Grid information
  !  call ESMF_TimeIntervalSet(stabilityTimeStep, m=60, rc=rc) 
  !  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !    line=__LINE__, &
  !    file=__FILE__)) &
  !    return  ! bail out
  !  call NUOPC_GridCompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
  !  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !    line=__LINE__, &
  !    file=__FILE__)) &
  !    return  ! bail out

  !  
  !end subroutine

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
    
    call NUOPC_ClockPrintCurrTime(clock, &
      "------>Advancing OCN from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_TimePrint(currTime + timeStep, &
      "--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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

    call update_ocean_model(Ice_ocean_boundary, Ocean_state, Ocean_sfc, Time, Time_step_coupled)

    call external_coupler_sbc_after(Ice_ocean_boundary, Ocean_sfc, nc, dt_cpld )

    write(*,*) 'MOM: --- run phase called ---'

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
    type(ESMF_VM)                          :: vm
    integer                                :: mpi_comm_mom, dt_cpld, num_cpld_calls
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

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, mpiCommunicator=mpi_comm_mom, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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

      call data_override('OCN', 't_flux',          x%t_flux         , Time_next)
      call data_override('OCN', 'u_flux',          x%u_flux         , Time_next)
      call data_override('OCN', 'v_flux',          x%v_flux         , Time_next)
      call data_override('OCN', 'q_flux',          x%q_flux         , Time_next)
      call data_override('OCN', 'salt_flux',       x%salt_flux      , Time_next)
      call data_override('OCN', 'lw_flux',         x%lw_flux        , Time_next)
      call data_override('OCN', 'sw_flux_vis_dir', x%sw_flux_vis_dir, Time_next)
      call data_override('OCN', 'sw_flux_vis_dif', x%sw_flux_vis_dif, Time_next)
      call data_override('OCN', 'sw_flux_nir_dir', x%sw_flux_nir_dir, Time_next)
      call data_override('OCN', 'sw_flux_nir_dif', x%sw_flux_nir_dif, Time_next)
      call data_override('OCN', 'lprec',           x%lprec          , Time_next)
      call data_override('OCN', 'fprec',           x%fprec          , Time_next)
      call data_override('OCN', 'runoff',          x%runoff         , Time_next)
      call data_override('OCN', 'calving',         x%calving        , Time_next)
      call data_override('OCN', 'p',               x%p              , Time_next)
            
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
  subroutine MOM5_RealizeImportFields(importState, gridIn, Ice_ocean_boundary, rc)

    type(ESMF_State), intent(inout)             :: importState
    type(ESMF_Grid), intent(in)                 :: gridIn
    type(ice_ocean_boundary_type), intent(in)   :: Ice_ocean_boundary
    integer, intent(inout)                      :: rc

    type(ESMF_Field)                            :: field
    
    rc = ESMF_SUCCESS

    ! importable field: i-directed wind stress into ocean
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: 
    if(.not. NUOPC_FieldDictionaryHasEntry('u_flux', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='u_flux', &
        canonicalUnits='Pa', &
        defaultLongName='i-directed wind stress into ocean', &
        defaultShortName='u_flux', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%u_flux, &
      name="u_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='u_flux', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: j-directed wind stress into ocean
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: 
    if(.not. NUOPC_FieldDictionaryHasEntry('v_flux', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='v_flux', &
        canonicalUnits='Pa', &
        defaultLongName='j-directed wind stress into ocean', &
        defaultShortName='v_flux', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%v_flux, &
      name="v_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='v_flux', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: sensible heat flux into the ocean
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: sensible heat flux
    if(.not. NUOPC_FieldDictionaryHasEntry('t_flux', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='t_flux', &
        canonicalUnits='W/m^2', &
        defaultLongName='sensible heat flux into the ocean', &
        defaultShortName='t_flux', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%t_flux, &
      name="t_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='t_flux', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: longwave radiation
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: longwave_radiative_flux_into_ocean
    if(.not. NUOPC_FieldDictionaryHasEntry('lw_flux', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='lw_flux', &
        canonicalUnits='W/m^2', &
        defaultLongName='longwave radiation', &
        defaultShortName='lw_flux', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%lw_flux, &
      name="lw_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='lw_flux', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: mass flux of liquid precip
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: 
    if(.not. NUOPC_FieldDictionaryHasEntry('lprec', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='lprec', &
        canonicalUnits='kg/m^2/s', &
        defaultLongName='mass flux of liquid precip', &
        defaultShortName='lprec', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%lprec, &
      name="lprec", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='lprec', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: pressure of overlying sea ice and atmosphere
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: 
    if(.not. NUOPC_FieldDictionaryHasEntry('p', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='p', &
        canonicalUnits='Pa', &
        defaultLongName='pressure of overlying sea ice and atmosphere', &
        defaultShortName='p', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridIn, &
      farrayPtr=Ice_ocean_boundary%p, &
      name="p", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, standardName='p', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine MOM5_RealizeImportFields
!-----------------------------------------------------------------------------------------
  subroutine MOM5_RealizeExportFields(exportState, gridOut, Ocean_sfc, rc)

    type(ESMF_State), intent(inout)             :: exportState
    type(ESMF_Grid), intent(in)                 :: gridOut
    type(ocean_public_type), intent(in)         :: Ocean_sfc
    integer, intent(inout)                      :: rc

    type(ESMF_Field)                            :: field
    
    rc = ESMF_SUCCESS

    ! exportable field: sea surface temperature on t-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: 
    if(.not. NUOPC_FieldDictionaryHasEntry('t_surf', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='t_surf', &
        canonicalUnits='K', &
        defaultLongName='sea surface temperature on t-cell', &
        defaultShortName='t_surf', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridOut, &
      farrayPtr=Ocean_sfc%t_surf, &
      name="t_surf", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='t_surf', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea surface salinity on t-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: 
    if(.not. NUOPC_FieldDictionaryHasEntry('s_surf', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='s_surf', &
        canonicalUnits='psu', &
        defaultLongName='sea surface salinity on t-cell', &
        defaultShortName='s_surf', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridOut, &
      farrayPtr=Ocean_sfc%s_surf, &
      name="s_surf", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='s_surf', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: i-directed surface ocean velocity on u-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: 
    if(.not. NUOPC_FieldDictionaryHasEntry('u_surf', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='u_surf', &
        canonicalUnits='m/s', &
        defaultLongName='i-directed surface ocean velocity on u-cell', &
        defaultShortName='u_surf', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridOut, &
      farrayPtr=Ocean_sfc%u_surf, &
      name="u_surf", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='u_surf', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: j-directed surface ocean velocity on u-cell
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: 
    if(.not. NUOPC_FieldDictionaryHasEntry('v_surf', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='v_surf', &
        canonicalUnits='m/s', &
        defaultLongName='j-directed surface ocean velocity on u-cell', &
        defaultShortName='v_surf', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridOut, &
      farrayPtr=Ocean_sfc%v_surf, &
      name="v_surf", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='v_surf', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea level
    ! Available from GSM atmosphere model: YES
    ! Corresponding GSM atmosphere output field name: 
    if(.not. NUOPC_FieldDictionaryHasEntry('sea_lev', rc=rc)) then
      call NUOPC_FieldDictionaryAddEntry(standardName='sea_lev', &
        canonicalUnits='m', &
        defaultLongName='sea level', &
        defaultShortName='sea_lev', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! Wraps around the internal MOM5 Fortran array pointer
    field = ESMF_FieldCreate(grid=gridOut, &
      farrayPtr=Ocean_sfc%sea_lev, &
      name="sea_lev", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, standardName='sea_lev', &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine MOM5_RealizeExportFields

end module mom_cap_mod
