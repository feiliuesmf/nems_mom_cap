!>
!! @mainpage MOM NUOPC Cap
!! @author Fei Liu (fei.liu@gmail.com)
!! @date 5/10/13 Original documentation
!! 
!! 
!! @section Repository
!! The MOM NUOPC cap is maintained in a GitHub repository:
!! https://github.com/feiliuesmf/nems_mom_cap
!!
!! @section References 
!! 
!! - [MOM Home Page] (http://mom-ocean.org/web)
!!
!!
module mom_solo_cap_mod

  use MOM_cpu_clock,       only : cpu_clock_id, cpu_clock_begin, cpu_clock_end
  use MOM_cpu_clock,       only : CLOCK_COMPONENT
  use MOM_diag_mediator,   only : enable_averaging, disable_averaging, diag_mediator_end
  use MOM_diag_mediator,   only : diag_mediator_close_registration, diag_mediator_end
  use MOM,                 only : initialize_MOM, step_MOM, MOM_control_struct, MOM_end
  use MOM,                 only : calculate_surface_state, finish_MOM_initialization
  use MOM,                 only : step_offline
  use MOM_domains,         only : MOM_infra_init, MOM_infra_end
  use MOM_error_handler,   only : MOM_error, MOM_mesg, WARNING, FATAL, is_root_pe
  use MOM_error_handler,   only : callTree_enter, callTree_leave, callTree_waypoint
  use MOM_file_parser,     only : read_param, get_param, log_param, log_version, param_file_type
  use MOM_file_parser,     only : close_param_file
  use MOM_forcing_type,    only : forcing, forcing_diagnostics, mech_forcing_diags, MOM_forcing_chksum
  use MOM_get_input,       only : directories
  use MOM_grid,            only : ocean_grid_type
  use MOM_io,              only : file_exists, open_file, close_file
  use MOM_io,              only : check_nml_error, io_infra_init, io_infra_end
  use MOM_io,              only : APPEND_FILE, ASCII_FILE, READONLY_FILE, SINGLE_FILE
  use MOM_restart,         only : save_restart
  use MOM_string_functions, only : uppercase
  use MOM_sum_output,      only : write_energy, accumulate_net_input
  use MOM_sum_output,      only : MOM_sum_output_init, sum_output_CS
  use MOM_surface_forcing, only : set_forcing, forcing_save_restart
  use MOM_surface_forcing, only : surface_forcing_init, surface_forcing_CS
  use MOM_time_manager,    only : time_type, set_date, set_time, get_date, time_type_to_real
  use MOM_time_manager,    only : operator(+), operator(-), operator(*), operator(/)
  use MOM_time_manager,    only : operator(>), operator(<), operator(>=)
  use MOM_time_manager,    only : increment_date, set_calendar_type, month_name
  use MOM_time_manager,    only : JULIAN, GREGORIAN, NOLEAP, THIRTY_DAY_MONTHS
  use MOM_time_manager,    only : NO_CALENDAR
  use MOM_variables,       only : surface
  use MOM_verticalGrid,    only : verticalGrid_type
  use MOM_write_cputime,   only : write_cputime, MOM_write_cputime_init
  use MOM_write_cputime,   only : write_cputime_start_clock, write_cputime_CS

  use ensemble_manager_mod, only : ensemble_manager_init, get_ensemble_size
  use ensemble_manager_mod, only : ensemble_pelist_setup
  use mpp_mod, only : set_current_pelist => mpp_set_current_pelist

  use MOM_ice_shelf, only : initialize_ice_shelf, ice_shelf_end, ice_shelf_CS
  use MOM_ice_shelf, only : shelf_calc_flux, ice_shelf_save_restart

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_Advance   => label_Advance, &
    model_label_Finalize  => label_Finalize

  use time_utils_mod

  implicit none

  ! A structure containing pointers to the ocean forcing fields.
  type(forcing) :: fluxes

  ! A structure containing pointers to the ocean surface state fields.
  type(surface) :: state

  ! A pointer to a structure containing metrics and related information.
  type(ocean_grid_type), pointer :: grid
  type(verticalGrid_type), pointer :: GV

  ! If .true., use the ice shelf model for part of the domain.
  logical :: use_ice_shelf

  ! This is .true. if incremental restart files may be saved.
  logical :: permit_incr_restart = .true.

  integer :: n = 1

  ! nmax is the number of iterations after which to stop so that the
  ! simulation does not exceed its CPU time limit.  nmax is determined by
  ! evaluating the CPU time used between successive calls to write_energy.
  ! Initially it is set to be very large.
  integer :: nmax=2000000000;

  ! A structure containing several relevant directory paths.
  type(directories) :: dirs

  ! A suite of time types for use by MOM
  type(time_type), target :: Time       ! A copy of the ocean model's time.
                                        ! Other modules can set pointers to this and
                                        ! change it to manage diagnostics.
  type(time_type) :: Master_Time        ! The ocean model's master clock. No other
                                        ! modules are ever given access to this.
  type(time_type) :: Time1              ! The value of the ocean model's time at the
                                        ! start of a call to step_MOM.
  type(time_type) :: Start_time         ! The start time of the simulation.
  type(time_type) :: segment_start_time ! The start time of this run segment.
  type(time_type) :: Time_end           ! End time for the segment or experiment.
  type(time_type) :: write_energy_time  ! The next time to write to the energy file.
  type(time_type) :: restart_time       ! The next time to write restart files.
  type(time_type) :: Time_step_ocean    ! A time_type version of time_step.

  real    :: elapsed_time = 0.0   ! Elapsed time in this run in seconds.
  logical :: elapsed_time_master  ! If true, elapsed time is used to set the
                                  ! model's master clock (Time).  This is needed
                                  ! if Time_step_ocean is not an exact
                                  ! representation of time_step.
  real :: time_step               ! The time step of a call to step_MOM in seconds.
  real :: dt                      ! The baroclinic dynamics time step, in seconds.
  real :: dt_off                  ! Offline time step in seconds
  integer :: ntstep               ! The number of baroclinic dynamics time steps
                                  ! within time_step.

  integer :: Restart_control    ! An integer that is bit-tested to determine whether
                                ! incremental restart files are saved and whether they
                                ! have a time stamped name.  +1 (bit 0) for generic
                                ! files and +2 (bit 1) for time-stamped files.  A
                                ! restart file is saved at the end of a run segment
                                ! unless Restart_control is negative.

  real            :: Time_unit       ! The time unit in seconds for the following input fields.
  type(time_type) :: restint         ! The time between saves of the restart file.
  type(time_type) :: daymax          ! The final day of the simulation.
  type(time_type) :: energysavedays  ! The interval between writing the energies
                                     ! and other integral quantities of the run.

  integer :: date_init(6)=0                ! The start date of the whole simulation.
  integer :: date(6)=-1                    ! Possibly the start date of this run segment.
  integer :: years=0, months=0, days=0     ! These may determine the segment run
  integer :: hours=0, minutes=0, seconds=0 ! length, if read from a namelist.
  integer :: yr, mon, day, hr, min, sec    ! Temp variables for writing the date.
  type(param_file_type) :: param_file      ! The structure indicating the file(s)
                                           ! containing all run-time parameters.
  character(len=9)  :: month
  character(len=16) :: calendar = 'julian'
  integer :: calendar_type=-1

  integer :: unit, io_status, ierr
  integer :: ensemble_size, nPEs_per, ensemble_info(6)

  integer, dimension(0) :: atm_PElist, land_PElist, ice_PElist
  integer, dimension(:), allocatable :: ocean_PElist
  logical :: unit_in_use
  integer :: initClock, mainClock, termClock

  logical :: offline_tracer_mode ! If false, use the model in prognostic mode where
                                 ! the barotropic and baroclinic dynamics, thermodynamics,
                                 ! etc. are stepped forward integrated in time.
                                 ! If true, then all of the above are bypassed with all
                                 ! fields necessary to integrate only the tracer advection
                                 ! and diffusion equation are read in from files stored from
                                 ! a previous integration of the prognostic model

  type(MOM_control_struct),  pointer :: MOM_CSp => NULL()
  type(surface_forcing_CS),  pointer :: surface_forcing_CSp => NULL()
  type(sum_output_CS),       pointer :: sum_output_CSp => NULL()
  type(write_cputime_CS),    pointer :: write_CPU_CSp => NULL()
  type(ice_shelf_CS),        pointer :: ice_shelf_CSp => NULL()
  !-----------------------------------------------------------------------

  character(len=4), parameter :: vers_num = 'v2.0'
  character(len=*), parameter :: version = "MOM_SOLO_CAP"
  character(len=40)  :: mod_name = "MOM_main (MOM_driver)" ! This module's name.

  integer :: ocean_nthreads = 1
  integer :: ncores_per_node = 36
  logical :: use_hyper_thread = .false.
  integer :: omp_get_num_threads,omp_get_thread_num,get_cpu_affinity,adder,base_cpu
  namelist /ocean_solo_nml/ date_init, calendar, months, days, hours, minutes, seconds,&
                            ocean_nthreads, ncores_per_node, use_hyper_thread
  private
  public SetServices

  type fld_list_type
    character(len=64) :: stdname
    character(len=64) :: shortname
    character(len=64) :: transferOffer
    logical           :: assoc    ! is the farrayPtr associated with internal data
    real(ESMF_KIND_R8), dimension(:,:), pointer :: farrayPtr
  end type fld_list_type

  integer,parameter :: fldsMax = 100
  integer :: fldsToOcn_num = 0
  type (fld_list_type) :: fldsToOcn(fldsMax)
  integer :: fldsFrOcn_num = 0
  type (fld_list_type) :: fldsFrOcn(fldsMax)

  integer   :: import_slice = 1
  integer   :: export_slice = 1
  character(len=256) :: tmpstr
  integer   :: dbrc

  type(ESMF_Grid), save   :: mom_grid_i
  logical                 :: write_diagnostics = .true.
  logical                 :: profile_memory = .true.
  logical                 :: ocean_solo = .true.
  logical                 :: grid_attach_area = .false.
  integer(ESMF_KIND_I8)   :: restart_interval

  contains
  !-----------------------------------------------------------------------
  !------------------- Solo Ocean code starts here -----------------------
  !-----------------------------------------------------------------------

  !> NUOPC SetService method is the only public entry point.
  !! SetServices registers all of the user-provided subroutines
  !! in the module with the NUOPC layer.
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param rc return code  
  subroutine SetServices(gcomp, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    character(len=*),parameter  :: subname='(mom_cap:SetServices)'

    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ocean_model_finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  !> First initialize subroutine called by NUOPC.  The purpose
  !! is to set which version of the Initialize Phase Definition (IPD)
  !! to use.
  !!
  !! For this MOM cap, we are using IPDv01.
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param importState an ESMF_State object for import fields
  !! @param exportState an ESMF_State object for export fields
  !! @param clock an ESMF_Clock object
  !! @param rc return code
  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    character(len=10)                         :: value

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  !> Called by NUOPC to advertise import and export fields.  "Advertise"
  !! simply means that the standard names of all import and export
  !! fields are supplied.  The NUOPC layer uses these to match fields
  !! between components in the coupled system.
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param importState an ESMF_State object for import fields
  !! @param exportState an ESMF_State object for export fields
  !! @param clock an ESMF_Clock object
  !! @param rc return code
  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)

    type(ESMF_GridComp)                    :: gcomp
    type(ESMF_State)                       :: importState, exportState
    type(ESMF_Clock)                       :: clock
    integer, intent(out)                   :: rc

    type(ESMF_VM)                          :: vm
    integer                                :: mpi_comm_mom

    rc = ESMF_SUCCESS

    !call write_cputime_start_clock(write_CPU_CSp)

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

    call MOM_infra_init(mpi_comm_mom) ; call io_infra_init()

    ! Initialize the ensemble manager.  If there are no settings for ensemble_size
    ! in input.nml(ensemble.nml), these should not do anything.  In coupled
    ! configurations, this all occurs in the external driver.
    call ensemble_manager_init() ; ensemble_info(:) =  get_ensemble_size()
    ensemble_size=ensemble_info(1) ; nPEs_per=ensemble_info(2)
    if (ensemble_size > 1) then ! There are multiple ensemble members.
      allocate(ocean_pelist(nPEs_per))
      call ensemble_pelist_setup(.true., 0, nPEs_per, 0, 0, atm_pelist, ocean_pelist, &
                                 land_pelist, ice_pelist)
      call set_current_pelist(ocean_pelist)
      deallocate(ocean_pelist)
    endif

    ! These clocks are on the global pelist.
    initClock = cpu_clock_id( 'Initialization' )
    mainClock = cpu_clock_id( 'Main loop' )
    termClock = cpu_clock_id( 'Termination' )
    call cpu_clock_begin(initClock)

    call MOM_mesg('======== Model being driven by MOM_driver ========', 2)
    call callTree_waypoint("Solo Ocean CAP")

    if (file_exists('input.nml')) then
      ! Provide for namelist specification of the run length and calendar data.
      call open_file(unit, 'input.nml', form=ASCII_FILE, action=READONLY_FILE)
      read(unit, ocean_solo_nml, iostat=io_status)
      call close_file(unit)
      ierr = check_nml_error(io_status,'ocean_solo_nml')
      if (years+months+days+hours+minutes+seconds > 0) then
        if (is_root_pe()) write(*,ocean_solo_nml)
      endif
    endif

    ! Read ocean_solo restart, which can override settings from the namelist.
    if (file_exists(trim(dirs%restart_input_dir)//'ocean_solo.res')) then
      call open_file(unit,trim(dirs%restart_input_dir)//'ocean_solo.res', &
                     form=ASCII_FILE,action=READONLY_FILE)
      read(unit,*) calendar_type
      read(unit,*) date_init
      read(unit,*) date
      call close_file(unit)
    else
      calendar = uppercase(calendar)
      if (calendar(1:6) == 'JULIAN') then ;         calendar_type = JULIAN
      else if (calendar(1:9) == 'GREGORIAN') then ; calendar_type = GREGORIAN
      else if (calendar(1:6) == 'NOLEAP') then ;    calendar_type = NOLEAP
      else if (calendar(1:10)=='THIRTY_DAY') then ; calendar_type = THIRTY_DAY_MONTHS
      else if (calendar(1:11)=='NO_CALENDAR') then; calendar_type = NO_CALENDAR
      else if (calendar(1:1) /= ' ') then
        call MOM_error(FATAL,'MOM_driver: Invalid namelist value '//trim(calendar)//' for calendar')
      else
        call MOM_error(FATAL,'MOM_driver: No namelist value for calendar')
      endif
    endif
    call set_calendar_type(calendar_type)


    if (sum(date_init) > 0) then
      Start_time = set_date(date_init(1),date_init(2), date_init(3), &
           date_init(4),date_init(5),date_init(6))
    else
      Start_time = set_time(0,days=0)
    endif

    if (sum(date) >= 0) then
      ! In this case, the segment starts at a time fixed by ocean_solo.res
      segment_start_time = set_date(date(1),date(2),date(3),date(4),date(5),date(6))
      Time = segment_start_time
      ! Note the not before CS%d
      call initialize_MOM(Time, param_file, dirs, MOM_CSp, segment_start_time, offline_tracer_mode = offline_tracer_mode)
    else
      ! In this case, the segment starts at a time read from the MOM restart file
      ! or left as Start_time by MOM_initialize.
      Time = Start_time
      call initialize_MOM(Time, param_file, dirs, MOM_CSp, offline_tracer_mode=offline_tracer_mode)
    endif
    fluxes%C_p = MOM_CSp%tv%C_p  ! Copy the heat capacity for consistency.

    Master_Time = Time
    grid => MOM_CSp%G
    GV   => MOM_CSp%GV
    call calculate_surface_state(state, MOM_CSp%u, MOM_CSp%v, MOM_CSp%h, &
                                 MOM_CSp%ave_ssh, grid, GV, MOM_CSp)


    call surface_forcing_init(Time, grid, param_file, MOM_CSp%diag, &
                              surface_forcing_CSp, MOM_CSp%tracer_flow_CSp)
    call callTree_waypoint("done surface_forcing_init")

    call get_param(param_file, mod_name, "ICE_SHELF", use_ice_shelf, &
                   "If true, enables the ice shelf model.", default=.false.)
    if (use_ice_shelf) then
      ! These arrays are not initialized in most solo cases, but are needed
      ! when using an ice shelf
      call initialize_ice_shelf(param_file, grid, Time, ice_shelf_CSp, MOM_CSp%diag, fluxes)
    endif

    call MOM_sum_output_init(grid, param_file, dirs%output_directory, &
                             MOM_CSp%ntrunc, Start_time, sum_output_CSp)
    call MOM_write_cputime_init(param_file, dirs%output_directory, Start_time, &
                                write_CPU_CSp)
    call callTree_waypoint("done MOM_sum_output_init")

    segment_start_time = Time
    elapsed_time = 0.0

    ! Read all relevant parameters and write them to the model log.
    call log_version(param_file, mod_name, version, "")
    call get_param(param_file, mod_name, "DT", value=dt, fail_if_missing=.true.)
    call get_param(param_file, mod_name, "DT_FORCING", time_step, &
                   "The time step for changing forcing, coupling with other \n"//&
                   "components, or potentially writing certain diagnostics. \n"//&
                   "The default value is given by DT.", units="s", default=dt)
    if (offline_tracer_mode) then
      call get_param(param_file, mod_name, "DT_OFFLINE", time_step, &
                     "Time step for the offline time step")
      dt = time_step
    endif
    ntstep = MAX(1,ceiling(time_step/dt - 0.001))

    Time_step_ocean = set_time(int(floor(time_step+0.5)))
    elapsed_time_master = (abs(time_step - time_type_to_real(Time_step_ocean)) > 1.0e-12*time_step)
    if (elapsed_time_master) &
      call MOM_mesg("Using real elapsed time for the master clock.", 2)

    ! Determine the segment end time, either from the namelist file or parsed input file.
    call get_param(param_file, mod_name, "TIMEUNIT", Time_unit, &
                   "The time unit for DAYMAX, ENERGYSAVEDAYS, and RESTINT.", &
                   units="s", default=86400.0)
    if (years+months+days+hours+minutes+seconds > 0) then
      Time_end = increment_date(Time, years, months, days, hours, minutes, seconds)
      call MOM_mesg('Segment run length determined from ocean_solo_nml.', 2)
      call get_param(param_file, mod_name, "DAYMAX", daymax, timeunit=Time_unit, &
                     default=Time_end, do_not_log=.true.)
      call log_param(param_file, mod_name, "DAYMAX", daymax, &
                   "The final time of the whole simulation, in units of \n"//&
                   "TIMEUNIT seconds.  This also sets the potential end \n"//&
                   "time of the present run segment if the end time is \n"//&
                   "not set via ocean_solo_nml in input.nml.", &
                   timeunit=Time_unit)
    else
      call get_param(param_file, mod_name, "DAYMAX", daymax, &
                   "The final time of the whole simulation, in units of \n"//&
                   "TIMEUNIT seconds.  This also sets the potential end \n"//&
                   "time of the present run segment if the end time is \n"//&
                   "not set via ocean_solo_nml in input.nml.", &
                   timeunit=Time_unit, fail_if_missing=.true.)
      Time_end = daymax
    endif

    if (Time >= Time_end) call MOM_error(FATAL, &
      "MOM_driver: The run has been started at or after the end time of the run.")

    call get_param(param_file, mod_name, "RESTART_CONTROL", Restart_control, &
                   "An integer whose bits encode which restart files are \n"//&
                   "written. Add 2 (bit 1) for a time-stamped file, and odd \n"//&
                   "(bit 0) for a non-time-stamped file. A non-time-stamped \n"//&
                   "restart file is saved at the end of the run segment \n"//&
                   "for any non-negative value.", default=1)
    call get_param(param_file, mod_name, "RESTINT", restint, &
                   "The interval between saves of the restart file in units \n"//&
                   "of TIMEUNIT.  Use 0 (the default) to not save \n"//&
                   "incremental restart files at all.", default=set_time(0), &
                   timeunit=Time_unit)
    call get_param(param_file, mod_name, "ENERGYSAVEDAYS", energysavedays, &
                   "The interval in units of TIMEUNIT between saves of the \n"//&
                   "energies of the run and other globally summed diagnostics.", &
                   default=set_time(int(time_step+0.5)), timeunit=Time_unit)

    call log_param(param_file, mod_name, "ELAPSED TIME AS MASTER", elapsed_time_master)

    ! Close the param_file.  No further parsing of input is possible after this.
    call close_param_file(param_file)
    call diag_mediator_close_registration(MOM_CSp%diag)

    ! Write out a time stamp file.
    if (calendar_type /= NO_CALENDAR) then
      call open_file(unit, 'time_stamp.out', form=ASCII_FILE, action=APPEND_FILE, &
                     threading=SINGLE_FILE)
      call get_date(Time, date(1), date(2), date(3), date(4), date(5), date(6))
      month = month_name(date(2))
      if (is_root_pe()) write(unit,'(6i4,2x,a3)') date, month(1:3)
      call get_date(Time_end, date(1), date(2), date(3), date(4), date(5), date(6))
      month = month_name(date(2))
      if (is_root_pe()) write(unit,'(6i4,2x,a3)') date, month(1:3)
      call close_file(unit)
    endif

  !  This has been moved inside the loop to be applied when n=1.
  !  call write_energy(MOM_CSp%u, MOM_CSp%v, MOM_CSp%h, &
  !                    MOM_CSp%tv, Time, 0, grid, GV, sum_output_CSp, MOM_CSp%tracer_flow_CSp)
    call write_cputime(Time, 0, nmax, write_CPU_CSp)

    write_energy_time = Start_time + energysavedays * &
        (1 + (Time - Start_time) / energysavedays)

    if (((.not.BTEST(Restart_control,1)) .and. (.not.BTEST(Restart_control,0))) &
        .or. (Restart_control < 0)) permit_incr_restart = .false.

    if (restint > set_time(0)) then
      ! restart_time is the next integral multiple of restint.
      restart_time = Start_time + restint * &
          (1 + ((Time + Time_step_ocean) - Start_time) / restint)
    else
      ! Set the time so late that there is no intermediate restart.
      restart_time = Time_end + Time_step_ocean
      permit_incr_restart = .false.
    endif

    call cpu_clock_end(initClock) !end initialization

    write(*,*) '----- MOM initialization phase Advertise completed'

  end subroutine InitializeAdvertise
  
  !-----------------------------------------------------------------------------

  !> Called by NUOPC to realize import and export fields.  "Realizing" a field
  !! means that its grid has been defined and an ESMF_Field object has been
  !! created and put into the import or export State.
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param importState an ESMF_State object for import fields
  !! @param exportState an ESMF_State object for export fields
  !! @param clock an ESMF_Clock object
  !! @param rc return code
  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Local Variables
    type(ESMF_VM)                          :: vm
    type(ESMF_Grid)                        :: gridIn
    type(ESMF_Grid)                        :: gridOut
    type(ESMF_DeLayout)                    :: delayout
    type(ESMF_Distgrid)                    :: Distgrid
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    integer                                :: npet, ntiles
    integer                                :: nxg, nyg, cnt
    integer                                :: isc,iec,jsc,jec
    integer, allocatable                   :: xb(:),xe(:),yb(:),ye(:),pe(:)
    integer, allocatable                   :: deBlockList(:,:,:), &
                                              petMap(:),deLabelList(:), &
                                              indexList(:)
    integer                                :: ioff, joff
    integer                                :: i, j, n, i1, j1, n1, icount
    integer                                :: lbnd1,ubnd1,lbnd2,ubnd2
    integer                                :: lbnd3,ubnd3,lbnd4,ubnd4
    integer                                :: nblocks_tot
    logical                                :: found
    real(ESMF_KIND_R8), allocatable        :: ofld(:,:), gfld(:,:)
    real(ESMF_KIND_R8), pointer            :: t_surf(:,:)
    integer(ESMF_KIND_I4), pointer         :: dataPtr_mask(:,:)
    real(ESMF_KIND_R8), pointer            :: dataPtr_area(:,:)
    real(ESMF_KIND_R8), pointer            :: dataPtr_xcen(:,:)
    real(ESMF_KIND_R8), pointer            :: dataPtr_ycen(:,:)
    real(ESMF_KIND_R8), pointer            :: dataPtr_xcor(:,:)
    real(ESMF_KIND_R8), pointer            :: dataPtr_ycor(:,:)
    type(ESMF_Field)                       :: field_t_surf
    character(len=*),parameter  :: subname='(mom_cap:InitializeRealize)'
    
    rc = ESMF_SUCCESS

    write(*,*) '----- MOM initialization phase Realize completed'

  end subroutine InitializeRealize
  
  !> Called by NUOPC to advance the model a single timestep.
  !!  
  !! @param gcomp an ESMF_GridComp object
  !! @param rc return code
  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)                    :: gcomp
    integer, intent(out)                   :: rc
    
    rc = ESMF_SUCCESS
    if(profile_memory) call ESMF_VMLogMemInfo("Entering MOM Model_ADVANCE: ")

    if(profile_memory) call ESMF_VMLogMemInfo("Entering MOM update_ocean_model: ")
    call callTree_enter("Main loop, MOM_driver.F90",n)

    ! Set the forcing for the next steps.
    if (.not. offline_tracer_mode) then
        call set_forcing(state, fluxes, Time, Time_step_ocean, grid, &
                     surface_forcing_CSp)
    endif
    if (MOM_CSp%debug) then
      call MOM_forcing_chksum("After set forcing", fluxes, grid, haloshift=0)
    endif

    if (use_ice_shelf) then
      call shelf_calc_flux(state, fluxes, Time, time_step, ice_shelf_CSp)
!###IS     call add_shelf_flux_forcing(fluxes, ice_shelf_CSp)
!###IS  ! With a coupled ice/ocean run, use the following call.
!###IS      call add_shelf_flux_IOB(ice_ocean_bdry_type, ice_shelf_CSp)
    endif
    fluxes%fluxes_used = .false.
    fluxes%dt_buoy_accum = time_step

    if (n==1) then
      call finish_MOM_initialization(Time, dirs, MOM_CSp, fluxes)

      call write_energy(MOM_CSp%u, MOM_CSp%v, MOM_CSp%h, MOM_CSp%tv, &
                        Time, 0, grid, GV, sum_output_CSp, MOM_CSp%tracer_flow_CSp, &
                        MOM_CSp%OBC)
    endif

    ! This call steps the model over a time time_step.
    Time1 = Master_Time ; Time = Master_Time
    if (offline_tracer_mode) then
      call step_offline(fluxes, state, Time1, time_step, MOM_CSp)
    else
      call step_MOM(fluxes, state, Time1, time_step, MOM_CSp)
    endif

!   Time = Time + Time_step_ocean
!   This is here to enable fractional-second time steps.
    elapsed_time = elapsed_time + time_step
    if (elapsed_time > 2e9) then
      ! This is here to ensure that the conversion from a real to an integer
      ! can be accurately represented in long runs (longer than ~63 years).
      ! It will also ensure that elapsed time does not loose resolution of order
      ! the timetype's resolution, provided that the timestep and tick are
      ! larger than 10-5 seconds.  If a clock with a finer resolution is used,
      ! a smaller value would be required.
      segment_start_time = segment_start_time + set_time(int(floor(elapsed_time)))
      elapsed_time = elapsed_time - floor(elapsed_time)
    endif
    if (elapsed_time_master) then
      Master_Time = segment_start_time + set_time(int(floor(elapsed_time+0.5)))
    else
      Master_Time = Master_Time + Time_step_ocean
    endif
    Time = Master_Time

    call enable_averaging(time_step, Time, MOM_CSp%diag)
    call mech_forcing_diags(fluxes, time_step, grid, MOM_CSp%diag, &
                            surface_forcing_CSp%handles)
    call disable_averaging(MOM_CSp%diag)

    if (.not. offline_tracer_mode) then
      if (fluxes%fluxes_used) then
        call enable_averaging(fluxes%dt_buoy_accum, Time, MOM_CSp%diag)
        call forcing_diagnostics(fluxes, state, fluxes%dt_buoy_accum, grid, &
                                 MOM_CSp%diag, surface_forcing_CSp%handles)
        call accumulate_net_input(fluxes, state, fluxes%dt_buoy_accum, grid, sum_output_CSp)
        call disable_averaging(MOM_CSp%diag)
      else
        call MOM_error(FATAL, "The solo MOM_driver is not yet set up to handle "//&
               "thermodynamic time steps that are longer than the coupling timestep.")
      endif
    endif

!  See if it is time to write out the energy.
    if ((Time + (Time_step_ocean/2) > write_energy_time) .and. &
        (MOM_CSp%t_dyn_rel_adv == 0.0)) then
      call write_energy(MOM_CSp%u, MOM_CSp%v, MOM_CSp%h, &
                        MOM_CSp%tv, Time, n+ntstep-1, grid, GV, sum_output_CSp, &
                        MOM_CSp%tracer_flow_CSp)
      call write_cputime(Time, n+ntstep-1, nmax, write_CPU_CSp)
      write_energy_time = write_energy_time + energysavedays
    endif

!  See if it is time to write out a restart file - timestamped or not.
    if ((permit_incr_restart) .and. (fluxes%fluxes_used) .and. &
        (Time + (Time_step_ocean/2) > restart_time)) then
      if (BTEST(Restart_control,1)) then
        call save_restart(dirs%restart_output_dir, Time, grid, &
                          MOM_CSp%restart_CSp, .true., GV=GV)
        call forcing_save_restart(surface_forcing_CSp, grid, Time, &
                            dirs%restart_output_dir, .true.)
        if (use_ice_shelf) call ice_shelf_save_restart(ice_shelf_CSp, Time, &
                                    dirs%restart_output_dir, .true.)
      endif
      if (BTEST(Restart_control,0)) then
        call save_restart(dirs%restart_output_dir, Time, grid, &
                          MOM_CSp%restart_CSp, GV=GV)
        call forcing_save_restart(surface_forcing_CSp, grid, Time, &
                            dirs%restart_output_dir)
        if (use_ice_shelf) call ice_shelf_save_restart(ice_shelf_CSp, Time, &
                                    dirs%restart_output_dir)
      endif
      restart_time = restart_time + restint
    endif

    n = n + ntstep
    call callTree_leave("Main loop")
    if(profile_memory) call ESMF_VMLogMemInfo("Leaving MOM update_ocean_model: ")

    if(profile_memory) call ESMF_VMLogMemInfo("Leaving MOM Model_ADVANCE: ")
  end subroutine ModelAdvance

  !> Called by NUOPC at the end of the run to clean up.
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param rc return code
  subroutine ocean_model_finalize(gcomp, rc)

    ! input arguments
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(TIME_TYPE)                        :: Time        
    type(ESMF_Clock)                       :: clock
    type(ESMF_Time)                        :: currTime
    character(len=64)                      :: timestamp
    character(len=*),parameter  :: subname='(mom_cap:ocean_model_finalize)'

    write(*,*) 'MOM: --- finalize called ---'
    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    Time = esmf2fms_time(currTime)

    if (is_root_pe()) then
      do unit=10,1967
        INQUIRE(unit,OPENED=unit_in_use)
        if (.not.unit_in_use) exit
      enddo
      open(unit,FILE="exitcode",FORM="FORMATTED",STATUS="REPLACE",action="WRITE")
      if (Time < daymax) then
        write(unit,*) 9
      else
        write(unit,*) 0
      endif
      close(unit)
    endif

    call callTree_waypoint("End MOM_main")
    call diag_mediator_end(Time, MOM_CSp%diag, end_diag_manager=.true.)

    call io_infra_end ; call MOM_infra_end

    call MOM_end(MOM_CSp)
    if (use_ice_shelf) call ice_shelf_end(ice_shelf_CSp)

    write(*,*) 'MOM: --- completed ---'

  end subroutine ocean_model_finalize

!-----------------------------------------------------------------------------

end module mom_solo_cap_mod
