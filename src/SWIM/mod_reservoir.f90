!-------------------------------------------------------------------------------
! Authors : hagen.koch@pik-potsdam.de; stefan.liersch@pik-potsdam.de
! Version : 0.6
! Date    : 2012-01-15
! modified: 2016-09-28
!
! HISTORY OF CHANGES:
! 2014-12-11
! - The reservoir module is now using the water management module.
!   Additional supply from potential water user(s) is added in subroutine RSV_Reservoir_processes.
!   Withdrawals from water management user(s) are calculated in subroutine RSV_withdrawal.
!-------------------------------------------------------------------------------
!
! PURPOSE:  This module contains all variables and functions
!           that are required to simulate reservoirs and hydropower dams in SWIM.
!           Reservoirs must be implemented as subbasins!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
! INPUT  : - reservoir.ctrl
!          - reservoir_monthly.csv
!          - reservoir_storage_conf.csv
!
! OUTPUT : One output file is created for each reservoir
!          - ../Res/reservoir_'name'.out

!-------------------------------------------------------------------------------
! NOMENCLATURE
!  - all global module variable names start with 'rsv'
!  - all module-specific functions and subroutines start with 'RSV_'
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Subroutines & functions
!-------------------------------------------------------------------------------
!  - Subroutines
!     - subroutine RSV_init_reservoir
!     - subroutine RSV_init_parms
!     - subroutine RSV_Reservoir_processes(ih,in1,in2)
!     - subroutine RSV_daily_from_monthly
!     - subroutine RSV_Reservoir_outflow ! [m^3/d]
!     - subroutine RSV_release_discharge_refilling
!     - subroutine RSV_withdrawal(res)
!     - subroutine RSV_mean_water_level
!     - subroutine RSV_subbasin(ih,in1,in2)
!     - subroutine RSV_read_reservoir_control_file ! reservoir.ctrl
!     - subroutine RSV_read_reservoir_parm_file ! reservoir.csv
!     - subroutine RSV_read_reservoir_month_file ! reservoir_monthly.csv
!     - subroutine RSV_read_reservoir_stor_file ! reservoir_storage_conf.csv
!     - subroutine RSV_init_rsvSubbasin
!     - subroutine RSV_routing
!     - subroutine RSV_allocate_reservoir
!     - subroutine RSV_deallocate_reservoir

!  - Functions
!     - real(8) function RSV_calc_inflow(in2,res,sub,ida) ! returns inflow in [m^3/d]
!     - real(8) function RSV_calc_seepage(res)            ! returns seepage in m^3/d
!     - real(8) function RSV_release_firm_energy_yield(res)
!     - real(8) function RSV_release_depending_on_filling(res)
!     - real(8) function RSV_produced_energy(res)
!     - real(8) function RSV_POL(k,xp,yp,xps)
!     - integer function RSV_get_reservoir(sub)
!     - real(8) function ET_Turc(sub)
!     - integer function get_days_in_month(month,year)
!     - integer function get_days_in_month(month,year)
!     - logical function open_file(funit,fname)



!-------------------------------------------------------------------------------
! ToDo
!-------------------------------------------------------------------------------
! - Oftentimes the reservoir subbasin might consist of more than one hydrotope
!   - For correct GIS output, the relevant variables must be written to the respective hydrotope variables
!   - no hydrotope cycle in this module
! - What should happen to seepage?
!   - gwq? percolation to deep aquifer (loss from the system?)
!   - if gwq > 0 then water is usually generated in this system from existing lake volume
! - Input is varoute(2,in2) and varoute(8,in2), surface and subsurface flow
!   - what about reservoir output, is it surface and/or subsurface and gw?
!   - this has consequences for output statistics (bay.csv etc.)
!   - it has to be investigated what the downstream consequences of surface/subsurface fractions are.
! - Implement res_active function
!   - if res_active = 0, subbasin is a normal subbasin until res_active is set to 1 !
!   - outflow (wie aufteilen auf varoute(2,) und 8), bisher: wie inflows-Verteilung
! - Reservoir subbasin area must (should) be equal to the area indicated in the input file!
!   - currently we are using the area calculated from dry and wet area.
!-------------------------------------------------------------------------------


module mod_reservoir
use common_par ! include SWIM global variables
!use mod_water_mgt !#### WATER MANAGEMENT MODULE ####
implicit none

!#############################################################################
!
! VARIABLES & PARAMETERS
!
!#############################################################################
   ! logical/boolean value indicates whether the reservoir module is active (1) or not (0)
   ! This parameter is read from *.bsn input file.
   logical, save                               :: bRsvModule

   ! This variable is used to identify hydrograph nodes (ihouts(mhyd))
   ! that are input to a reservoir subbasin.
   ! used in mainpro.f90
   logical, save, dimension(:), allocatable    :: bRsvHydrograph       ! bRsvHydrograph(mhyd)

   ! the subbasin numbers/IDs used in SWIM
   !    if value = 0 = subbasin is no reservoir subbasin
   !    if value = subbasin number, subbasin is reservoir
   integer, save, dimension(:), allocatable    :: rsvSubbasin          ! rsvSubbasin(subtot)

   integer, save, dimension(:), allocatable    :: rsv_funit            ! file units used for output files

   ! Input file name used to determine the number of reservoirs used, necessary to allocate arrays
   ! and other general input parameters
   character(len=100), parameter               :: rsv_Reservoir_CTRL_file  = "reservoir.ctrl"
   ! Input file name used to parameterize reservoir(s)
   character(len=100), parameter               :: rsv_Reservoir_month_file = "reservoir_monthly.csv"
   ! Input file name used to parameterize reservoir(s)
   character(len=100), parameter               :: rsv_Reservoir_stor_file  = "reservoir_storage_conf.csv"

   real(8), dimension(:), allocatable          :: rsv_Inflow           ! Sum of inflow into reservoir from one or more upstream subbasins [m^3/d]
   real(8), dimension(:), allocatable          :: rsv_frac_sr          ! Fraction surface runoff inflow
   real(8), dimension(:,:), allocatable        :: rsv_Outflow          ! 1: simulated discharge; 2: available discharge [m3/d], dim(2,rsv_nReservoirs)
   real(8), dimension(:), allocatable          :: rsv_gwq              ! Ground water contribution to streamflow [mm]

   real(8), dimension(:), allocatable          :: rsv_B                ! Filling of reservoir beginning of month [m3], dimension(rsv_nReservoirs)

   ! dimension(10,nResersvoirs)
   real(8), dimension(:,:), allocatable        :: Rsv                  ! Reservoir filling during steps of simulation [m3], not including dead storage!
                                                                       ! 1 = reservoir filling of previous time step
                                                                       ! 2 = reservoir filling minus seepage
                                                                       ! 3 = reservoir filling +/- cwb (climatic water balance)
                                                                       ! 4 = reservoir filling (daily capacity) after evap, outflow, and inflow
                                                                       ! 5 = reservoir filling after release
                                                                       ! 6 = reservoir filling after spill
                                                                       ! 7 = reservoir filling after withdrawal
   real(8), dimension(:), allocatable          :: rsv_dead_stor_act    ! current fillling of the dead storage m^3

   real(8), dimension(:), allocatable          :: rsv_seepage          ! seepage from reservoir [m^3/d]

   real(8), dimension(:), allocatable          :: rsv_Day_Cap_Act      ! maximum daily active capacity calculated from monthly values [m3], dim(rsv_nReservoirs)
   real(8), dimension(:), allocatable          :: rsv_Day_Fill_Min     ! daily minimum filling calculated from monthly values [m3], dim(rsv_nReservoirs)
   real(8), dimension(:), allocatable          :: rsv_Day_ann_cycle    ! daily minimum filling calculated from monthly values [share of filling]

   real(8), dimension(:), allocatable          :: rsv_Day_Disch_Min    ! daily minimum discharge calculated from monthly values [m3/s], dim(rsv_nReservoirs)
   real(8), dimension(:,:), allocatable        :: rsv_Disch_Min        ! Minimum discharge downstream of Reservoir for months 1 to 12, 0 & 12 Dec. [m3/s]

   real(8), dimension(:), allocatable          :: rsv_act_withdrawal   ! volume allowed to be withdrawn from reservoir

   real(8), dimension(:), allocatable          :: rsv_water_level

   real(8), dimension(:), allocatable          :: rsv_height_hpp

   real(8), dimension(:), allocatable          :: rsv_Prod_HPP
   
   ! The total reservoir area in m^2 calculated from dart(j)
   real(8), dimension(:), allocatable          :: rsv_tot_area        ! m^2

!#############################################################################
!
! USER DEFINED PARAMETERS READ FROM RESERVOIR INPUT FILES
!
!#############################################################################

!read from reservoir.ctrl
   integer                                     :: rsv_nReservoirs      ! number of reservoirs
   integer, dimension(:), allocatable          :: rsv_ResSubbasins     ! subbasin number for reservoir 1..n

! reservoir parameter, read from reservoir.csv
! dimension(rsv_nReservoirs)
   character(len=20), dimension(:), allocatable:: rsv_ResNames         ! reservoir names, not too important
   real(8), dimension(:), allocatable          :: rsv_Capac_Max        ! Maximum capacity of Reservoir [m3]
   real(8), dimension(:), allocatable          :: rsv_dead_stor_capac  ! Dead storage [m3]
   real(8), dimension(:), allocatable          :: rsv_Start_Fill       ! Filling of Reservoir for 1st month of simulation [Filling / rsv_Cap_Act(1)]
   logical, dimension(:), allocatable          :: rsv_active           ! active Reservoir? no: 0 OR yes: 1
   real(8), dimension(:), allocatable          :: rsv_activate_thresh  ! activation threshold, gets active (if Res_active==0) if % of active storage volume is reached during filling
   real(8), dimension(:), allocatable          :: rsv_level_max        ! Maximum Waterlevel of HPP [mNN]
   real(8), dimension(:), allocatable          :: rsv_level_min        ! Minimum Waterlevel of HPP [mNN]
   real(8), dimension(:), allocatable          :: rsv_level_hpp        ! Maximum fall heigth of HPP [m]
   real(8), dimension(:), allocatable          :: rsv_cap_hpp          ! Capacity of HPP [m3/s]
   real(8), dimension(:), allocatable          :: rsv_eff_hpp          ! Efficiency of HPP [-], between 0.01 and 0.99 (eq. 1% and 99%); included 2015-07-07 (H. Koch)
   real(8), dimension(:), allocatable          :: rsv_loss_seepage     ! in [m3/s] per [million m3 of filling volume]
   real(8), dimension(:), allocatable          :: rsv_gwc              ! Fraction of seepage that becomes (gwq) ground water contribution to streamflow
   real(8), dimension(:), allocatable          :: rsv_evapc            ! Coefficient to correct lake evaporation
   integer, dimension(:), allocatable          :: rsv_start_year       ! Year for start of filling of new reservoir (rsv_active= 0)
   integer, dimension(:), allocatable          :: rsv_start_day        ! Day of year for start of filling of new reservoir (rsv_active= 0)
   integer, dimension(:), allocatable          :: rsv_Mngmt            ! Reservoir Management Option
                                                                         ! 1= Min (rsv_Fill_Min) & Max (rsv_Cap_Act) Capacity and Minimum Discharge (Rsv_Dis_Min)
                                                                         ! 2= safe energy yield (calculation of release depending on hydropower production)
                                                                         ! 3= release depending on filling of reservoir (might be rising or falling release with incresed filling)
   real(8), dimension(:), allocatable          :: rsv_shr_withdr       ! share of reservoir filling available for withdrawal directly from reservoir (Max: 1.0= 100%; Min: 0.0= 0%)

! monthly reservoir data, read from 'reservoir_monthly.csv'
! dimension(rsv_nReservoirs,0:12) 0=dec, 1=jan, 2=feb...12=dec
   real(8), dimension(:,:), allocatable        :: rsv_Cap_Act          ! [m3] (active capacity)
   real(8), dimension(:,:), allocatable        :: rsv_Fill_Min         ! [m3] (minimum filling)
   real(8), dimension(:,:), allocatable        :: rsv_Dis_Min_Fill     ! [m3/s] (minimum discharge during filling)
   real(8), dimension(:,:), allocatable        :: rsv_Dis_Min_Act      ! [m3/s] (minimum discharge during active storage)
   real(8), dimension(:,:), allocatable        :: rsv_Withdr_Mon       ! [m3/s]
   real(8), dimension(:,:), allocatable        :: rsv_ann_cycle        ! part of filling

! storage parameters, read from 'reservoir_storage_conf.csv'
! dimension(rsv_nReservoirs,20)
   real(8), dimension(:,:), allocatable        :: rsv_pol_L            ! water level [m.a.s.l.]
   real(8), dimension(:,:), allocatable        :: rsv_pol_L2           ! water level [m.a.s.l.]
   real(8), dimension(:,:), allocatable        :: rsv_pol_A            ! surface area [km2]
   real(8), dimension(:,:), allocatable        :: rsv_pol_V            ! storage volume including dead storage [m3]
   real(8), dimension(:,:), allocatable        :: rsv_pol_HP           ! release of water depending on water level (filling) [m3/s]
                                                                       ! rising or falling release with incresed filling (used with "rsv_rsv_pol_L2")
!#############################################################################

! values of previous time steps
   real(8), dimension(:), allocatable          :: pd_outflow
   real(8), dimension(:), allocatable          :: pd_seepage
   real(8), dimension(:), allocatable          :: pd_area_wet
   real(8), dimension(:), allocatable          :: pd_et
   real(8), dimension(:), allocatable          :: pd_gwq
   real(8), dimension(:), allocatable          :: pd_gwseep
   real(8), dimension(:), allocatable          :: pd_gwchrg
   real(8), dimension(:), allocatable          :: pd_wysb

!#############################################################################



!#############################################################################
!
CONTAINS
!
!#############################################################################

!*****************************************************************************
subroutine RSV_init_reservoir
! CALLED BY: mainpro
! initialise variables and arrays
   integer            :: res, funit
   character(len=255) :: filename = ""

   !-----------------------------------------------------------------
   ! read reservoir input files and allocate arrays
   !-----------------------------------------------------------------
   ! calls RSV_allocate_reservoir
   call RSV_read_reservoir_control_file ! reservoir.ctrl

   call RSV_read_reservoir_month_file   ! reservoir_monthly.csv

   call RSV_read_reservoir_stor_file    ! reservoir_storage_conf.csv
   !-----------------------------------------------------------------

   ! set reservoir subbasins of array rsvSubbasin
   call RSV_init_rsvSubbasin

   ! get bRsvHydrograph
   call RSV_routing

   ! initialise filling status etc.
   call RSV_init_parms

   !-----------------------------------------------------------------
   ! open and create reservoir output files (one file per reservoir)
   !-----------------------------------------------------------------
   res = 1
   do funit = 401, 401+rsv_nReservoirs-1
      rsv_funit(res) = funit
      filename = ""
      filename = trim(swimPath)//"Res/reservoir_"//trim(rsv_ResNames(res))//".out"
      open(funit,file=trim(filename),status='REPLACE')
      ! write header line
      write(funit,fmt="(a8)",advance="NO")   "YEAR DAY "
      write(funit,fmt="(a16)",advance="NO")  "Precip_m3/s"
      write(funit,fmt="(a16)",advance="NO")  "Precip_mm"
      write(funit,fmt="(a16)",advance="NO")  "CWB_m3/s"
      write(funit,fmt="(a16)",advance="NO")  "CWB_mm"
      write(funit,fmt="(a16)",advance="NO")  "Area_dry_km2"
      write(funit,fmt="(a16)",advance="NO")  "Area_wet_km2"
      write(funit,fmt="(a18)",advance="NO")  "Act_stor_Mio_m3"
      write(funit,fmt="(a18)",advance="NO")  "Dead_stor_Mio_m3"
      write(funit,fmt="(a16)",advance="NO")  "WaterLevel_masl"
      write(funit,fmt="(a16)",advance="NO")  "Inflow_m3/s"
      write(funit,fmt="(a16)",advance="NO")  "Outflow_m3/s"
      write(funit,fmt="(a16)",advance="NO")  "Withdrawal_m3"
      write(funit,fmt="(a16)",advance="NO")  "Seepage_m3/s"
      write(funit,fmt="(a16)",advance="NO")  "ETpot_water_mm"
      write(funit,fmt="(a16)",advance="NO")  "ETpot_land_mm"
      write(funit,fmt="(a16)",advance="NO")  "ETact_mm"
      write(funit,fmt="(a17)",advance="NO")  "ETact_m3/s"
      write(funit,fmt="(a17)",advance="YES") "Energy_MW"
      res = res + 1
   end do
   !-----------------------------------------------------------------

end subroutine RSV_init_reservoir
!*****************************************************************************

!*****************************************************************************
subroutine RSV_init_parms
! CALLED BY: RSV_init_reservoir
   integer :: i
   real(8)    :: A, SH, ResA_dry, ResA_wet

   ! rsv_B and Rsv in [m^3]
   do i = 1, rsv_nReservoirs
      ! if reservoir is active from the first day
      if ( rsv_active(i) ) then
         ! set dead storage volume to its capacity
         ! if not, it was already initialised with 0.
         rsv_dead_stor_act(i) = rsv_dead_stor_capac(i)
      
         ! initialise filling at first day
         ! Filling of reservoir beginning of month [m^3
         ! RsvB(k)= Rsv_Day_Cap_Act(k) * Start_Fill (Hagens version)
         rsv_B(i) = rsv_Capac_Max(i) * rsv_Start_Fill(i) - rsv_dead_stor_act(i) ! not the same as in Hagens version, because 'Rsv_Day_Cap_Act' not yet initialised
         rsv_B(i) = max(0.,rsv_B(i))
      end if
      
      ! Filling of reservoir during steps of simulation [m^3]
      Rsv(1,i) = rsv_B(i)

!!    Initialise parameters for first day
!!    necessary for subroutine Rsv_subbasin
      Rsv(2,i) = max(Rsv(1,i),0.)
      SH = 0. ! water level [m.a.s.l.]
      SH = RSV_POL(20,rsv_pol_V(i,:),rsv_pol_L(i,:),Rsv(2,i) + rsv_dead_stor_act(i))
      A = 0.  ! flooded surface area
      A = RSV_POL(20,rsv_pol_L(i,:),rsv_pol_A(i,:),SH)

      ! calculate dry and wet (flooded) reservoir surface area [km^2]
      ResA_dry = rsv_pol_A(i,20) - A
      ResA_dry = max(1.e-6,ResA_dry)
      ResA_wet = A
      ResA_wet = max(1.e-6,ResA_wet)

      ! sl 2015-08-18
      !pd_seepage(i) = (Rsv(1,i) * rsv_loss_seepage(i))/(ResA_wet*10**6)*1000.  ![mm]
      rsv_tot_area(i) = ResA_wet*10**6 + ResA_dry*10**6
      !rsv_tot_area(i) = dart(rsv_ResSubbasins(i))*10**6 ! subbasin area of the reservoir in m^2
      pd_seepage(i)   = (Rsv(1,i) * rsv_loss_seepage(i))/rsv_tot_area(i)*1000.  ![mm]
      
      rsv_frac_sr(i) = 0.5
      pd_outflow(i)  = 0.5 ![mm]
      pd_et(i)       = 1.  ![mm]

   end do
end subroutine RSV_init_parms
!*****************************************************************************


!*****************************************************************************
logical function RSV_is_operational(year,day,sub)
! check, if reservoir is already operating
   integer, intent(in) :: year, day, sub
   integer             :: res = 0
   
   RSV_is_operational = .false.
   res = RSV_get_reservoir(sub)
   if ( res > 0) then
      if ( year >  rsv_start_year(res) )                                 RSV_is_operational = .true.
      if ( year == rsv_start_year(res) .AND. day >= rsv_start_day(res) ) RSV_is_operational = .true.
   end if
end function RSV_is_operational
!*****************************************************************************


!*****************************************************************************
subroutine RSV_Reservoir_processes(ih,in1,in2)
! CALLED FROM mainpro.f90 during route command
   integer, intent(in)       :: ih                ! actual hydrograph storage location
   integer, intent(in)       :: in1               ! inum1: subbasin number of this reservoir
   integer, intent(in)       :: in2               ! inum2: the input (upstream) subbasin or hydrograph storage location
   integer                   :: sub               ! subbasin number
   integer                   :: res               ! reservoir number
   real(8)                   :: ETpot_water_mm    ! potential evapotranspiration of water surface [mm]
   real(8)                   :: ETpot_land_mm     ! potential evapotranspiration of land surface [mm]
   real(8)                   :: ETpot_water_m3    ! potential evapotranspiration of land surface [m^3/d]
   real(8)                   :: ETpot_land_m3     ! potential evapotranspiration of land surface [m^3/d]
   real(8)                   :: ETpot_weighted_mm ! area weighted pot. evapotranspiration (water/land)
   real(8)                   :: a_frac_wet        ! fraction of flooded surface area total reservoir area
   real(8)                   :: A                 ! A = flooded surface area [km^2]
   real(8)                   :: ResA_dry          ! Area of Reservoir which is dry [km^2]
   real(8)                   :: ResA_wet          ! Area of Reservoir which is wet [km^2]
   real(8)                   :: cwb               ! climatic water balance [m^3/d]
   real(8)                   :: precip            ! precipation [mm/d] current day on reservoir subbasin
   real(8)                   :: Res_space         ! volume of Reservoir not filled [m3]
   real(8)                   :: d_Q               ! delta Quantity [m^3/d]
   real(8)                   :: release           ! Reservoir release [m3/d]
   real(8)                   :: SH                ! water level [m.a.s.l.]
   real(8)                   :: excess
   integer                   :: funit             ! file unit of current reservoir
!   TYPE (TSubbasin), POINTER :: pS                ! pointer on subbasin !#### WATER MANAGEMENT MODULE ####

   !-----------------------------------------------------------------
   ! initialise
   !-----------------------------------------------------------------
   sub                = in1
   res                = RSV_get_reservoir(sub)
   Res_space          = 0.
   d_Q                = 0.
   release            = 0.
   precip             = subp(sub) ! Precipitation on reservoir subbasin [mm]
   rsv_Outflow(1,res) = 0.
   rsv_Outflow(2,res) = 0.

   ! set actual reservoir volume to previous day volume
   Rsv(1,res) = rsv_B(res)
   !-----------------------------------------------------------------

   !-----------------------------------------------------------------
   ! check if reservoir is active
   ! if not: set active if actual storage volume exceeds given threshold
   !-----------------------------------------------------------------
   if ( .NOT. rsv_active(res) ) then
      if ( Rsv(1,res) >= (rsv_Capac_Max(res) - rsv_dead_stor_capac(res)) * rsv_activate_thresh(res) ) &
         rsv_active(res) = .true.
   end if
   !-----------------------------------------------------------------
   
   !-----------------------------------------------------------------
   ! calculate daily data from monthly data
   ! rsv_Day_Cap_Act, rsv_Day_Fill_Min, Rsv_ann_cycle, rsv_Day_Disch_Min
   !-----------------------------------------------------------------
   call RSV_daily_from_monthly
   !-----------------------------------------------------------------

   !-----------------------------------------------------------------
   ! calculate inflow volume to reservoir subbasin [m^3/d]
   !-----------------------------------------------------------------
   rsv_Inflow(res) = RSV_calc_inflow(in2,res,sub,ida)

!    !#################################
!    !#### WATER MANAGEMENT MODULE ####
!    !#################################
!    ! Add inflows from transfers
!    if ( bWAM_Module ) then
!       ! check if water transfers take place in current subbasin
!       if ( wam_is_transfer_subbasin(sub) ) then
!          pS => wam_get_pointer_subbasin(sub) ! get pointer on current subbasin
!          ! add subbasins' %inflow(daycounter-1) of previous day
!          if (daycounter > 1) rsv_Inflow(res) = rsv_Inflow(res) + pS%inflow(daycounter-1) * 86400.
!       end if
!    end if
!    !#################################
   !-----------------------------------------------------------------

   ! calculate fraction of surface runoff to total inflow
   !rsv_frac_sr(res) = varoute(2,in2) / (varoute(2,in2) + varoute(8,in2))

   !-----------------------------------------------------------------
   ! calculate seepage from reservoir [m^3/d]
   !-----------------------------------------------------------------
   rsv_seepage(res) = RSV_calc_seepage(res)

   ! remove seepage from actual and/or dead storage
   ! re-calculate actual storage volume Rsv(2,res) after seepage in [m^3]
   ! re-calculate dead storage if necessary
   call RSV_remove_seepage(res)
   !-----------------------------------------------------------------

   !-----------------------------------------------------------------
   ! calculate water level [m.a.s.l.] and flooded surface area [km^2]
   !-----------------------------------------------------------------
   SH = 0. ! water level [m.a.s.l.]
   SH = RSV_POL(20,rsv_pol_V(res,:),rsv_pol_L(res,:),Rsv(2,res) + rsv_dead_stor_act(res))
   A = 0.  ! flooded surface area
   A = RSV_POL(20,rsv_pol_L(res,:),rsv_pol_A(res,:),SH)

   ! calculate dry and wet (flooded) reservoir surface area [km^2]
   ResA_dry = rsv_pol_A(res,20) - A
   ResA_dry = max(1.e-6,ResA_dry)
   ResA_wet = A
   ResA_wet = max(1.e-6,ResA_wet)
   ! fraction of flooded reservoir subbasin area
   a_frac_wet = ResA_wet / rsv_pol_A(res,20)
   !-----------------------------------------------------------------

   !-----------------------------------------------------------------
   ! calculate ETpot in [mm] and [m^3/d]
   !-----------------------------------------------------------------
   ETpot_water_mm   = 0.
   ETpot_water_m3   = 0.
   ETpot_land_mm = 0.
   ETpot_land_m3 = 0.
   ETpot_water_mm   = ET_Turc(sub) * ecal(sub) * 1.3 * rsv_evapc(res)
   ETpot_land_mm    = ET_Turc(sub) * ecal(sub) * rsv_evapc(res)
   ETpot_land_m3    = ETpot_land_mm/10**3 * ResA_dry*10**6
   ETpot_water_m3   = ETpot_water_mm/10**3 * A*10**6
   ! area weighted evapotranspiration from reservoir subbasin
   ! ToDo: here, we assume that ET_a from land surface equals ET_pot from land surface which may lead to overestimations
   ETpot_weighted_mm = ETpot_water_mm * a_frac_wet + ETpot_land_mm * (1.-a_frac_wet)

   ! calculate climatic water balance [m^3/d]
   cwb = (precip/10**3 * rsv_pol_A(res,20)*10**6) - (ETpot_land_m3 + ETpot_water_m3)
   !-----------------------------------------------------------------

   !-----------------------------------------------------------------
   ! remove evaporation from actual and/or dead storage
   ! re-calculate actual storage volume Rsv(3,res) after cwb, evaporation in [m^3]
   ! re-calculate dead storage if necessary
   !-----------------------------------------------------------------
   if ( cwb < 0. ) then
      call RSV_remove_cwb(res)
   else
      call RSV_add_cwb(res)
      ! determine the volume of water that must be released in order to reduce Rsv to daily maximum active capacity
      excess = cwb - (rsv_Day_Cap_Act(res) - Rsv(2,res))
      if ( excess > 0. ) then
         rsv_Outflow(1,res) = max(excess,0.) ! simulated reservoir discharge
         rsv_Outflow(2,res) = max(excess,0.) ! available reservoir discharge
      end if
   end if
   !-----------------------------------------------------------------
   
   !*****************************************************************
   !*****************************************************************
   ! If the active storage is empty, filling and outflow is determined
   ! by the status of the dead storage.
   !*****************************************************************
   !*****************************************************************
   if ( .NOT. RSV_is_full_dead_storage(res) ) then ! if dead storage is not full

      !-----------------------------------------------------------------
      ! as long as dead storage is not full, calc filling of dead storage
      ! and discharges downstream
      !-----------------------------------------------------------------
      excess = 0.
      excess = rsv_Inflow(res) - rsv_Day_Disch_Min(res) * 86400.
      
      ! if inflow is larger than required minimal discharges
      if ( excess > 0. ) then
         ! fill dead storage
         rsv_dead_stor_act(res) = rsv_dead_stor_act(res) + excess

         ! dead storage is full after filling
         if ( RSV_is_full_dead_storage(res) ) then
            ! set active storage
            Rsv(4,res) = max(rsv_dead_stor_act(res) - rsv_dead_stor_capac(res),0.)
            ! set dead storage to max. dead storage capacity
            rsv_dead_stor_act(res) = rsv_dead_stor_capac(res)
            rsv_Outflow(1,res) = rsv_Day_Disch_Min(res) * 86400.

         ! dead storage is not full after filling
         else
            Rsv(4,res) = 0. ! active storage
            rsv_outflow(1,res) = rsv_Day_Disch_Min(res) * 86400.
         end if

      ! if inflow is not larger than required minimal discharges
      else
         ! route inflow through without filling the dead storage
         rsv_Outflow(1,res) = rsv_Inflow(res)
         Rsv(4,res) = 0.

      end if
      !-----------------------------------------------------------------

      ! calculate water level in reservoir [m.a.s.l.]
      call RSV_mean_water_level(Rsv(4,res),rsv_dead_stor_act(res),res)

      ! set current reservoir volume for next day
      ! if Rsv(4,res) got part of the filling, ignore it this time step
      ! volumes are certainly negligible
      if ( Rsv(4,res) == 0. ) Rsv(:,res) = 0.
      rsv_B(res) = Rsv(4,res)
      
      rsv_Prod_HPP(res) = 0.

   !*****************************************************************
   else
   ! if dead storage was full, continue here...
   !*****************************************************************

      !-----------------------------------------------------------------
      ! calculate reservoir outflow
      !Todo: Warum eigentlich hier?
      !-----------------------------------------------------------------
      call RSV_Reservoir_outflow
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      ! if reservoir is operating
      !-----------------------------------------------------------------
      if ( rsv_active(res) ) then

         select case(rsv_Mngmt(res)) ! Reservoir Management Option
            case(1) ! Min (rsv_Fill_Min) & max (rsv_Cap_Act) capacity and minimum discharge (Rsv_Dis_Min)
               ! continue below

            case(2) ! safe energy yield (calculation of release depending on hydropower production)
               ! calculate release
               release = RSV_release_firm_energy_yield(res) ! release in [m3/d]
            case(3) ! release depending on filling of reservoir (might be rising or falling release with incresed filling)
               ! calculate release
               release = RSV_release_depending_on_filling(res) ! [m3/d]

            case default
               write(*,*) "No valid reservoir management option was selected! Valid setting of rsv_Mngmt (1-3)"
               write(*,*) "Check input file: ../data/reservoir.csv"
               write(*,*) "SWIM stops!"
               STOP
            ! do nothing
         end select

         ! calculate minimal discharges
         rsv_Day_Disch_Min(res) = release / 86400. ! [m3/d -> m3/s]
         rsv_Day_Fill_Min(res)  = max(Rsv(4,res)-release,0.)

      end if ! if ( rsv_active(res) )
      !-----------------------------------------------------------------

      !if ( .NOT. RSV_is_full_dead_storage(res) ) then
      !   call RSV_fill_dead_storage(res), wie bisher in RSV_release_discharge_refilling
      !   set values of Rsv(4,5,6,res)
      ! else: call RSV_release_discharge_refilling
      
      call RSV_release_discharge_refilling

      ! calculate direct withdrawal from reservoir (irrigation, drinking water etc.)
      call RSV_withdrawal(res)

      ! calculate water level in reservoir [m.a.s.l.]
      call RSV_mean_water_level(Rsv(7,res),rsv_dead_stor_act(res),res)

      ! calculate firm energy yield, release is calculated depending on water level
      rsv_Prod_HPP(res) = RSV_produced_energy(res)

      ! set current reservoir volume for next day
      rsv_B(res) = Rsv(7,res)

      ! if (rsv_active(res) > 0) RsvE(res) = RsvE(res) + rsv_dead_stor_act(res)

      rsv_Day_Cap_Act(res) = rsv_Day_Cap_Act(res)  + rsv_dead_stor_act(res)     ! incl. dead storage
      rsv_Day_Fill_Min(res)= rsv_Day_Fill_Min(res) + rsv_dead_stor_act(res)    ! incl. dead storage

   end if ! if dead storage is empty...
   !*****************************************************************
   !*****************************************************************

   !varoute(1,ih) = precip
   varoute(2,ih) = rsv_Outflow(1,res) * 0.5 ! rsv_frac_sr(res)     ! [m^3]
   varoute(8,ih) = rsv_Outflow(1,res) * 0.5 !(1-rsv_frac_sr(res)) ! [m^3]
   ! Add ground water contribution to gw flow component varoute(8,)
   ! Ground water contribution is computed in subroutine RSV_subbasin
! sl 2015-08-18
   !varoute(8,ih) = varoute(8,ih) + (rsv_gwq(res) / 1000. * ResA_wet*10**6)     ! mm-->m^3
   varoute(8,ih) = varoute(8,ih) + (rsv_gwq(res) / 1000. * rsv_tot_area(res))     ! mm-->m^3

!    !#################################
!    !#### WATER MANAGEMENT MODULE ####
!    !#################################
!    if ( bWAM_Module ) then
!       ! check if water transfers take place in current subbasin
!       if ( wam_is_transfer_subbasin(sub) ) then
!          pS => wam_get_pointer_subbasin(sub) ! get pointer on current subbasin
!          ! set Q_act
!          pS%Q_act(daycounter) = rsv_Outflow(1,res) / 86400. ! m3/s
!       end if
!    end if
!    !#################################
   ! set previous day (pd) variables
! sl 2015-08-18
   !pd_outflow(res)  = (rsv_Outflow(1,res)/((ResA_dry+ResA_wet)*10**6))*1000. ! [mm]
   !pd_outflow(res)  = (rsv_Outflow(1,res)/(rsv_upstream_area(res)*10**6))*1000. ! [mm]
   pd_outflow(res)  = rsv_Outflow(1,res) ![m3]
   !pd_seepage(res)  = (rsv_seepage(res)/(ResA_wet*10**6))*1000.              ! [mm]
   pd_seepage(res)  = (rsv_seepage(res)/(rsv_tot_area(res)))*1000.              ! [mm]
   pd_area_wet(res) = ResA_wet                                               ! [km^2]
   pd_et(res)       = ETpot_weighted_mm                                      ! [mm]
   !    pd_gwq(res)      =
   !    pd_gwseep(res)   =
   !    pd_gwchrg(res)   =
   !    pd_wysb(res)     =

   ! #### write output files
   funit = rsv_funit(res) ! get file unit
   write(funit,fmt="(i4,i4)", advance="NO")  iyr,ida
   write(funit,fmt="(2f16.2)",advance="NO")  (precip/10**3 * rsv_pol_A(res,20)*10**6)/86400.,precip ! m^3/s and mm
   write(funit,fmt="(2f16.2)",advance="NO")  cwb/86400.,(cwb/(rsv_pol_A(res,20)*10**6))*10**3       ! m^3/s and mm
   write(funit,fmt="(2f16.4)",advance="NO")  ResA_dry,ResA_wet                                      ! km^2
   write(funit,fmt="(f18.2)", advance="NO")  Rsv(7,res)/10**6                                       ! [million m^3]
   write(funit,fmt="(f18.2)", advance="NO")  rsv_dead_stor_act(res)/10**6                           ! [million m^3]
   write(funit,fmt="(f16.2)", advance="NO")  rsv_water_level(res)                                   ! [m.a.s.l.]
   write(funit,fmt="(2f16.2)",advance="NO")  rsv_Inflow(res)/86400.,rsv_Outflow(1,res)/86400.       ! [m^3/s]
   write(funit,fmt="(2f16.2)",advance="NO")  rsv_act_withdrawal(res)/86400.,rsv_seepage(res)/86400. ! [m^3 and m^3/s]
   write(funit,fmt="(2f16.2)",advance="NO")  ETpot_water_mm,ETpot_land_mm                           ! ETpot water and land [mm]
   write(funit,fmt="(f16.2)", advance="NO")  ETpot_weighted_mm                                      ! actual ET [mm]
   write(funit,fmt="(f16.2)",advance="NO")   (ETpot_land_m3+ETpot_water_m3)/86400.
   write(funit,fmt="(f16.2)",advance="YES")  rsv_Prod_HPP(res)                                      ! produced energy in MW


!***************************
! subroutine RSV_Reservoir_processes
CONTAINS
! following subroutines and functions
!***************************

!-----------------------------------------------------------------------
   subroutine RSV_daily_from_monthly
      integer :: d_im ! number of days in current month
      integer :: d_om ! day of month
                      ! mo  = month (global variable in SWIM)
                      ! iyr = year  (global variable in SWIM)

      ! calculate date stuff
      d_im = get_days_in_month(mo,iyr)
      d_om = get_day_of_month(mo,iyr)

      ! maximum daily active capacity calculated from monthly values [m^3]
      ! re-calculated in RSV_release_discharge_refilling: (calculation of reservoir re-filling)
      rsv_Day_Cap_Act(res) = (rsv_Cap_Act(res,mo)  - rsv_Cap_Act(res,mo-1)) / d_im * d_om
      rsv_Day_Cap_Act(res) =  rsv_Day_Cap_Act(res) + rsv_Cap_Act(res,mo-1)

      ! daily minimum filling (minimum daily active capacity) calculated from monthly values [m^3]
      ! volume of dead storage is added at the end of the day
      ! possibly re-calculated in calculation of firm energy yield (mgt option 2)
      ! possibly re-calculated in calculation of reservoir release (mgt option 3)
      rsv_Day_Fill_Min(res) = (rsv_Fill_Min(res,mo) - rsv_Fill_Min(res,mo-1)) / d_im * d_om
      rsv_Day_Fill_Min(res) = rsv_Day_Fill_Min(res) + rsv_Fill_Min(res,mo-1)

      ! daily minimum filling calculated from monthly values [share of filling]
!Todo: probably not necessary to do this every day (only once in the beginning)
      rsv_Day_ann_cycle(res) = (rsv_ann_cycle(res,mo) - rsv_ann_cycle(res,mo-1)) / d_im * d_om
      rsv_Day_ann_cycle(res) = rsv_Day_ann_cycle(res) + rsv_ann_cycle(res,mo-1)

!Todo: das könnte wahrscheinlich optimiert werden (vielleicht nicht jeden Tag aufrufen)
      ! [m^3/s]
      
      ! set mimimal discharges according to status of reservoir (filling or operational)
      if ( rsv_active(res) ) then
         rsv_Disch_Min(res,mo) = rsv_Dis_Min_Act(res,mo)   ! is operational
      else
         rsv_Disch_Min(res,mo) = rsv_Dis_Min_Fill(res,mo)  ! during period of filling
      end if

      ! daily minimum discharge calculated from monthly values [m^3/s]
      ! re-calculated if mgt option > 1
      rsv_Day_Disch_Min(res) = (rsv_Disch_Min(res,mo) - rsv_Disch_Min(res,mo-1)) / d_im * d_om
      rsv_Day_Disch_Min(res) = rsv_Day_Disch_Min(res) + rsv_Disch_Min(res,mo-1)

   end subroutine RSV_daily_from_monthly
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   real(8) function RSV_calc_inflow(in2,res,sub,ida) ! returns inflow in [m^3/d]
      integer, intent(in) :: in2,res,sub,ida

      RSV_calc_inflow = 0.
      RSV_calc_inflow = varoute(2,in2) + varoute(8,in2) ! [m^3/d]
      if ( RSV_calc_inflow < 0. ) then
         RSV_calc_inflow = 0.
         if ( bErrorFile ) write(1000,*) "Reservoir inflow is negative!!! (res,sub,day)",res,sub,ida
      end if
   end function RSV_calc_inflow
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   real(8) function RSV_calc_seepage(res) ! returns seepage in m^3/d
      integer, intent(in) :: res
!Todo: rsv_loss_seepage is probably too high /10^6
      RSV_calc_seepage = (Rsv(1,res) + rsv_dead_stor_act(res)) * rsv_loss_seepage(res)
   end function RSV_calc_seepage
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   subroutine RSV_remove_seepage(res)
   ! remove seepage from storage(s)
      integer, intent(in) :: res  ! actual reservoir
      real(8)             :: vol  ! volume, m^3
      
      vol = Rsv(1,res) - rsv_seepage(res)
      if ( vol > 0. ) then
         Rsv(2,res) = vol
      else ! if seepage > actual storage
         Rsv(:,res) = 0.
         ! remove negative seepage value from dead storage
         rsv_dead_stor_act(res) = max(rsv_dead_stor_act(res) + vol, 0.)
      end if
      
   end subroutine RSV_remove_seepage
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   subroutine RSV_remove_cwb(res)
   ! remove evaporation from storage(s)
      integer, intent(in) :: res  ! actual reservoir
      real(8)             :: vol  ! volume, m^3
      
      vol = Rsv(2,res) + cwb ! cwb is negative!
      if ( vol > 0. ) then
         Rsv(3,res) = vol
      else ! if evaporation > actual storage
         Rsv(:,res) = 0.
         ! remove negative cwb value from dead storage
         rsv_dead_stor_act(res) = max(rsv_dead_stor_act(res) + vol, 0.)
      end if

   end subroutine RSV_remove_cwb
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   subroutine RSV_add_cwb(res)
   ! remove evaporation from storage(s)
      integer, intent(in) :: res  ! actual reservoir
      real(8)             :: dx

      if ( .NOT. RSV_is_full_dead_storage(res) ) then
         ! if dead storage is not full
         dx = rsv_dead_stor_capac(res) - (rsv_dead_stor_act(res) + cwb)
         if ( dx < 0. ) then
            ! add to dead storage
            rsv_dead_stor_act(res) = rsv_dead_stor_capac(res)
            ! add to active storage
            Rsv(3,res) = min(Rsv(2,res) + dx, rsv_Day_Cap_Act(res)) 
         else
            rsv_dead_stor_act(res) = rsv_dead_stor_act(res) + cwb
         end if
  
      else ! if dead storage was already full
         ! add cwb to reservoir storage
         Rsv(3,res) = min(Rsv(2,res) + cwb, rsv_Day_Cap_Act(res)) 
      end if

   end subroutine RSV_add_cwb
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   subroutine RSV_Reservoir_outflow ! [m^3/d]
      !real(8) :: Res_free  ! [m^3]
      real(8) :: Res_space ! Volume of reservoir active capacity not filled [m^3]
      !real(8) :: dx        ! The volume [m^3/d] that should be released from the reservoir if Rsv > rsv_Day_Cap_Act
      real(8) :: Rsv_Spill ! Volume of water surpassing Reservoir capacity [m^3/d]

!       rsv_Outflow(1,res) = 0.
!       rsv_Outflow(2,res) = 0.
!       dx                 = 0.
!       Res_free           = rsv_Day_Cap_Act(res) - Rsv(2,res)
! 
!       if (cwb > 0.) then
!          ! if cwb positive, add to reservoir storage
!          Rsv(3,res) = min(Rsv(2,res) + cwb, rsv_Day_Cap_Act(res))
!          dx = cwb - Res_free
!          if (dx > 0.) then
!             ! determine the volume of water that must be released in order to reduce Rsv to daily maximum active capacity
!             rsv_Outflow(1,res) = dx ! simulated reservoir discharge
!             rsv_Outflow(2,res) = dx ! available reservoir discharge
!          end if
!       else
!          ! if cwb < 0. remove evaporated volume from actual storage volume
!          Rsv(3,res) = max(Rsv(2,res) + cwb, 0.)
!       end if

      ! calculate reservoir volume (not filled)
      Res_space = rsv_Day_Cap_Act(res) - Rsv(3,res)
      if (Res_space < 0.) then
         rsv_Outflow(1,res) = rsv_Outflow(1,res) + Res_space * (-1.) + rsv_Inflow(res)
         rsv_Outflow(2,res) = rsv_Outflow(2,res) + Res_space * (-1.) + rsv_Inflow(res)
         Rsv(4,res) = rsv_Day_Cap_Act(res)
      else
         ! Calculation of reservoir spill
         Rsv_Spill = rsv_Inflow(res) - Res_space
         if (Rsv_Spill > 0.) then
            rsv_Outflow(1,res) = rsv_Outflow(1,res) + Rsv_Spill
            rsv_Outflow(2,res) = rsv_Outflow(2,res) + Rsv_Spill
            Rsv(4,res) = rsv_Day_Cap_Act(res)
         else
            Rsv(4,res) = Rsv(3,res) + rsv_Inflow(res)
         end if
      end if

   end subroutine RSV_Reservoir_outflow
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   subroutine RSV_release_discharge_refilling
      real(8) :: release      ! Volume of water available in the Reservoir [m^3/d]
      real(8) :: Res_free     ! [m^3]
      real(8) :: Rsv_Spill    ! Volume of water surpassing Reservoir capacity [m^3]
      real(8) :: Rsv_Add_Cap  ! additional active capacity [m3]
      
      ! calculate reservoir release
      release = 0.
      release = release + Rsv(4,res) - rsv_Day_Fill_Min(res)
      if (release > 0.) then
          rsv_Outflow(1,res) = rsv_Outflow(1,res) + release
          rsv_Outflow(2,res) = rsv_Outflow(2,res) + release
          Rsv(5,res) = max(Rsv(4,res) - release, 0.)
      else
          Rsv(5,res) = Rsv(4,res)
      end if

      ! calculate discharge downstream
      d_Q = rsv_Outflow(1,res) - max(rsv_Day_Disch_Min(res) * 86400.,0.)
      ! calculate reservoir re-filling
      ! Rsv(5,res) = actual storage volume where release (for hydropower or minimal requirements downstream) is already abstracted
      rsv_Outflow(2,res) = 0.
      if (d_Q > 0.) then
         rsv_Outflow(1,res) = max(rsv_Day_Disch_Min(res) * 86400., 0.)
         Res_free = Rsv(5,res) - rsv_Day_Cap_Act(res)
         Rsv_Spill = d_Q + Res_free
         if (Rsv_Spill > 0.) then
            release = rsv_Outflow(1,res) + Rsv_Spill
            Rsv_Spill = MIN(release, rsv_cap_hpp(res) * 86400.) ! rsv_cap_hpp is in [m^3/s]
            Res_space = release - Rsv_Spill
            Rsv_Spill = release - rsv_Outflow(1,res)
            if ( rsv_Day_Cap_Act(res) < (rsv_Capac_Max(res) - rsv_dead_stor_act(res)) ) then
               if (Res_space > 0.) then
                  release              = MAX(((rsv_cap_hpp(res) * 86400.) - rsv_Outflow(1,res)),0.)
                  Res_space            = MAX((Rsv_Spill - release), 0.)
                  Rsv_Add_Cap          = rsv_Capac_Max(res) - rsv_dead_stor_act(res) - rsv_Day_Cap_Act(res)
                  Rsv_Add_Cap          = MIN(Res_space, Rsv_Add_Cap)
                  rsv_Day_Cap_Act(res) = rsv_Day_Cap_Act(res) + Rsv_Add_Cap
                  Rsv_Spill            = MAX((Rsv_Spill - Rsv_Add_Cap), 0.)
               end if
            end if
            rsv_Outflow(1,res) = rsv_Outflow(1,res) + Rsv_Spill
            rsv_Outflow(2,res) = rsv_Outflow(2,res) + Rsv_Spill
            Rsv(6,res)         = Rsv_Day_Cap_Act(res)
         else ! if (Rsv_Spill > 0.)
            Rsv(6,res) = Rsv(5,res) + d_Q
         end if ! if (Rsv_Spill > 0.)
      else ! if (d_Q > 0.)
         Rsv(6,res) = Rsv(5,res)
      end if ! if (d_Q > 0.)

   end subroutine RSV_release_discharge_refilling
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   subroutine RSV_withdrawal(res)
   ! calculate direct water withdrawal from reservoir [m^3/d]
   ! (drinking water abstraction, irrigation etc.)
      integer, intent(in) :: res ! reservoir number
!      TYPE (TSubbasin), POINTER :: pS ! water management module, pointer on subbasin

      rsv_act_withdrawal(res) = 0.

      !rsv_Day_Fill_Min = the actual filling (current release already removed)

      if (rsv_Mngmt(res) > 1) then
         rsv_Day_Fill_Min(res) = rsv_Day_Fill_Min(res) - (Rsv(6,res) * rsv_shr_withdr(res))
         rsv_Day_Fill_Min(res) = max(rsv_Day_Fill_Min(res),0.)
      end if

      Res_space = Rsv(6,res) - max(rsv_Day_Fill_Min(res),0.)
      Res_space = max(Res_space,0.)

!       !#################################
!       !#### WATER MANAGEMENT MODULE ####
!       !#################################
!       if ( bWAM_Module ) then
!          ! check if water transfers take place in current subbasin
!          if ( wam_is_transfer_subbasin(sub) ) then
!             pS => wam_get_pointer_subbasin(sub) ! get pointer on current subbasin
!             !!!! overwrite withdrawals from reservoir input file!!!
!             rsv_Withdr_Mon(res,mo) = pS%totDemand(daycounter) ! [m3/s]
!          end if
!       end if
!       !#################################

      if ( Res_space > (rsv_Withdr_Mon(res,mo)*86400.) ) then
         Rsv(7,res) = Rsv(6,res) - (rsv_Withdr_Mon(res,mo)*86400.)
         rsv_act_withdrawal(res) = rsv_Withdr_Mon(res,mo)*86400.
      else
         d_Q = rsv_Outflow(2,res)
         if ( (Res_space + d_Q) < (rsv_Withdr_Mon(res,mo)*86400.) ) then
            rsv_Outflow(1,res) = rsv_Outflow(1,res) - d_Q
            rsv_Outflow(2,res) = rsv_Outflow(2,res) - d_Q
            Rsv(7,res) = Rsv(6,res) - Res_space
            rsv_act_withdrawal(res) = Res_space + d_Q
         else
            d_Q = (rsv_Withdr_Mon(res,mo)*86400.) - Res_space
            rsv_Outflow(1,res) = rsv_Outflow(1,res) - d_Q
            rsv_Outflow(2,res) = rsv_Outflow(2,res) - d_Q
            Rsv(7,res) = Rsv(6,res) - Res_space
            rsv_act_withdrawal(res) = Res_space + d_Q
         end if
      end if

!       !#################################
!       !#### WATER MANAGEMENT MODULE ####
!       !#################################
!       if ( bWAM_Module ) then
!          ! check if water transfers take place in current subbasin
!          if ( wam_is_transfer_subbasin(sub) ) then
!             pS => wam_get_pointer_subbasin(sub) ! get pointer on current subbasin
!             ! assign actual withdrawal to subbasins' water user(s)
!             if ( rsv_act_withdrawal(res) > 0. ) &
!                call wam_distribute_water_from_reservoir(pS,rsv_act_withdrawal(res)) ! [m3]
!          end if
!       end if
!       !#################################

   end subroutine RSV_withdrawal
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   subroutine RSV_mean_water_level(vol_act,vol_dead,res)
   ! calculate water level in [m.a.s.l.]
      integer, intent(in) :: res
      real(8), intent(in) :: vol_act       ! volume of active storage
      real(8), intent(in) :: vol_dead      ! volume of dead storage
      real(8) :: filling

      filling = 0.
      filling = (vol_act + rsv_B(res)) / 2. ! mean of actual storage t=0 and t=-1
      SH = RSV_POL(20,rsv_pol_V(res,:),rsv_pol_L(res,:),filling+vol_dead)
      SH = max(SH,rsv_level_min(res))
      rsv_water_level(res) = min(rsv_level_max(res),SH)
      rsv_height_hpp(res)  = min(rsv_water_level(res)-rsv_level_min(res),rsv_level_hpp(res))

!write(*,*)"SH",SH,"wlevel",rsv_water_level(res),"heighthpp",rsv_height_hpp(res)
   end subroutine RSV_mean_water_level
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   real(8) function RSV_produced_energy(res)
      integer, intent(in) :: res
      real(8) :: release_hpp

      RSV_produced_energy = 0.
      release_hpp = 0.

      release_hpp = min(rsv_Outflow(1,res) / 86400., rsv_cap_hpp(res))
      RSV_produced_energy = rsv_height_hpp(res)*release_hpp*9.81*1.0*rsv_eff_hpp(res)/1000.  ! Efficiency of HPP; included 2015-07-07 (H. Koch)
!     MW                  = H [m] * Q [m³/s] * g [m/s]* ρ [kg/m³] * η [-] / 1000.         
   end function RSV_produced_energy
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   real(8) function RSV_release_firm_energy_yield(res)
      ! Reservoir management option 2
      ! Release for firm energy yield depends on water level.
      ! Returns reservoir release in [m3/d]
      integer, intent(in) :: res      ! reservoir number
      real(8) :: water_level             ! [m.a.s.l.]
      real(8) :: release_hpp ! [m3/s]

      water_level = 0.
      RSV_release_firm_energy_yield = 0.
      release_hpp = 0.
      SH = 0.
      ! water level [m.a.s.l.]
      SH = RSV_POL(20,rsv_pol_V(res,:),rsv_pol_L(res,:),Rsv(4,res)+rsv_dead_stor_act(res))
      water_level = min(rsv_level_max(res),SH)

      ! [m3/s]
      release_hpp = RSV_POL(20,rsv_pol_L2(res,:),rsv_pol_HP(res,:),water_level)

      ! Maximum Release depending on Reservoir filling
      release_hpp = MIN(release_hpp, rsv_cap_hpp(res)) ! [m3/s]

      ! return [m3/d]
      RSV_release_firm_energy_yield = min(release_hpp * 86400., Rsv(4,res) * rsv_Day_ann_cycle(res))

   end function RSV_release_firm_energy_yield
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   real(8) function RSV_release_depending_on_filling(res)
      ! Reservoir management option 3
      ! Release depends on filling.
      integer, intent(in) :: res      ! reservoir number
      real(8) :: water_level             ! [m.a.s.l.]
      real(8) :: release                     ! [m3/d]

      water_level = 0.
      release = 0.
      RSV_release_depending_on_filling = 0.

      ! water level [m.a.s.l.]
      SH = RSV_POL(20,rsv_pol_V(res,:),rsv_pol_L(res,:),Rsv(4,res)+rsv_dead_stor_act(res))
      water_level = min(rsv_level_max(res),SH)

      release = RSV_POL(20,rsv_pol_L2(res,:),rsv_pol_HP(res,:),water_level) ![m3/s]

      ! release [m3/d]
      RSV_release_depending_on_filling = release * 86400.

   end function RSV_release_depending_on_filling
!-----------------------------------------------------------------------

end subroutine RSV_Reservoir_processes
!*****************************************************************************

!-----------------------------------------------------------------------
logical function RSV_is_full_dead_storage(res)
   integer, intent(in) :: res
   RSV_is_full_dead_storage = .true.
   if ( rsv_dead_stor_capac(res) > rsv_dead_stor_act(res) ) &
      RSV_is_full_dead_storage = .false.
end function RSV_is_full_dead_storage
!-----------------------------------------------------------------------

!*****************************************************************************
subroutine RSV_subbasin(ih,in1,in2)
! CALLED FROM: mainpro.f90 during subbasin cycle in routing scheme.
! This is a modified version of the original 'subbasin' subroutine in SWIM.
! It was basically implemented to maintain output statistics.
! NOTE: reservoir subbasin estimates should not be added in subroutine add (route.f90)
   integer, intent(in) :: ih,in1,in2
   integer             :: i,j,res,hru
   real(8)                :: rchrg1 ! = rchrg of previous time step
   real(8)                :: SH, A, ResA_dry, ResA_wet
   real(8)                :: out = 0.

   j = in1
   res = RSV_get_reservoir(j)

!#### INITIALIZATION: call initsub
   call initsub
   aff   = af * flu(j)
!   stemN = 0.

!**** COMPUTE TOTALS for precip:
   precip = subp(j)
   susb(1,j) = susb(1,j) + precip
   sub(1) = sub(1) + precip * flu(j)
   sbp(j) = sbp(j) + precip

!#### COMPUTE snow, recalc precip
!     ATTN: Snow accumulation, melt, and snow evaporation
!        are calculated at hydrotope level!
   snow = 0.
   if (tx(j).le.tsnfall(j)) then
     snow = precip
     sub(2) = sub(2) + precip * flu(j)
     susb(2,j) = susb(2,j) + precip
     precip = 0.
   end if
   susb(3,j) = susb(3,j) + sml
   sub(3) = sub(3) + sml * flu(j)

!#### Hydrotope loop is not called for reservoir subbasins
!#### This means that hydrotope output files are not written if a
!#### reservoir hydrotope was chosen for this subbasin.

   !**** COMPUTE TOTALS for ground water: sub(15...19)
!### sl 2013-11-08
   !call gwmod(j,pd_seepage(res),pd_et(res))
   pd_seepage(res) = max(0.,pd_seepage(res))
   rchrg1 = rchrg(j)
   rchrg(j)  = pd_seepage(res)
   gwseep = pd_seepage(res) * (1. - rsv_gwc(res)) !rchrgc(j)
   rchrg(j) = (1.-delay(j)) * rchrg(j) + delay(j) * rchrg1
   if (rchrg(j) < 1.e-6) rchrg(j) = 0.
   gwchrg = rchrg(j)
   gwq(j) = gwq(j) * abf1(j) + (rchrg(j)) * (1.-abf1(j))

!  Calc revap storage
   revapst(j) = revapst(j) + rchrg(j)

   ! remove seepage to deep aquifer from revap storage (shallow aquifer)
   revapst(j) = revapst(j) - gwseep
   if ( revapst(j) < 0. ) then
      gwseep = gwseep + revapst(j)
      revapst(j) = 0.
   end if

!! remove ground water flow from shallow aquifer storage
   revapst(j) = revapst(j) - gwq(j)
   if ( revapst(j) < 0. ) then
      gwq(j) = gwq(j) + revapst(j)
      revapst(j) = 0.
   end if

!! Ground water contribution is added to varoute(8,) in RSV_Reservoir_processes
   rsv_gwq(res) = max(0.,gwq(j))


!#### values of previous day are being used here !!!
   ! calculate water level [m.a.s.l.] and flooded surface area [km^2]
   SH = 0. ! water level [m.a.s.l.]
   SH = RSV_POL(20,rsv_pol_V(res,:),rsv_pol_L(res,:),Rsv(2,res) + rsv_dead_stor_act(res))
   A = 0.  ! flooded surface area
   A = RSV_POL(20,rsv_pol_L(res,:),rsv_pol_A(res,:),SH)
   ! calculate dry and wet (flooded) reservoir surface area [km^2]
   ResA_dry = rsv_pol_A(res,20) - A
   ResA_dry = max(1.e-6,ResA_dry)
   ResA_wet = A
   ResA_wet = max(1.e-6,ResA_wet)
! sl 2015-08-18
   !rsv_tot_area(res) = (ResA_wet+ResA_dry)*10**6 ! km2 --> m2 total resevoir area

!! pd_outflow is the total outflow from the reservoir in [mm]
!! For surface and subsurface (subbasin) output it must be corrected on ground water contribution
!! Otherwise, some water balance output files are accounting double
!! The half is distributed to surface runoff and the other half to subsurface
! sl 2015-08-18
   !upstream_area = rsv_upstream_area(res)*10**6 - tot_area(res)
   !if ( upstream_area <= 0. ) upstream_area = tot_area(res) ! if reservoir is a headwater
!   out = pd_outflow(res) - rsv_gwq(res) - (rsv_Inflow(res) / upstream_area * 1000.) ! mm
   out = (pd_outflow(res) - rsv_Inflow(res)) / rsv_tot_area(res) * 1000. ! mm
! sl 2015-08-18
!    if (out <= 0.) then
!       !write(*,*) "Outflow from reservoir is negative ! Subroutine: RSV_subbasin"
!       xqd = 0.
!       xssf = 0.
!    else
      xqd  = out * 0.5 ! rsv_frac_sr(res)      ! surface runoff in [mm]
      xssf = out * 0.5 !(1-rsv_frac_sr(res)) ! subsurface runoff in [mm]
!    end if

   qtl = 0.                     ! transmission losses in [mm]
   xeo = pd_et(res)             ! pot. evapotranspiration in [mm]
   xet = xeo                    ! act. evapotranspiration in [mm]

   if ( gis_m > 0 .OR. gis_y > 0 .OR. gis_ave > 0 ) then
      do hru = 1,neap(j) !nbr_hru_sub(j)
         !**** CALC variables for GIS output
         if ( gis_y > 0 .OR. gis_ave > 0 ) then
            presum(j,hru) = presum(j,hru) + precip
            runsum(j,hru) = runsum(j,hru) + xqd + xssf !qd + ssf
            gwrsum(j,hru) = gwrsum(j,hru) + sep
            evasum(j,hru) = evasum(j,hru) + pd_et(res) !ep + es + canev
            petsum(j,hru) = petsum(j,hru) + pd_et(res) !eo
         end if

         ! monthly GIS output
         if ( gis_m > 0 ) then
            presummon(j,hru,mo) = presummon(j,hru,mo) + precip
            runsummon(j,hru,mo) = runsummon(j,hru,mo) + xqd + xssf !qd + ssf
            evasummon(j,hru,mo) = evasummon(j,hru,mo) + pd_et(res) !ep + es + canev
            petsummon(j,hru,mo) = petsummon(j,hru,mo) + pd_et(res) !eo
            gwssummon(j,hru,mo) = gwssummon(j,hru,mo) + sep
            ! Assuming that the soil water index below a reservoir is always 1.0
            swisummon(j,hru,mo) = 1.
            if (precip >= 1.)  npredays01(j,hru,mo) = npredays01(j,hru,mo) + 1
            if (precip >= 5.)  npredays05(j,hru,mo) = npredays05(j,hru,mo) + 1
            if (precip >= 10.) npredays10(j,hru,mo) = npredays10(j,hru,mo) + 1
            if (precip >= 20.) npredays20(j,hru,mo) = npredays20(j,hru,mo) + 1
         end if
         ! monthly GIS output

         !#### CALL HYDRO_GIS
         if ( gis_ave > 0 .AND. ida == 365 ) then
            call hydro_gis(j,hru)
            evamean(j,hru) = evamean(j,hru) + evasum(j,hru)
            pcpmean(j,hru) = pcpmean(j,hru) + presum(j,hru)
            petmean(j,hru) = petmean(j,hru) + petsum(j,hru)
            gwrmean(j,hru) = gwrmean(j,hru) + gwrsum(j,hru)
            if ( iy == nbyr ) call gis_mean(j,hru,nbyr)
         end if
      end do
   end if 
!### sl 2013-11-08

!####

   !**** COMPUTE TOTALS for water: sub(4...13)
!### sl
!! flu(j) replaced by tot_area / da

   sub(4) = sub(4) + tmx(j) * flu(j) ! daily max temp. for subbasin, readcli, degree C
   sub(5) = sub(5) + tmn(j) * flu(j) ! daily min temp. for subbasin, readcli, degree C
   sub(6) = sub(6) + ra(j) * flu(j)  ! solar radiation
   sub(7) = sub(7) + sumcn * flu(j)  ! weighted aver. Cur.Num. in subbasin (SUM(cn))
   susb(8,j) = susb(8,j) + xqd
   sub(8) = sub(8) + xqd * flu(j) 
   smq(j) = smq(j) + xqd
   susb(9,j) = susb(9,j) + xssf
   sub(9) = sub(9) + xssf * flu(j) 
   smsq(j) = smsq(j) + xssf
   susb(10,j) = susb(10,j) + qtl
   sub(10) = sub(10) + qtl * flu(j) 
   revapst(j) = revapst(j) + qtl

   xsep = pd_seepage(res)            ! percolation [mm]
   susb(11,j) = susb(11,j) + xsep
   sub(11) = sub(11) + xsep * flu(j) 
   susb(12,j) = susb(12,j) + xeo     !pot. evapotranspiration
   sub(12) = sub(12) + xeo * flu(j) 
   !  actual is equal to potential evapotranspiration in reservoir subbasin
   susb(13,j) = susb(13,j) + xet
   sub(13) = sub(13) + xet * flu(j) 

   !write(*,*)"seepage mm",(rsv_seepage(res)/(area*10**6))*1000.
   susb(15,j) = susb(15,j) + gwq(j)
   sub(15) = sub(15) + gwq(j) * flu(j) 
   susb(16,j) = susb(16,j) + 0. !revap
   sub(16) = sub(16) + revap  * flu(j) ! revap * tot_area / da
   susb(17,j) = susb(17,j) + gwseep
   sub(17) = sub(17) + gwseep * flu(j) 
   susb(18,j) = susb(18,j) + gwchrg
   sub(18) = sub(18) + gwchrg * flu(j) 
   xswind = 1.
   susb(19,j) = susb(19,j) + xswind
   sub(19) = sub(19) + xswind * flu(j) 
   xxswind = xxswind + xswind * flu(j) 

   !**** COMPUTE TOTALS for water yield (wysb) & sediments: sub(20...25)
   !wysb = xqi + xssf + gwq(j) - qtl
   !wysb = (pd_outflow(res) + gwq(j)) ! - (rsv_Inflow(res)*1000./(rsv_pol_A(res,20)*10**6)) ! [mm]
! sl 2015-08-18
   !wysb = pd_outflow(res)
   ! wysb can be negative if inflow is larger than outflow + gwq
   wysb = pd_outflow(res)/rsv_tot_area(res)*1000. + rsv_gwq(res) - qtl
   !wysb = max(wysb,0.)

   susb(20,j) = susb(20,j) + wysb
   sub(20) = sub(20) + wysb * flu(j) 
!Todo: sediment
!Todo: water quality parameters
!    sub(21) = sub(21) + yd/(100.*da*tot_area / da)
!    sym(j) = sym(j) + yd
!    susb(21,inum1) = susb(21,inum1) + yd/(100.*da*tot_area / da)
!    sub(22) = sub(22) + yon * tot_area / da
!    susb(22,inum1) = susb(22,inum1) + yon
!    sub(23) = sub(23) + yph * tot_area / da
!    susb(23,inum1) = susb(23,inum1) + yph
!    sub(24) = sub(24) + ysp * tot_area / da
!    susb(24,inum1) = susb(24,inum1) + ysp


   !**** COMPUTE SUBBASIN OUTPUTS FOR ROUTING: sda(), varoute()
   !     ATTN: sda(6,j) = sum of 3 fluxes after retention, new version
   !     ATTN: coef (dart()*1000) to transform from mm to m3 (42,48)
   !     ATTN: coef (dart()*100)  to transform kg/ha to kg (44-47)
   !     ATTN: wysb(mm)*dart(km2)*1000 = wysb*dart*1000 (m3)
   !     ATTN: sda(2,j), sda(8,j) in m3
   !     ATTN: xyno3 in kg/ha, sda(6) & varoute(6,.) in kg

   xqi = xqd !pd_outflow(res) * 0.5 ! rsv_frac_sr(res) ! [mm]
   yd = 0.
   yon = 0.
   yph = 0.
   xyno3 = 0.
   xssfn = 0.
   xpercn = 0.
   xysp = 0.
!  * dart * 1000. replaced by tot_area/1000.
   !sda(2,j) = xqi * dart(j) * 1000.
! sl 2015-08-18
   !sda(2,j) = xqi * rsv_tot_area(res) / 1000.
   sda(2,j) = pd_outflow(res)*0.5 / rsv_tot_area(res) * 1000.

   sda(3,j) = yd
   sda(4,j) = yon * dart(j) * 100
   sda(5,j) = yph * dart(j) * 100
   sda(6,j) = (xyno3 + xssfn + xpercn) * dart(j) * 100
   sda(7,j) = xysp * dart(j) * 100
! sl 2015-08-18
   !sda(8,j) = (wysb - xqi) * rsv_tot_area(res) / 1000.
   sda(8,j) = (wysb - pd_outflow(res)*0.5) / rsv_tot_area(res) * 1000.
   xwysb = xwysb + wysb / rsv_tot_area(res) * 1000.

   do i = 1, 8
     varoute(i,j) = sda(i,j)
   end do

   !**** WRITE daily water outputs for ALL subbasins: FILE 61 - sbd.out
   if ( bAllSubbasinsDaily ) then
      if (ida.eq.1.and.j.eq.1.and.iy.eq.1) write(61,128)
      if (iy.eq.1) write(61,115) iyr-1900,ida,j,precip,tx(j),xqd,xssf,xsep,xeo,xet,wysb
   end if
   
   !**** WRITE daily water outputs for selected subbasins: FILES 63,64,65,66,67:
   if ( j.eq.isu1 ) then
      if ( ida == 1 .AND. iy==1 ) write(63,129)
      write(63,121) iyr,ida,j,precip,xqd,xssf,xsep,gwq(j),gwseep,xeo,xet,wysb,xswind
   end if
   if ( j.eq.isu2 ) then
      if ( ida == 1 .AND. iy==1 ) write(64,129)
      write(64,121) iyr,ida,j,precip,xqd,xssf,xsep,gwq(j),gwseep,xeo,xet,wysb,xswind
   end if
   if ( j.eq.isu3 ) then
      if ( ida == 1 .AND. iy==1 ) write(65,129)
      write(65,121) iyr,ida,j,precip,xqd,xssf,xsep,gwq(j),gwseep,xeo,xet,wysb,xswind
   end if
   if ( j.eq.isu4 ) then
      if ( ida == 1 .AND. iy==1 ) write(66,129)
      write(66,121) iyr,ida,j,precip,xqd,xssf,xsep,gwq(j),gwseep,xeo,xet,wysb,xswind
   end if
   if ( j.eq.isu5 ) then
      if ( ida == 1 .AND. iy==1 ) write(67,129)
      write(67,121) iyr,ida,j,precip,xqd,xssf,xsep,gwq(j),gwseep,xeo,xet,wysb,xswind
   end if

  115 format(3i4,12f8.2)
  121 format(i5,i4,i6,20f8.3)
  128 format(/,'  YR DAY SUB  PRECIP    TEMP    SURQ    SUBQ    PERC',5X,'PET     AET    WYLD')
  129 format('YEAR  DAY   SUB   PRECIP   SURQ    SUBQ    PERC     GWQ  GWSEEP     PET     AET    WYLD   SWIND')

end subroutine RSV_subbasin
!*****************************************************************************


!*****************************************************************************
real(8) function RSV_POL(k,xp,yp,xps)
!****  Interpolation using function POLygon  ***
   integer, intent(in) :: k
   real(8), intent(in) :: xp(k),yp(k),xps
   real(8)             :: x0,xt
   integer             :: i,ih

   RSV_POL = 0.
   x0 = xps
   if (x0 < xp(1)) x0 = xp(1)
   do i = 1, k
      ih = i
      xt = ABS (xp(i)-x0)
      if (xt <= 1.E-6) then
         RSV_POL = yp(ih)
         exit
      end if
      if (x0 <= xp(i)) then
         RSV_POL = yp(i-1) + (yp(i)-yp(i-1))*(x0-xp(i-1))/(xp(i)-xp(i-1))
         exit
      end if
   end do
end function RSV_POL
!*****************************************************************************
!*****************************************************************************


!#############################################################################
!
! READ INPUT FILES
!
!#############################################################################


!*****************************************************************************
subroutine RSV_read_reservoir_control_file ! reservoir.ctrl
   character(len=1)                   :: header
   integer                            :: readsucc,i
   integer, dimension(:), allocatable :: act
   character(len=100)                 :: file

   file = trim(inputPath)//trim(rsv_Reservoir_CTRL_file)
   if ( open_file(400,file) ) then
      write(*,*)"OK"
      read(400,*,IOSTAT=readsucc) header
      read(400,*,IOSTAT=readsucc) rsv_nReservoirs
      allocate(rsv_ResSubbasins(rsv_nReservoirs))
      allocate(act(rsv_nReservoirs))
      rsv_ResSubbasins = 0
      read(400,*,IOSTAT=readsucc) (rsv_ResSubbasins(i), i=1,rsv_nReservoirs)

      call RSV_allocate_reservoir

      read(400,*,IOSTAT=readsucc) (rsv_ResNames(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_Capac_Max(i), i=1,rsv_nReservoirs)
      rsv_Capac_Max = rsv_Capac_Max * 10**6 ! convert [million m^3] into [m^3]
      read(400,*,IOSTAT=readsucc) (rsv_dead_stor_capac(i), i=1,rsv_nReservoirs)
      rsv_dead_stor_capac = rsv_dead_stor_capac * 10**6 ! convert [million m^3] into [m^3]
      read(400,*,IOSTAT=readsucc) (rsv_Start_Fill(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (act(i), i=1,rsv_nReservoirs)
!      read(400,*,IOSTAT=readsucc) (rsv_active(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_activate_thresh(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_level_max(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_level_min(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_level_hpp(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_cap_hpp(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_eff_hpp(i), i=1,rsv_nReservoirs) !Efficiency of HPP; included 2015-07-07 (H. Koch)      
      read(400,*,IOSTAT=readsucc) (rsv_loss_seepage(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_gwc(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_evapc(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_start_year(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_start_day(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_Mngmt(i), i=1,rsv_nReservoirs)
      read(400,*,IOSTAT=readsucc) (rsv_shr_withdr(i), i=1,rsv_nReservoirs)

      if ( readsucc /= 0 ) then
         write(*,*) "ERROR while reading file: ", trim(inputPath)//trim(rsv_Reservoir_CTRL_file)
         write(*,*) "SWIM stops!!!"
         STOP
      end if
   else
      write(*,*) "ERROR while opening file: ", trim(inputPath)//trim(rsv_Reservoir_CTRL_file), " does it exist?"
      write(*,*) "SWIM stops!!!"
      STOP
   end if
   close(400)

   ! assign rsv_active
   do i = 1, rsv_nReservoirs
      if ( act(i) > 0 ) rsv_active(i) = .true.
   end do

end subroutine RSV_read_reservoir_control_file
!*****************************************************************************

!*****************************************************************************
subroutine RSV_read_reservoir_month_file ! reservoir_monthly.csv
   character(len=1) :: header
   integer          :: readsucc,i,j,k
   character(len=100) :: file

   file = trim(inputPath)//trim(rsv_Reservoir_month_file)

   if ( open_file(400,file) ) then
      write(*,*)"OK"
      read(400,*,IOSTAT=readsucc) header
      do i=1,rsv_nReservoirs
         read(400,*,IOSTAT=readsucc) (rsv_Cap_Act(i,j), j=1,12)
         rsv_Cap_Act(i,0) = rsv_Cap_Act(i,12) ! index 0 = December value
         do k=0,12
            if ( rsv_Cap_Act(i,k) > rsv_Capac_Max(i) ) then
               rsv_Cap_Act(i,k) = rsv_Capac_Max(i)
               write(*,*) "#### Reservoir module: storage capacity corrected ####"
               write(*,*) "Reservoir:",i,"Month:",k
            end if
         end do
         read(400,*,IOSTAT=readsucc) (rsv_Fill_Min(i,j), j=1,12)
         rsv_Fill_Min(i,0) = rsv_Fill_Min(i,12) ! index 0 = December value
         read(400,*,IOSTAT=readsucc) (rsv_Dis_Min_Fill(i,j), j=1,12)
         rsv_Dis_Min_Fill(i,0) = rsv_Dis_Min_Fill(i,12) ! index 0 = December value
         read(400,*,IOSTAT=readsucc) (rsv_Dis_Min_Act(i,j), j=1,12)
         rsv_Dis_Min_Act(i,0) = rsv_Dis_Min_Act(i,12) ! index 0 = December value
         read(400,*,IOSTAT=readsucc) (rsv_Withdr_Mon(i,j), j=1,12)
         rsv_Withdr_Mon(i,0) = rsv_Withdr_Mon(i,12) ! index 0 = December value
         read(400,*,IOSTAT=readsucc) (rsv_ann_cycle(i,j), j=1,12)
         rsv_ann_cycle(i,0) = rsv_ann_cycle(i,12) ! index 0 = December value
      end do

      ! convert million m^3 to m^3
      rsv_Cap_Act  = rsv_Cap_Act  * 10**6
      rsv_Fill_Min = rsv_Fill_Min * 10**6

      if ( readsucc /= 0 ) then
         write(*,*) "ERROR while reading file: ", trim(inputPath)//trim(rsv_Reservoir_month_file)
         write(*,*) "SWIM stops!!!"
         STOP
      end if
   else
      write(*,*) "ERROR while opening file: ", trim(inputPath)//trim(rsv_Reservoir_month_file), " does it exist?"
      write(*,*) "SWIM stops!!!"
      STOP
   end if
   close(400)

end subroutine RSV_read_reservoir_month_file
!*****************************************************************************

!*****************************************************************************
subroutine RSV_read_reservoir_stor_file ! reservoir_storage_conf.csv
   character(len=1) :: header
   integer          :: readsucc,i,j
   character(len=100) :: file

   file = trim(inputPath)//trim(rsv_Reservoir_stor_file)

   if ( open_file(400,file) ) then
      write(*,*)"OK"
      read(400,*,IOSTAT=readsucc) header
      do i=1,rsv_nReservoirs
         read(400,*,IOSTAT=readsucc) (rsv_pol_L(i,j),  j=1,20)
         read(400,*,IOSTAT=readsucc) (rsv_pol_L2(i,j), j=1,20)
         read(400,*,IOSTAT=readsucc) (rsv_pol_A(i,j),  j=1,20)
         read(400,*,IOSTAT=readsucc) (rsv_pol_V(i,j),  j=1,20)
         read(400,*,IOSTAT=readsucc) (rsv_pol_HP(i,j), j=1,20)
      end do

      ! convert million m^3 to m^3
      rsv_pol_V = rsv_pol_V * 10**6 ! convert [million m^3] into [m^3]

      if ( readsucc /= 0 ) then
         write(*,*) "ERROR while reading file: ", trim(inputPath)//trim(rsv_Reservoir_stor_file)
         write(*,*) "SWIM stops!!!"
         STOP
      end if
   else
      write(*,*) "ERROR while opening file: ", trim(inputPath)//trim(rsv_Reservoir_stor_file), " does it exist?"
      write(*,*) "SWIM stops!!!"
      STOP
   end if
   close(400)

end subroutine RSV_read_reservoir_stor_file
!*****************************************************************************

!*****************************************************************************
subroutine RSV_init_rsvSubbasin
! rsvSubbasin(subtot) is initialised with 0 in RSV_allocate_reservoir
! here the reservoir subbasins will stored at the corresponding array indices
! if rsvSubbasin() = 0, subbasin is no reservoir
! if rsvSubbasin() /= 0, subbasin is reservoir
   integer i

   do i=1,rsv_nReservoirs
      rsvSubbasin(rsv_ResSubbasins(i)) = rsv_ResSubbasins(i)
   end do
end subroutine RSV_init_rsvSubbasin
!*****************************************************************************

!*****************************************************************************
subroutine RSV_routing
! reading the routing structure in order to identify the hydrograph storage locations (ihout)
! that are inputs to the reservoir subbasins
   integer :: i

   do i = 1, mhyd
      if (icodes(i) > 0) then
         select case (icodes(i))
            case (1)      ! SUBBASIN command
               ! do nothing

            case (2)      ! ROUTE command
               if ( inum1s(i) <= mb ) then
                  if ( rsvSubbasin(inum1s(i))  > 0 ) &
                     bRsvHydrograph(i) = .true.
               end if
            case (3) ! not implemented
               ! do nothing

            case (4) ! not implemented
               ! do nothing

            case (5)      ! ADD command
               !call indAdd(ihout,inum1,inum2)

            case default ! do nothing
         end select
      end if
   end do
end subroutine RSV_routing
!*****************************************************************************

!*****************************************************************************
integer function RSV_get_reservoir(sub)
   integer, intent(in) :: sub
   integer             :: i

   RSV_get_reservoir = 0
   do i = 1, rsv_nReservoirs
      if ( rsv_ResSubbasins(i) == sub ) RSV_get_reservoir = i
   end do

end function RSV_get_reservoir
!*****************************************************************************

!*****************************************************************************
real(8) function ET_Turc(sub)
! calculates potential evapotranspiration in [mm]
! Turc-Ivanov method
   integer, intent(in)  :: sub
   ET_Turc = 0.

   ! #### CALCULATE EVAPORATION from water surface
   ! TURC-IVANOV POTENTIAL EVAP
   if (tx(sub).ge.5.) then
      ET_Turc = 0.0031 * omega(mo) * (ra(sub)+209.4) * (tx(sub)/(tx(sub)+15.))
   else
      if ( humi(sub) < 0. ) then
         ET_Turc = 0.000036 * (25.+tx(sub))**2. * 35.
      else
         ET_Turc = 0.000036 * (25.+tx(sub))**2. * (100.-humi(sub))
      end if
   end if

end function ET_Turc
!*****************************************************************************

!*****************************************************************************
logical function open_file(funit,fname)
! This function opens a file with given file unit and name and returns true,
! if file open was successful. The file status remains open!
! If file opening was not successful, the functions returns false and closes the file.
   integer, intent(in)            :: funit
   character(len=100), intent(in) :: fname
   integer                        :: opensucc

   open_file = .true.
   open(funit,file=trim(fname),status='OLD',IOSTAT=opensucc)
   write(*,*) "OPEN FILE:", trim(fname),"... "
   if ( opensucc /= 0 ) then
      open_file = .false.
      close(funit)
   end if

end function open_file
!*****************************************************************************

!#############################################################################
!
! ALLOCATE / DEALLOCATE ARRAYS
!
!#############################################################################

!*****************************************************************************
subroutine RSV_allocate_reservoir

   allocate(bRsvHydrograph(mhyd))
   bRsvHydrograph = .false.
   allocate(rsvSubbasin(mb))
   rsvSubbasin = 0

   allocate(rsv_funit(rsv_nReservoirs))
   rsv_funit = 0

! allocate all reservoir module-specific arrays

   allocate(Rsv(10,rsv_nReservoirs))
   Rsv = 0.
   
   allocate(rsv_dead_stor_act(rsv_nReservoirs))
   rsv_dead_stor_act = 0.

   allocate(rsv_Inflow(rsv_nReservoirs))
   rsv_Inflow = 1.e-6
   allocate(rsv_frac_sr(rsv_nReservoirs))
   rsv_frac_sr = 0.
   allocate(rsv_B(rsv_nReservoirs))
   rsv_B = 0.
   allocate(rsv_seepage(rsv_nReservoirs))
   rsv_seepage = 1.e-6
   allocate(rsv_Outflow(2,rsv_nReservoirs))
   rsv_Outflow = 1.e-6
   allocate(rsv_gwq(rsv_nReservoirs))
   rsv_gwq = 1.e-6

   allocate(rsv_Day_Cap_Act(rsv_nReservoirs))
   rsv_Day_Cap_Act = 0.
   allocate(rsv_Day_Fill_Min(rsv_nReservoirs))
   rsv_Day_Fill_Min = 0.
   allocate(rsv_Day_ann_cycle(rsv_nReservoirs))
   rsv_Day_ann_cycle = 0.

   allocate(rsv_Day_Disch_Min(rsv_nReservoirs))
   rsv_Day_Disch_Min = 0.

   allocate(rsv_act_withdrawal(rsv_nReservoirs))
   rsv_act_withdrawal = 0.

   allocate(rsv_water_level(rsv_nReservoirs))
   rsv_water_level = 0.
   allocate(rsv_height_hpp(rsv_nReservoirs))
   rsv_height_hpp = 0.

   allocate(rsv_Prod_HPP(rsv_nReservoirs))
   rsv_Prod_HPP = 0.

   allocate(rsv_tot_area(rsv_nReservoirs))
   rsv_tot_area = 0.

! reservoir parameter, read from reservoir.ctrl
! dimension(rsv_nReservoirs)
   allocate(rsv_ResNames(rsv_nReservoirs))
   rsv_ResNames = ''
   allocate(rsv_Capac_Max(rsv_nReservoirs))
   rsv_Capac_Max = 0.
   allocate(rsv_dead_stor_capac(rsv_nReservoirs))
   rsv_dead_stor_capac = 0.
   allocate(rsv_Start_Fill(rsv_nReservoirs))
   rsv_Start_Fill = 0.
   allocate(rsv_active(rsv_nReservoirs))
   rsv_active = .false.
   allocate(rsv_activate_thresh(rsv_nReservoirs))
   rsv_activate_thresh = 0.
   allocate(rsv_level_max(rsv_nReservoirs))
   rsv_level_max = 0.
   allocate(rsv_level_min(rsv_nReservoirs))
   rsv_level_min = 0.
   allocate(rsv_level_hpp(rsv_nReservoirs))
   rsv_level_hpp = 0.
   allocate(rsv_cap_hpp(rsv_nReservoirs))
   rsv_cap_hpp = 0.
   allocate(rsv_eff_hpp(rsv_nReservoirs))
   rsv_eff_hpp = 0.
   allocate(rsv_loss_seepage(rsv_nReservoirs))
   rsv_loss_seepage = 0.
   allocate(rsv_gwc(rsv_nReservoirs))
   rsv_gwc = 0.
   allocate(rsv_evapc(rsv_nReservoirs))
   rsv_evapc = 0.
   allocate(rsv_start_year(rsv_nReservoirs))
   rsv_start_year = 0
   allocate(rsv_start_day(rsv_nReservoirs))
   rsv_start_day = 0
   allocate(rsv_Mngmt(rsv_nReservoirs))
   rsv_Mngmt = 0
   allocate(rsv_shr_withdr(rsv_nReservoirs))
   rsv_shr_withdr = 0.

! monthly reservoir data, read from 'reservoir_monthly.csv'
! dimension(rsv_nReservoirs,0:12) 1-12 = Jan-Dec; 0 = Dec (easier to apply month-1)
   allocate(rsv_Cap_Act(rsv_nReservoirs,0:12))
   rsv_Cap_Act = 0.
   allocate(rsv_Fill_Min(rsv_nReservoirs,0:12))
   rsv_Fill_Min = 0.
   allocate(rsv_Dis_Min_Fill(rsv_nReservoirs,0:12))
   rsv_Dis_Min_Fill = 0.
   allocate(rsv_Dis_Min_Act(rsv_nReservoirs,0:12))
   rsv_Dis_Min_Act = 0.
   allocate(rsv_Withdr_Mon(rsv_nReservoirs,0:12))
   rsv_Withdr_Mon = 0.
   allocate(rsv_ann_cycle(rsv_nReservoirs,0:12))
   rsv_ann_cycle = 0.
   allocate(rsv_Disch_Min(rsv_nReservoirs,0:12))
   rsv_Disch_Min = 0.
! storage parameters, read from 'reservoir_storage_conf.csv'
! dimension(rsv_nReservoirs,20)
   allocate(rsv_pol_L(rsv_nReservoirs,20))
   rsv_pol_L = 0.
   allocate(rsv_pol_L2(rsv_nReservoirs,20))
   rsv_pol_L2 = 0.
   allocate(rsv_pol_A(rsv_nReservoirs,20))
   rsv_pol_A = 0.
   allocate(rsv_pol_V(rsv_nReservoirs,20))
   rsv_pol_V = 0.
   allocate(rsv_pol_HP(rsv_nReservoirs,20))
   rsv_pol_HP = 0.

! previous day arrays
   allocate(pd_outflow(rsv_nReservoirs))
   pd_outflow = 1.e-6
   allocate(pd_seepage(rsv_nReservoirs))
   pd_seepage = 1.e-6
   allocate(pd_area_wet(rsv_nReservoirs))
   pd_area_wet = 1.e-6
   allocate(pd_et(rsv_nReservoirs))
   pd_et = 1.e-6
   allocate(pd_gwq(rsv_nReservoirs))
   pd_gwq = 1.e-6
   allocate(pd_gwseep(rsv_nReservoirs))
   pd_gwseep = 1.e-6
   allocate(pd_gwchrg(rsv_nReservoirs))
   pd_gwchrg = 1.e-6
   allocate(pd_wysb(rsv_nReservoirs))
   pd_wysb = 1.e-6

end subroutine RSV_allocate_reservoir
!*****************************************************************************

!*****************************************************************************
subroutine RSV_deallocate_reservoir
! should be called by main program at the end of the simulation
   integer :: res

   ! close reservoir output files
   do res = 1, rsv_nReservoirs
      close(rsv_funit(res))
   end do

   deallocate(bRsvHydrograph)
   deallocate(rsvSubbasin)
   deallocate(rsv_funit)

   deallocate(rsv_dead_stor_act)
   deallocate(rsv_Inflow)
   deallocate(rsv_frac_sr)
   deallocate(rsv_B)
   deallocate(Rsv)
   deallocate(rsv_seepage)
   deallocate(rsv_Outflow)
   deallocate(rsv_gwq)

   deallocate(rsv_Day_Cap_Act)
   deallocate(rsv_Day_Fill_Min)
   deallocate(rsv_Day_ann_cycle)

   deallocate(rsv_Day_Disch_Min)
   deallocate(rsv_act_withdrawal)
   deallocate(rsv_water_level)
   deallocate(rsv_height_hpp)
   deallocate(rsv_Prod_HPP)
   deallocate(rsv_tot_area)

   deallocate(rsv_ResSubbasins)

   deallocate(rsv_ResNames)
   deallocate(rsv_Capac_Max)
   deallocate(rsv_dead_stor_capac)
   deallocate(rsv_Start_Fill)
   deallocate(rsv_active)
   deallocate(rsv_activate_thresh)
   deallocate(rsv_level_max)
   deallocate(rsv_level_min)
   deallocate(rsv_level_hpp)
   deallocate(rsv_cap_hpp)
   deallocate(rsv_eff_hpp)
   deallocate(rsv_loss_seepage)
   deallocate(rsv_gwc)
   deallocate(rsv_evapc)
   deallocate(rsv_start_year)
   deallocate(rsv_start_day)
   deallocate(rsv_Mngmt)
   deallocate(rsv_shr_withdr)

   deallocate(rsv_Cap_Act)
   deallocate(rsv_Fill_Min)
   deallocate(rsv_Dis_Min_Fill)
   deallocate(rsv_Dis_Min_Act)
   deallocate(rsv_Withdr_Mon)
   deallocate(rsv_ann_cycle)
   deallocate(rsv_Disch_Min)

   deallocate(rsv_pol_L)
   deallocate(rsv_pol_L2)
   deallocate(rsv_pol_A)
   deallocate(rsv_pol_V)
   deallocate(rsv_pol_HP)

! previous day arrays
   deallocate(pd_outflow)
   deallocate(pd_seepage)
   deallocate(pd_area_wet)
   deallocate(pd_et)
   deallocate(pd_gwq)
   deallocate(pd_gwseep)
   deallocate(pd_gwchrg)
   deallocate(pd_wysb)

end subroutine RSV_deallocate_reservoir
!*****************************************************************************


!#############################################################################
end module mod_reservoir
!#############################################################################