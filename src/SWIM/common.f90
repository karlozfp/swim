!     FILE common.f
!

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! INDICES:
!      i, ida - day
!      j      - subbasin
!      jea,je - hydrotope or HRU
!      k      - soil type
!      n      - land use type
!      l      - soil layers
!
! PHASE VARIABLES & FLUXES:
! HYDROLOGY:
!      swe(mb,meap)      = soil water content, mm
!      ste(mb,meap,ml)   = water storage in a layer, mm
!      snoa(mb,meap)     = snow water content, mm
!      flate(mb,meap,ml) = lateral subsurface flow for layer, mm
!      poe(mb,meap,ml)   = percolation for layer, mm
! CROP:
!      cva(mb,meap)      = vegetation cover, kg/ha
!      dm(mb,meap)       = total biomass, kg/ha
!      rsd(mb,meap)      = crop residue, kg/ha
! NUTRIENTS:
!      ano3(mb,meap,ml)  = nitrate (NO3-N) content in a layer, kg/ha
!      anora(mb,meap,ml) = active org. N content in a layer, kg/ha
!      anors(mb,meap,ml) = stable org. N content in a layer, kg/ha
!      plab(mb,meap,ml)  = labile P content in a layer, kg/ha
!      porg(mb,meap,ml)  = organic P in a layer, kg/ha
!      pma(mb,meap,ml)   = active mineral P in a layer, kg/ha
!      pms(mb,meap,ml)   = stable mineral P in a layer, kg/ha
!      fop(mb,meap)      = fresh org N, kg/ha
!      fon(mb,meap)      = fresh org P, kg/ha
! OTHER IMPORTANT VARIABLES:
!      mstruc(mb,meap,4) = HRU structure vector. land use, soil, wet, manag
!      frar(mb,meap)     = fractional area of hydrotope in subbasin
!      te(mb,meap,ml)    = daily ave temp at the bottom of layer, degree !
!      te0(mb,meap)      = bare soil surface temp, degree !
!      smx(mb,meap)      = retention factor, corresponding cn1
!      wf(2,mb,meap)     = shape parameters for calc. of retention
!      igro(mb,meap)     = vegetation index, =1 if vegetation is growing
!      g(mb,meap)        = fraction of heat units to maturity accumulated
!      alai(mb,meap)     = leaf area index
!      rwt(mb,meap)      = fraction of root weight
!      hia(mb,meap)      = harvest index
!      hiad(mb,meap)     = harvest index, adjusted
!      canstor(mb,meap)  = canopy water storage, mm
!      ws(mb,meap)       = water stress factor
!      huharv(mb,meap)   = harvest index heat unit
!      rd(mb,meap)       = root depth, mm
!      yld(mb,meap)      = crop yield, kg/ha
!      cklsp(mb,meap)    = combined c,k,ls,p factor
!      snup(mb,meap)     = N uptake, kg/ha
!      spup(mb,meap)     = P uptake, kg/ha
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
module common_par

! from module SwimEngine
   integer, save                                :: currcrop = 1       ! crop used for computation (default winter wheat)
   integer, save                                :: ndsum = 0          ! number of days
   integer, save                                :: nd = 0             ! number of days in the current year
   character(len=256), save                     :: configPath         ! 
   character(len=256), save                     :: inputPath          ! 
   character(len=256), save                     :: swimPath           ! 
   character(len=256), save                     :: climPath           ! 
   character(len=256), save                     :: hydrotopePath      ! 

!**** COMMON PARAMETERS
   integer, save                                :: crop_rot           ! 1 - with crop-rotation
   character(len=5)                             :: rot_variant        ! rotated crop (po,ma,sb,wr,wb,wwII,wrape,ww) see initcrp_xx.f

   ! land management controlled by external input file
   character(len=13), save                      :: landmgtdat = 'landmgt.csv' ! file containing land management operations
   integer, save                                :: iyrrot = 0         ! current rotation year (land management, 1-3)
   integer, parameter                           :: nrotyrs = 3        ! number of rotation years
   integer, save                                :: mgt_tot = 0        ! total number of management operation years (lines in landman.csv)
   integer, save, dimension(:), allocatable     :: mgt_id             ! management IDs in landman.csv
   integer, save, dimension(:), allocatable     :: mgt_yr             ! management/rotation year in landman.csv
   integer, save, dimension(:), allocatable     :: mgt_nop            ! number of management operations during one year
   integer, save, dimension(:), allocatable     :: mgt_lu_id          ! land use id read from landman.csv
   integer, save, dimension(:,:), allocatable   :: mgt_idop         ! day of operation read from landman.csv
   integer, save, dimension(:,:), allocatable   :: mgt_iopc         ! operation code: 1 - planting, 2 - harvest & kill read from landman.csv
   integer, save, dimension(:,:), allocatable   :: mgt_ncrp         ! crop number read from landman.csv
   integer, save, dimension(:,:), allocatable   :: mgt_idfe         ! day of fertilization read from landman.csv
   real(8),    save, dimension(:,:), allocatable:: mgt_fen          ! N fertilizer kg/ha read from landman.csv
   real(8),    save, dimension(:,:), allocatable:: mgt_feno         ! org N fertilizer kg/ha read from landman.csv
   real(8),    save, dimension(:,:), allocatable:: mgt_fep          ! P fertilizer kg/ha read from landman.csv
   integer, save                                :: cur_nop = 0      ! Number of land management operations of current hydrotop


!**** cod-file parameter
   integer, save                                :: nbyr               ! number of years of simulations read from *.cod file
   integer, save                                :: iyr                ! current years (YYYY) real year
   integer, save                                :: idaf               ! first day
   integer, save                                :: idal               ! last day
   integer, save                                :: mb                 ! total number of subbasins
   integer, save                                :: numht              ! total number of hydrotopes

   integer, save                                :: meap               ! max number of HRUs in subbasin (counted)
   integer, save                                :: mch                ! mch   = max number of channels
   integer, save                                :: mhyd               ! mhyd  = max number of hydrograph nodes

   integer, parameter                           :: ml = 10            ! ml    = max number of soil layers
   integer, parameter                           :: mc = 15            ! max number of land use types
   integer, save                                :: mcrdb = 0          ! number of crops in crop.dat
   integer, save                                :: msdb = 0           ! msdb  = max number of soils in database (used in readsol), counted during runtime 
   integer, save                                :: ms = 0             ! total number of soils used (ms), counted during runtime
   integer, parameter                           :: mop = 7            ! mop   = max number of agric. operations within one year
   integer, parameter                           :: mfe = 7            ! mfe   = max number of fertilizations
!**** variables
   integer, save                                :: ida = 0            ! current day
   integer, save                                :: iy = 0             ! current year starting with 1
   integer, save                                :: mo = 0             ! current month
   integer, save                                :: nt = 0             ! ??

!**** Output variables
   ! variables for routed output of selected subbasins
   integer, save                                :: ngaugesout = 0     ! number of subbasins (reaches) used for discharge output (input file: gauges.output)
   integer, save, dimension(:), allocatable     :: gaugesout          ! subbasins (reaches) numbers used for discharge output (input file: gauges.output)
   integer, save, dimension(:), allocatable     :: gaugesouthyd       ! hydrograph storage location for subbasins (reaches) numbers used for discharge output (input file: gauges.output)
   character(len=80), dimension(:), allocatable :: gaugesout_names    ! names of output gauges to be written in Q_gauges.csv
   real(8), save, dimension(:), allocatable     :: gaugesout_runoff   ! runoff for current day and gauge

   real(8), save, dimension(:), allocatable     :: runoff_mm          ! only used when CaMaFLood is activated

   logical                                      :: bAllSubbasinsOut   ! in .cod file write Q of all subbasins to output file or not
   logical                                      :: bAllSubbasinsDaily ! write subd.prn
   logical                                      :: bAllSubbasinsMonthly ! write subm.prn
   logical, save                                :: bCamaFlood ! generate output files for the cama flood model
   integer, save, dimension(:), allocatable     :: subouthyd          ! hydrograph storage location for each subbasin
   integer, save, dimension(:), allocatable     :: subs               ! array containing subbasin list
   real(8), save, dimension(:), allocatable     :: runsub_m3s         ! daily routed and added discharge per subbasin
   

   real(8), save                                :: run1,run2,run3     ! current discharge for rch1,rch2,rch3 (in route.f)
   integer, save                                :: isb1, ih1          ! Hydrotope output isb=basin number, ih=hydrotop number
   integer, save                                :: isb2, ih2
   integer, save                                :: isb3, ih3
   integer, save                                :: isb4, ih4
   integer, save                                :: isb5, ih5
   integer, save                                :: isb6, ih6
   integer, save                                :: isb7, ih7
   real(8), save, dimension(7,9)                :: htpmon             ! monthly output (9 parameters) of 7 selected hydrotopes ('project'.cod)

					        
!**** Additional parameters in *.bsn file
   real(8), save                                :: vegperiod          ! 0...? determines vegetation period (0=all year long, ~1=temperate zone)
   integer, save                                :: subtrop            ! 0 = off; 1 = active. accounts for subtropical vegetation, used in veget.f90

!**** *.cod file
   integer, save                                :: isu1,isu2,isu3,isu4,isu5 ! subbasin numbers for output
!**** File names and path names
   character(len=13), save                      :: routin
   character(len=13), save                      :: struct
   
!**** Parameters - dif. constants
   real(8), parameter                           :: ub  = 3.065        ! water use rate-depth parameter, used in wstress
   real(8), parameter                           :: ab  = 0.02083      ! parameter for calc of precip alpha factor in alpha
   real(8), parameter                           :: pit = 58.13        ! parameter used to calc day length in crop
   real(8), parameter                           :: psp = 0.5          ! parameter used to calc P availability index in pcycle
   real(8), parameter                           :: rtn = 0.15         ! active N pool fraction
!cls   real(8), parameter                       :: clt = 57.296       ! conversion factor used in readwet (1 rad = 57.296 deg)

!**** Dimension indices for writgen & main
!     nns = 107, max number of variables for output
   integer, parameter                           :: nvrch = 18
   integer, parameter                           :: nvsub = 30
   integer, parameter                           :: nsb   = 30
   integer, parameter                           :: nns   = 30
					        
!**** Parameters for sc estimation in readsol()
   real(8), save, dimension(17)                    :: bsc = (/-8.96847, 0., -0.028212, 19.52348, 0.00018107, &
                                                      -0.0094125, -8.395215, 0., 0.077718, 0., 0.0000173, 0.02733, &
                                                      0.001434, -0.0000035, 0., -0.00298, -0.019492/)

   character(len=4)                             :: prog
   character(len=8)                             :: till
					        
   !! array storing monthly Turc-Ivanov factors
   !! read in subroutine: assign_turc_ivanov(turc_ivanov) in file: evap.f90
   real(8), save, dimension(12)                 :: turc_ivanov = 1.

   !**** Number of julian days passed to the beginning of month
   integer, save, dimension(13)                 :: nc = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)

   real(8), save                                :: qup       ! qup = daily riparian zone uptake in hydrotope, mm (EET)
   real(8), save                                :: xqup

   real(8), save                                :: rn1       ! random number, used in subbasin to calc alpha
   integer, save                                :: nn        ! number of soil layers, calc in subbasin, cycle 100
   integer, save                                :: idayx     ! ida, to calc ndgro - number of growth days
   integer, save                                :: ndgro     ! number of growth days
   integer, save                                :: ndpri     ! day to write crop yield for GIS output
   real(8), save                                :: alfa      ! alpha factor for vegetation
   real(8), save                                :: r1        ! alpha for rainfall, the fraction of total rainfall
   real(8), save                                :: rp        ! alpha for rainfall, the fraction of total rainfall

   real(8), save                                :: sml       ! snow melt, calc in snom(), mm
   real(8), save                                :: snow      ! precipitation as snow, mm
   real(8), save                                :: aff       ! 1000. * da * flu(j), calc in subbasin
   real(8), save                                :: precip    ! precipitation, mm, read in readcli

!**** HYDROTOPE
   real(8), save                                :: qi        ! surface flow in HYDROTOPE, mm
   real(8), save                                :: ssf       ! subsurface flow in HYDROTOPE, mm
   real(8), save                                :: yno3      ! N-NO3 loss with surface flow, kg/ha
   real(8), save                                :: uno3      ! N uptake by the crop for a given day, kg/ha (SUPPLY)
   real(8), save                                :: swind     ! soil water index for hydrotope: swind=swe()/sumfc()
   real(8), save                                :: cn        ! current CN in hydrotope
   real(8), save                                :: qd        ! daily surface runoff, mm
   real(8), save                                :: rain      ! preinf(j,je)-qd, mm
   real(8), save                                :: su        ! water excess above FC, mm
   real(8), save                                :: sep       ! percolation, calc in perc for 4mm slugs, recalc here, mm
   real(8), save                                :: prk       ! lateral subsurface flow, mm, calc in perc for 4mm layers
   real(8), save                                :: eo        ! potential evapotranspiration, mm
   real(8), save                                :: ep        ! plant transpiration, mm
   real(8), save                                :: es        ! soil evaporation, mm
   real(8), save                                :: et        ! es + ep + canev
   real(8), save,dimension(12)                  :: omega = (/.7,.85,.95,1.05,1.25,1.15,1.05,.95,.9,.8,.75,.7/)  ! month factors for Turc(Ivanov) evap (Glugla 1989)

   real(8), save                                :: snoev     ! snow evaporation, mm
   real(8), save                                :: canev     ! canopy evaporation, mm / INTERCEPTION
   real(8), save                                :: eopot     ! potential evapotranspiration, mm / INTERCEPTION

!**** SUBBASIN
   real(8), save                                :: sumcn     ! weighted aver. Cur.Num. in subbasin (SUM(cn))
   real(8), save                                :: xqd       ! surface runoff for subbasin, mm, calc in subbasin
   real(8), save                                :: xqi       ! weighted aver. surface flow in subbasin, SUM(qd), mm
   real(8), save                                :: xssf      ! weighted aver. subsurface flow in subbasin, SUM(ssf), mm
   real(8), save                                :: xsep      ! subbasin percolation, mm
   real(8), save                                :: xeo       ! weighted aver. pot. evapotr. in subbasin, SUM(eo), mm
   real(8), save                                :: xet       ! evapotranspiration (actual), mm
   real(8), save                                :: xsnoev    ! weighted aver. snow evap in subbasin, SUM(snoev), mm
   real(8), save                                :: xswind    ! weighted aver. soil wat. index in subbasin, SUM(swimd)
   real(8), save                                :: xyno3     ! weighted aver. N loss with qd in subbasin, SUM(yno3), kg/ha
   real(8), save                                :: xysp      ! weighted aver. soluble P leaching in subbasin, kg/ha
   real(8), save                                :: xssfn     ! weighted aver. N loss with ssf in subbasin, SUM(ssfn), mm
   real(8), save                                :: xpercn    ! weighted aver. N leaching to g-w in subb., kg/ha
   real(8), save                                :: xnorg     ! weighted aver. st.org.N in lay 1 in subb., SUM(anors), g/t
   real(8), save                                :: xporg     ! weighted aver. org.P in layer 1 in subb., SUM(porg), g/t
   real(8), save                                :: xnorgp    ! weighted aver. st.org.N in lay 1 in subb., SUM(anors), kg/ha
   real(8), save                                :: xporgp    ! weighted aver. org.P in layer 1 in subb., SUM(porg), kg/ha
   real(8), save                                :: xcklsp    ! combined c,k,ls,p factor for subbasin, calc in subbasin
   real(8), save                                :: qtl       ! transmission losses in subbasin, mm
   real(8), save                                :: xpsed     ! weighted aver. total P (porg+pms+pma) in subb., g/t
   real(8), save                                :: xpsedp    ! weighted aver. total P (porg+pms+pma) in subb., kg/ha
   real(8), save                                :: cnb       ! optimal conc N in biomass, kg/kg
   real(8), save                                :: strsn     ! N stress for plant growth
   real(8), save                                :: uap       ! P uptake in hydrotope, kg/ha
   real(8), save                                :: strsp     ! P stress for plant growth
   real(8), save                                :: ts        ! temperature stress factor
   real(8), save                                :: potentl   ! plant evaporation, potential, mm
   real(8), save                                :: actual    ! actual evapotranspiration, mm
   real(8), save                                :: ssfn      ! N loss with subsurface flow, kg/ha
   real(8), save                                :: percn     ! N-NO3 leaching to g-w, kg/ha
   real(8), save                                :: ysp       ! soluble P leaching, kg/ha
   real(8), save                                :: humn      ! N mineralization from humus mineralization, kg/ha
   real(8), save                                :: csf       ! combined water/temperature factor
   real(8), save                                :: shump     ! SUM(hump): humus P-mineralization for basin, kg/h
   real(8), save                                :: sfomp     ! SUM(fomp): fresh organic P mineralisation for basin, kg/ha
   real(8), save                                :: saspf     ! SUM(aspflow): flow between active and stable P pool for basin, kg/ha
   real(8), save                                :: salpf     ! SUM(alpflow): flow between active and labile P pool for basin, kg/ha
   real(8), save                                :: pr        ! peak runoff rate, m3/sec.
   real(8), save                                :: iv        ! index for channel parameters chl(,), chw(,), chk(,)
   real(8), save                                :: vo        ! runoff volume = xqd * da * flu(j) * 1000, m3, calc in subbasin
   real(8), save                                :: dur       ! flow duration, h
   real(8), save                                :: q1        ! remember old xqd, mm, to compare with new one in subbasin
   real(8), save                                :: yd        ! daily soil loss from subbasin caused by water erosion, t
   real(8), save                                :: er        ! enrichment ratio for subbasin
   real(8), save                                :: yon       ! org N loss with erosion, kg/ha
   real(8), save                                :: conn      ! xnorg * er, g/t: N org. in I layer for subbasin, corrected for enrichment
   real(8), save                                :: yph       ! P org. loss with erosion, kg/ha
   real(8), save                                :: cpp       ! xporg * er, g/t: P org. in I layer in subbasin, g/t
   real(8), save                                :: gwseep = 0.    ! groundwater seepage, mm
   real(8), save                                :: revap  = 0.   ! revaporation from groundwater, mm
   real(8), save                                :: gwchrg = 0.   ! groundwater recharge, mm
   real(8), save                                :: wysb      ! water yield in subbasin = xqi+xssf+gwq(j)-qtl, mm

   !real(8),    save, dimension(23,14)              :: rotation  ! 
   real(8),    save, dimension(20)              :: xnflow    ! 
   real(8), save, allocatable, dimension(:)     :: xkm_qd
   real(8), save, allocatable, dimension(:)     :: c1_qd,c2_qd,c3_qd,c4_qd
   real(8), save, allocatable, dimension(:)     :: xkm_ssf
   real(8), save, allocatable, dimension(:)     :: c1_ssf,c2_ssf,c3_ssf,c4_ssf

   real(8), save                                :: shumn = 0. ! SUM(humn): humus N minerlisation for basin, kg/ha
   real(8), save                                :: sasnf = 0. ! SUM(asnflow): flow between active and stable org. N for basin, kg/ha
   real(8), save                                :: sfomn = 0. ! SUM(fomn): mineralisation from fresh org. N for basin, kg/ha
   real(8), save                                :: sdnit = 0. ! SUM(denit): daily N-NO3 loss by denitrification for basin, kg/ha
   real(8), save                                :: sbnup = 0.  ! sum of N uptake for basin
   real(8), save                                :: sbpup = 0.  ! weighted average P uptake in the basin, kg/ha

   real(8), save                                :: sdn = 0.  ! sum N stress days
   real(8), save                                :: sdp = 0.  ! sum P stress days
   real(8), save                                :: sdt = 0.  ! sum temp stress days
   real(8), save                                :: sdw = 0.  ! sum water stress days


!**** MAIN & READCOD

   integer, save                                :: isc     ! code for saturated cond.: 0 - read, 1 - calc
   integer, save                                :: icn     ! switch code for CN: 0: CN dif for soils (standard method) 
!                                                                                1: CN=const from cnum1, cnum3
   integer, save                                :: idlef   ! code for the day length effect in crop: 0 - without, 1 - with
   integer, save                                :: iemeth  ! 
   integer, save                                :: idvwk   !
   real(8), save, dimension(:), allocatable     :: dormhr    ! |hour          |time threshold used to define dormant , dimension(subtot)
                                                             ! |time threshold used to define dormant
                                                             ! |period for plant (when daylength is within
                                                             ! |the time specified by dormhr from the minimum
                                                             ! |daylength for the area, the plant will go
                                                             ! |dormant)

!------------------------------------------------------------------------------
! Transmission losses
!------------------------------------------------------------------------------
   real(8), save                                :: tlrch, evrch, tlc, evp
   integer, save                                :: tlgw    ! 0 = add transmission losses to shallow ground water; 1 = add to deep ground water
   real(8), save, dimension(:), allocatable     :: pet_day
!------------------------------------------------------------------------------

   integer, save                                :: icurn   ! switch code to print from curn() 
   integer, save                                :: icursb  ! number of subbasin to print from curn(), if icurn = 1
   integer, save                                :: isolt   ! switch code to print from solt()
   integer, save                                :: isosb   ! number of subbasin to print from solt(), if isolt = 1
   integer, save                                :: itran   ! switch code to print from tran()
   integer, save                                :: iperc   ! switch code to print from perc()
   integer, save                                :: ipesb   ! number of subbasin to print from perc(), if iperc = 1
   integer, save                                :: ipehd   ! number of hydrotope to print from perc(), if iperc = 1
   integer, save                                :: ievap   ! switch code to print from evap()
   integer, save                                :: ievsb   ! number of subbasin to print from evap(), if ievap = 1
   integer, save                                :: ievhd   ! number of hydrotope to print from evap(), if ievap = 1
   integer, save                                :: icrop   ! switch code to print from crop()
   integer, save                                :: icrsb   ! number of subbasin to print from crop(), if icrop = 1
   integer, save                                :: icrso   ! number of soil to print from crop(), if icrop = 1
   integer, save                                :: ieros   ! switch code to print from eros()
   integer, save                                :: iersb   ! number of subbasin to print from eros(), if ieros = 1
   integer, save                                :: inutr   ! switch code to print from ncycle()
   integer, save                                :: inusb   ! number of subbasin to print from ncycle(), if inutr=1
   integer, save                                :: inuhd   ! number of hydrotope to print from ncycle(), if inutr=1
   integer, save                                :: irout   ! switch code to print from routfun()
   integer, save                                :: iwstr   ! switch code to print from wstress()
   integer, save                                :: iwssb   ! number of subbasin to print from wstress(), if iwstr = 1
   integer, save                                :: iwshd   ! number of hydrotope to print from wstress(), if iwstr = 1
   integer, save                                :: gis_m   ! switch code to write monthly results (water & crops) for GIS output
   integer, save                                :: gis_y   ! switch code to write annual results (water & crops) for GIS output
   integer, save                                :: gis_ave ! switch code to write average annual results (water & crops) for GIS output
   integer, save                                :: iflom   ! switch code to write monthly flows for HRUs in subbasin=inusb, HRUs=(inuhd,...,inuhd+8)
   integer, save                                :: ifloa   ! switch code to write annual flows for HRUs in subbasin=inusb, HRUs=(inuhd,...,inuhd+8)
   logical, save                                :: bErrorFile ! write certain error messages to output file swim.err.log (0=do not write; 1=write file)

!**** READBAS
   ! switch parameters
   logical, save                                :: bSubcatch
   logical, save                                :: bLandMgt           ! switch parameter in *.bsn
   logical, save                                :: bDormancy ! dormancy threshold for day length
   logical, save                                :: bRunoffDat
   logical, save                                :: b3SubFiles         ! 0 = read single .sub, .rte, .gw files from 'Sub' directory; 1 = read only three files from directory 'Sub'

   ! .bsn parameters
   real(8), save                                :: da      ! basin area, km2
   !real(8), save                                :: bff     ! baseflow factor for basin, used to calc subsurface flow
   real(8), save                                :: gwq0    ! initial groundwater flow contribution to streamflow, mm (1 for basin)
   real(8), save                                :: abf0    ! alpha factor for groundwater, calib. param. from .bsn file
   real(8), save                                :: chwc0   ! coefficient to correct channel width (from .bsn)
   real(8), save                                :: snow1   ! initial snow content, mm (from .bsn)
   real(8), save                                :: storc1  ! storage correction coef (from .bsn)
   real(8), save                                :: stinco  ! init. water content coef., later stin()=fc()*stinco
   real(8), save                                :: cnum1   ! init. CN for cropland, cond 1
   real(8), save                                :: cnum2   ! init. CN for cropland, cond 2
   real(8), save                                :: cnum3   ! init. CN for cropland, cond 3
   real(8), save                                :: epco = 1. ! limit plant water demand compensation from lower layers.
   real(8), save                                :: af      ! 1000. * da, basin area (1000*km**2)
   real(8), save                                :: da9     ! 100. * da = basin area in ha, from readbas
   real(8), save                                :: uob     ! pap calc in readbas from ub, ub=3.065 blockdata
   real(8), save                                :: chxkc0  ! correction coef. for chnnel USLE K factor
   real(8), save                                :: chcc0   ! correction coef. for chnnel USLE C factor
   real(8), save                                :: chnnc0   ! correction coef. for channel N (Manning) factor
   real(8), save                                :: ekc0    ! soil erodibility correction factor
   real(8), save                                :: retNsur ! N retention in surface flow, days
   real(8), save                                :: retNsub ! N retention in subsurface flow, days
   real(8), save                                :: retNgrw ! N retention in groundwater, days
   real(8), save                                :: retPsur ! P retention in surface flow, days
   real(8), save                                :: degNsur ! N degradation in surface flow, 1/day
   real(8), save                                :: degNsub ! N degradation in subsurface flow, 1/day
   real(8), save                                :: degNgrw ! N degradation in groundwater, 1/day
   real(8), save                                :: degPsur ! P degradation in surface flow, 1/day
   integer, save                                :: iicep   ! switch for including interception module, 0 - no, 1 - yes
   integer, save                                :: ialpha  ! switch parameter: to calc CO2 effect on net photosynthesis?
   integer, save                                :: ibeta   ! switch parameter: to calc CO2 effect on transpiration?
   real(8), save                                :: ic3c4   ! switch parameter: 3/4 - C3 or C4 crop?
   real(8), save                                :: co2ref  ! atm CO2 in the reference period, ppm
   real(8), save                                :: co2sce  ! atm CO2 in the scenario period, ppm

!**** MAIN
   real(8), save                                :: sub(30)     ! 
   real(8), save                                :: smm(30)     ! 
   real(8), save                                :: smy(30)     ! 
   real(8), save                                :: sm(30)      ! 
   real(8), save                                :: vl          ! 
   real(8), save                                :: vb          ! 
   real(8), save                                :: v1          ! 
   real(8), save                                :: v3          ! 
   real(8), save                                :: v5          ! 
   real(8), save                                :: v7          ! 
   integer, save                                :: ieap        ! 
   integer, save                                :: ieapu       ! 
   real(8), save                                :: xxswind     ! 
   real(8), save                                :: xwysb       ! 
   real(8), save                                :: ndmo(12)    ! 
   integer, save                                :: iday        ! 
   integer, save                                :: inday       ! 
   integer, save                                :: ieapu_c     ! 
   integer, save                                :: ieap_crp    ! 
   real(8), save                                :: runs(366)   ! 
   real(8), save                                :: xxqd        ! 
   real(8), save                                :: sdti        ! 
   real(8), save                                :: ydi         ! 
   real(8), save                                :: diver       ! 
   real(8), save                                :: rflow       ! 
   real(8), save                                :: rl          ! 
   real(8), save                                :: prf         ! 
   real(8), save                                :: spcon       ! 
   real(8), save                                :: spexp       ! 
   real(8), save                                :: prcor = 1.  ! 
   real(8), save                                :: rdcor       ! 
   real(8), save                                :: wy          ! 
   real(8), save                                :: xxssf       ! 
   real(8), save                                :: xxeo        ! 
   real(8), save                                :: xxnit       ! 
   real(8), save                                :: roc1        ! 
   !real(8), save                                :: roc2        ! 
   real(8), save                                :: roc3        ! 
   !real(8), save                                :: roc4        ! 
   real(8), save                                :: accf(10)    ! 
   real(8), save                                :: diso(40000) ! 
   real(8), save                                :: diss(40000) ! 
   integer, save                                :: istyr       ! 

!******************************************************************************
!*** snow module parameters
   real(8), save                                :: area_tot_snow     ! area covered by snow [m2]
   real(8), save                                :: depth_ave_snow    ! average snow depth [mm]
   real(8), save                                :: area_tot_glacier  ! area covered by glaciers [m2]
   real(8), save                                :: depth_ave_glacier ! average glacier depth [mm], averaged over the entire catchment!
   real(8), save                                :: snow_acc_mm       ! accumulated snow water content in the catchment at the end of the year, mm
   real(8), save                                :: snow_acc_mm0      ! accumulated snow water content in the catchment at the end of the previous year, mm
   real(8), save                                :: glac_acc_mm       ! accumulated glacier water content in the catchment at the end of the year, mm
   real(8), save                                :: glac_acc_mm0       ! accumulated glacier water content in the catchment at the end of the previous , mm
   real(8), save                                :: soil_acc_mm       ! accumulated soil water content in the catchment at the end of the year, mm
   real(8), save                                :: soil_acc_mm0       ! accumulated soil water content in the catchment at the end of the previous year, mm
!******************************************************************************

!******************************************************************************
!*** subcatch parameters
   integer, save                                :: n_subcatch
   integer, save, dimension(:), allocatable     :: subcatch_id        ! array storing an ID for each subbasin indicating the sub-catchment ID
   character(len=20),dimension(:),allocatable   :: subcatch_name      ! subcatchment name read from subcatch.def
   real(8), save, dimension(:),allocatable      :: subcatch_area      ! area of subcatchment in [m2]
   real(8), save, dimension(:,:,:), allocatable :: subcatch_an        ! array storing annual subcatch output (nbyr,nSubcatch,30)

   ! subcatch.bsn
   real(8), save, dimension(:), allocatable     :: ecal               ! coefs for each subbasin
   real(8), save, dimension(:), allocatable     :: thc                ! coefs for each subbasin
   real(8), save, dimension(:), allocatable     :: sccor              ! coefs for each subbasin
   real(8), save                                :: sccor_
   real(8), save, dimension(:), allocatable     :: cncor              ! coefs for each subbasin
   real(8), save, dimension(:), allocatable     :: roc2               ! coefs for each subbasin
   real(8), save, dimension(:), allocatable     :: roc4               ! coefs for each subbasin
   real(8), save, dimension(:), allocatable     :: tsnfall            ! threshold temperature for snow fall
   real(8), save, dimension(:), allocatable     :: tmelt              ! threshold temperature for snow melt
   real(8), save, dimension(:), allocatable     :: smrate             ! snow melt rate for the degree-day method 
   real(8), save, dimension(:), allocatable     :: gmrate             ! glacier melt rate for the degree-day method 

   real(8), save, dimension(:), allocatable     :: bsn_ecal           ! general potential evap calibration factor
   real(8), save, dimension(:), allocatable     :: bsn_thc            ! correction factor for potential evapotranspiration; range for thc: (0.8-1.0), value 1. - from R. Muttiah
   real(8), save, dimension(:), allocatable     :: bsn_sccor          ! correction factor for saturated conductivity (all soils)
   real(8), save, dimension(:), allocatable     :: bsn_cncor          ! correction factor for curve number (0.25 - 1.25)
   real(8), save, dimension(:), allocatable     :: bsn_roc2           ! coef. for routing: correction coef to calculate xkm
   real(8), save, dimension(:), allocatable     :: bsn_roc4           ! coef. for routing: correction coef to calculate xkm
   real(8), save, dimension(:), allocatable     :: bsn_tsnfall        ! threshold temperature for snow fall
   real(8), save, dimension(:), allocatable     :: bsn_tmelt          ! threshold temperature for snow melt
   real(8), save, dimension(:), allocatable     :: bsn_smrate         ! snow melt rate for the degree-day method
   real(8), save, dimension(:), allocatable     :: bsn_gmrate         ! glacier melt rate for the degree-day method

   ! subcatch.gw
   real(8), save, dimension(:), allocatable     :: bff
   real(8), save, dimension(:), allocatable     :: gwht   ! groundwater height, mm
   real(8), save, dimension(:), allocatable     :: gwq    ! groundwater contribution to stream, in mm
   real(8), save, dimension(:), allocatable     :: abf    ! alpha factor for g-w = reaction factor * delta(T)
   real(8), save, dimension(:), allocatable     :: syld   ! specific yield for g-w
   real(8), save, dimension(:), allocatable     :: delay  ! groundwater delay, days
   real(8), save, dimension(:), allocatable     :: revapc ! fraction of root zone perc that goes to revap
   real(8), save, dimension(:), allocatable     :: rchrgc ! fraction (0-1) of root zone perc that percolates
   real(8), save, dimension(:), allocatable     :: revapmn! revap storage, mm

   real(8), save, dimension(:), allocatable     :: gw_bff             ! baseflow factor for basin, used to cal! subsurface flow
   real(8), save, dimension(:), allocatable     :: gw_gwht            ! groundwater height, mm
   real(8), save, dimension(:), allocatable     :: gw_gwq             ! initial groundwater flow contribution to streamflow, mm
   real(8), save, dimension(:), allocatable     :: gw_abf             ! alpha factor for groundwater
   real(8), save, dimension(:), allocatable     :: gw_syld            ! specific yield for g-w
   real(8), save, dimension(:), allocatable     :: gw_delay           ! groundwater delay, days        ! alpha factor for groundwater
   real(8), save, dimension(:), allocatable     :: gw_revapc          ! fraction of root zone perc that goes to revap
   real(8), save, dimension(:), allocatable     :: gw_rchrgc          ! fraction (0-1) of root zone perc that percolates
   real(8), save, dimension(:), allocatable     :: gw_revapmn         ! revap storage, mm
!******************************************************************************

   
!**** Number of julian days passed to the beginning of month
!cls   integer, save, dimension(13)              :: nc = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)


!******************************************************************************
!**** Begin: Arrays allocated with: mb = total number of subbasins (mb)

   integer, save, dimension(:), allocatable     :: neap                 ! number of HRUs in subbasin
   integer, save                                :: maxhru               ! maximum number of HRUs per subbasin
   real(8),    save, dimension(:), allocatable  :: sbar                 ! subbasin area [m2]
   real(8),    save, dimension(:), allocatable  :: tp5, tp6, tpnyr      ! wheather generator data from wgen.dat

!### VA
   integer, save                                :: radiation_switch     ! 0 = read radiation data from clim1; 1 = calculate radiation after Hargreaves (latitude in degrees in file stat-outdat.csv) required!
   real(8), save                                :: ec1     ! empirical constant in Hargreaves (1985) equation, readcli.f
!### VA
! RIPARIAN ZONE
!     new variables for riperian zone
!     gwqLastday[m3]=Groundwater and subsurface flow form day before;
!     rzmaxup [mm]  =maximum additional additional uptake rate in wetlands
!     additionalGwUptake [m3] = additional uptake of current day
   real(8),    save, dimension(:), allocatable  :: gwqLastday
   real(8),    save, dimension(:), allocatable  :: additionalGwUptake
   real(8),    save                             :: rzmaxup ! maximum additional additional uptake rate in wetlands, mm
! END RIPARIAN ZONE


!**** SUBBASIN parameter (*.sub)
   real(8),    save, dimension(:),   allocatable   :: flu    ! fraction of subbasin area in the basin
   real(8),    save, dimension(:),   allocatable   :: salb   ! soil albedo
   real(8),    save, dimension(:),   allocatable   :: sno    ! accumulated value of snow cover, mm / water content of snow, mm
   real(8),    save, dimension(:),   allocatable   :: chs    ! main channel slope, m/m
   real(8),    save, dimension(:),   allocatable   :: chn    ! channel N value
   real(8),    save, dimension(:,:), allocatable   :: chl    ! main channel length, km
   real(8),    save, dimension(:,:), allocatable   :: chw    ! average width of main channel, m
   real(8),    save, dimension(:,:), allocatable   :: chk    ! effective hydraulic conductivity of main channel, mm/h
   real(8),    save, dimension(:),   allocatable   :: ovn    ! overland flow N value
   real(8),    save, dimension(:),   allocatable   :: rt     ! return flow travel time, days. If 0, then estimated
   real(8),    save, dimension(:),   allocatable   :: css    ! sediment conc in return flow, ppm
   real(8),    save, dimension(:),   allocatable   :: ecp    ! USLE erosion control practice factor P
   real(8),    save, dimension(:),   allocatable   :: sl     ! USLE slope length/slope steepness factor
   real(8),    save, dimension(:),   allocatable   :: stp    ! average slope steepness for subbasin, m/m
!**** SUBBASIN parameter (*.sub)

   real(8),    save, dimension(:,:), allocatable   :: phi    ! channel and flow parameters

   real(8),    save, dimension(:), allocatable     :: yls
   real(8),    save, dimension(:), allocatable     :: ylc
   real(8),    save, dimension(:), allocatable     :: daylmn ! min day length, h, calc. in readwet

   real(8),    save, dimension(:), allocatable     :: avt    ! average annual air temp, degree C, used in solt()
   real(8),    save, dimension(:), allocatable     :: amp    ! annual 1/2 amplitude in daily average temp, calc readwet
   real(8),    save, dimension(:), allocatable     :: ffc    ! fraction of field capacity
   real(8),    save, dimension(:), allocatable     :: daylen ! day length in subbasin, h, calc in readcli

   real(8),    save, dimension(:), allocatable     :: slsoil ! average slope length for subbasin (m)
   real(8),    save, dimension(:), allocatable     :: abf1   ! exp function of alpha factor for groundwater (abf)
   real(8),    save, dimension(:), allocatable     :: abf2   ! 
   real(8),    save, dimension(:), allocatable     :: gwqs   ! 
   real(8),    save, dimension(:), allocatable     :: gwqd   ! 
   real(8),    save                                :: abfzwo   ! 
   real(8),    save                                :: del0   ! 
   real(8),    save                                :: evafac   ! 
   real(8),    save                                :: gwafac   ! 

   real(8),    save, dimension(:), allocatable     :: sdtsav ! initial water storage in subbasins, m3
   real(8),    save, dimension(:), allocatable     :: tc     ! sum of overland and channel concentration times, hours
   real(8),    save, dimension(:), allocatable     :: al     ! fun(tc,tp5,tp6,flu,da), calc in readsub
   real(8),    save, dimension(:), allocatable     :: cv     ! initial land cover, kg/ha

   real(8),    save, dimension(:), allocatable     :: sq     ! SUM(syq)  total SUM (whole period) surface runoff
   real(8),    save, dimension(:), allocatable     :: ssq    ! SUM(sysq) total SUM (whole period) sub-surface runoff
   real(8),    save, dimension(:), allocatable     :: sy     ! SUM(syy)  total SUM (whole period) sed. yield

   real(8),    save, dimension(:), allocatable     :: smq    ! SUM(xqd) - surface runoff for subbasin, mm
   real(8),    save, dimension(:), allocatable     :: smsq   ! SUM of subsurface runoff for subbasin
   real(8),    save, dimension(:), allocatable     :: sym    ! monthly SUM of sediment yield for subbasin
   real(8),    save, dimension(:), allocatable     :: syq    ! SUM(smq)  annual SUM surface runoff for subbasin
   real(8),    save, dimension(:), allocatable     :: sysq   ! SUM(smsq) annual SUM sub-surface runoff
   real(8),    save, dimension(:), allocatable     :: syy    ! SUM(sym)  annual SUM sed. yield for subbasin

   real(8),    save, dimension(:), allocatable     :: sbpy   ! annual SUM of precipitation in subbasin
   real(8),    save, dimension(:), allocatable     :: sbp    ! monthly  SUM of precipitation in subbasin
   real(8),    save, dimension(:), allocatable     :: tmx    ! daily max temp. for subbasin, readcli, degree C
   real(8),    save, dimension(:), allocatable     :: tmn    ! daily min temp. for subbasin, readcli, degree C
   real(8),    save, dimension(:), allocatable     :: tx     ! average daily temp., degree C
   real(8),    save, dimension(:), allocatable     :: subp   ! daily precipitation in the subbasin, mm
   real(8),    save, dimension(:), allocatable     :: ra     ! solar radiation in subbasin, J/cm^2
   real(8),    save, dimension(:), allocatable     :: humi   ! air humidity in the subbasin, %

   real(8),    save, dimension(:), allocatable     :: tmpNsur !
   real(8),    save, dimension(:), allocatable     :: tmpNsub !
   real(8),    save, dimension(:), allocatable     :: tmpNgrw !
   real(8),    save, dimension(:), allocatable     :: tmpPsur !

   real(8),    save, dimension(:), allocatable     :: yone   ! org. N loss with erosion (calc in orgnsed), kg/ha
   real(8),    save, dimension(:), allocatable     :: yphe   ! org. P loss with erosion, kg/ha

   real(8),    save, dimension(:), allocatable     :: rchrg  ! groundwater recharge to the aquifer, mm
   real(8),    save, dimension(:), allocatable     :: revapst!transmission losses in the reach, calc in route, m3

   real(8),    save, dimension(:), allocatable     :: wim     ! monthly max .5h rain for period of record, mm
   real(8),    save, dimension(:), allocatable     :: r       ! READWET
   real(8),    save, dimension(:), allocatable     :: rsm     ! READWET
   real(8),    save, dimension(:), allocatable     :: rsmm    ! READWET
   real(8),    save, dimension(:), allocatable     :: rsmy    ! READWET

!-----------------------------------------------------------------------------

   real(8),    save, dimension(:), allocatable     :: obmx   ! (12,:) average monthly max temp, degree C
   real(8),    save, dimension(:), allocatable     :: obmn   ! (12,:) average monthly min temp, degree C
   real(8),    save, dimension(:), allocatable     :: rst    ! (12,:)monthly mean event of daily rainfall, mm
   real(8),    save, dimension(:,:),allocatable    :: prw    ! prw(1,m,j) = monthly probability of wet day after dry day
                                                          ! prw(2,m,j) = monthly probability of wet day after wet day
   real(8),    save, dimension(:,:), allocatable   :: wft    ! monthly prob. of rainy day, calc readwet
   real(8),    save, dimension(:,:), allocatable   :: wi     ! wi(m,j)=f(wim), used in alpha() for estim of precip. alpha factor

   real(8),    save, dimension(:,:), allocatable   :: susb   ! monthly SUBBASIN outputs (dif. components) sysub (see subbasin.f)
   real(8),    save, dimension(:,:), allocatable   :: sysub  ! annual SUBBASIN outputs (dif. components)
   real(8),    save, dimension(:,:), allocatable   :: stsub  ! total SUBBASIN outputs = SUM(sysub)

   real(8),    save, dimension(:,:), allocatable   :: parsz  ! particle size distribution, calc in subbasin
   real(8),    save, dimension(:,:), allocatable   :: pct    ! delivery ratios (part. size distr. for sediments)
   real(8),    save, dimension(:,:), allocatable   :: sda    ! subbasin outputs for routing, andlogue to varoute()
                                                          ! varoute(1:10,ihout) = sda(1:10,j)

   ! read from stat-outdat.csv
   real(8),    save, dimension(:), allocatable     :: lat    ! latitude of subbasin centroid
   real(8),    save, dimension(:),   allocatable   :: elev0  ! elevation centroids, m

!**** End: Arrays allocated with: mb = total number of subbasins
!******************************************************************************


!******************************************************************************
!**** Begin: Arrays allocated with mb (mb) & meap (max number of hydrotopes in subbasin)
   real(8),    save, dimension(:,:), allocatable   :: frar   ! fractional area of HYDROTOPE in SUBBASIN

   real(8),    save, dimension(:,:),   allocatable :: swe  ! soil water content, mm
   real(8),    save, dimension(:,:),   allocatable :: snoa ! snow water content in HYDROTOPE, mm
   real(8),    save, dimension(:,:,:), allocatable :: ste  ! water storage in a layer, recalc here, mm
   real(8),    save, dimension(:,:,:), allocatable :: te   ! daily ave temp at the bottom of each layer, degree C
   real(8),    save, dimension(:,:),   allocatable :: te0  ! bare soil surface temp, degree
   real(8),    save, dimension(:,:,:), allocatable :: flate! lateral subsurface flow, sum of prk for layer, mm
   real(8),    save, dimension(:,:,:), allocatable :: poe  ! percolation, sum of sep for layer, mm
   real(8),    save, dimension(:,:),   allocatable :: s1   ! internal func. for Richie's method to estimate es
   real(8),    save, dimension(:,:),   allocatable :: s2   ! internal func. for Richie's method to estimate es
   real(8),    save, dimension(:,:),   allocatable :: tv   ! internal func. for Richie's method to estimate es

   real(8),    save, dimension(:,:),   allocatable :: smx  ! retention factor, corresponding cn1
   real(8),    save, dimension(:,:,:), allocatable :: wf   ! shape parameters for calc. of retention

   real(8),    save, dimension(:,:),   allocatable :: bcv  ! lag factor (residue and snow effect on temp)
   integer, save, dimension(:,:),   allocatable    :: nveg ! vegetation number (database)

   real(8),    save, dimension(:,:),   allocatable :: cklsp! combined c,k,ls,p factor

   integer, save, dimension(:,:,:), allocatable    :: mstruc ! HRU structure vector to define k,n

   integer, save, dimension(:,:),   allocatable    :: nucr ! crop number (database)
   integer, save, dimension(:,:),   allocatable    :: idorm! index for dormant period

   integer, save, dimension(:,:),   allocatable    :: nclc     ! index for dormant period
   integer, save, dimension(:,:),   allocatable    :: avyldrot ! index for dormant period
   integer, save, dimension(:,:),   allocatable    :: aryldrot ! index for dormant period
   integer, save, dimension(:,:),   allocatable    :: irotup   ! index for dormant period
   integer, save, dimension(:,:),   allocatable    :: iccup    ! index for dormant period


   real(8),    save, dimension(:,:,:), allocatable :: rsd  ! crop residue in two upper soil layers, kg/ha
   real(8),    save, dimension(:,:),   allocatable :: cva  ! vegetation cover, kg/ha
   real(8),    save, dimension(:,:),   allocatable :: dm   ! total biomass, kg/ha
   integer, save, dimension(:,:),   allocatable    :: igro ! vegetation index, =1 if vegetation is growing
   real(8),    save, dimension(:,:),   allocatable :: g    ! fraction heat units accumulated, 1/1
   real(8),    save, dimension(:,:),   allocatable :: huharv ! harvest index heat unit
   real(8),    save, dimension(:,:),   allocatable :: hia  ! harvest index
   real(8),    save, dimension(:,:),   allocatable :: hiad ! harvest index, adjusted
   real(8),    save, dimension(:,:),   allocatable :: rwt  ! fraction of root weight
   real(8),    save, dimension(:,:),   allocatable :: olai ! alai(j,je) - leaf area index
   real(8),    save, dimension(:,:),   allocatable :: alai ! leaf area index
   real(8),    save, dimension(:,:,:), allocatable :: fon  ! fresh organic N from residue in a layer, kg/ha
   real(8),    save, dimension(:,:,:), allocatable :: fop  ! fresh org N, kg/ha
   real(8),    save, dimension(:,:),   allocatable :: yld  ! crop yield, kg/ha
   real(8),    save, dimension(:,:),   allocatable :: ylda ! crop yield for subbasin and soil, kg/ha
   real(8),    save, dimension(:,:),   allocatable :: rd   ! root depth, mm
   real(8),    save, dimension(:,:),   allocatable :: ws   ! water stress factor
   real(8),    save, dimension(:,:),   allocatable :: wsav ! water stress, sum for the growth period
   real(8),    save, dimension(:,:),   allocatable :: tsav ! temp  stress, sum for the growth period
   real(8),    save, dimension(:,:),   allocatable :: swh  ! actual transp. by plants, mm
   real(8),    save, dimension(:,:),   allocatable :: swp  ! potent. transp. by plants, mm

   real(8),    save, dimension(:,:),   allocatable :: snup ! N uptake accumulated, kg/ha
   real(8),    save, dimension(:,:),   allocatable :: spup ! P uptake in hydrotop, kg/ha

   real(8),    save, dimension(:,:),   allocatable :: presum! annual sum of precip for GIS output, mm
   real(8),    save, dimension(:,:),   allocatable :: pcpmean! mean of annual sums of precip for GIS output, mm
   real(8),    save, dimension(:,:),   allocatable :: runsum! annual sum of surface+subsurface runoff for GIS output, mm
   real(8),    save, dimension(:,:),   allocatable :: evasum! annual sum of evapotranspiration in hydrotope, mm
   real(8),    save, dimension(:,:),   allocatable :: evamean ! mean of annual evapotranspiration in hydrotope, mm
   real(8),    save, dimension(:,:),   allocatable :: gwrsum! annual sum of groundwater recharge for GIS output, mm
   real(8),    save, dimension(:,:),   allocatable :: gwrmean! mean of annual sums of groundwater recharge for GIS output, mm

!  NEW GIS OUTPUTS
   integer, save                                   :: giscounter = 1;
   real(8),    save, dimension(:,:),   allocatable :: petsum ! annual sum of potential evapotranspiration for GIS ouput
   real(8),    save, dimension(:,:),   allocatable :: petmean ! annual sum of potential evapotranspiration for GIS ouput

!  SL monthly GIS output, dimension(subbasin,hydrotop,month)
   real(8),    save, dimension(:,:,:), allocatable :: presummon ! monthly sum of precip for GIS output, mm
   real(8),    save, dimension(:,:,:), allocatable :: runsummon ! monthly sum of surface+subsurface runoff for GIS output, mm
   real(8),    save, dimension(:,:,:), allocatable :: evasummon ! monthly sum of act. evapotranspiration for GIS output, mm
   real(8),    save, dimension(:,:,:), allocatable :: petsummon ! monthly sum of pot. evapotranspiration for GIS output, mm
   real(8),    save, dimension(:,:,:), allocatable :: gwssummon ! monthly sum of groundwater seepage for GIS output, mm
   real(8),    save, dimension(:,:,:), allocatable :: swisummon ! monthly average soil water index for GIS output, mm
   integer, save, dimension(:,:,:), allocatable    :: npredays01 ! number of rainy days > 1mm for monthly GIS output [n days]
   integer, save, dimension(:,:,:), allocatable    :: npredays05 ! number of rainy days > 5mm for monthly GIS output [n days]
   integer, save, dimension(:,:,:), allocatable    :: npredays10 ! number of rainy days > 10mm for monthly GIS output [n days]
   integer, save, dimension(:,:,:), allocatable    :: npredays20 ! number of rainy days > 20mm for monthly GIS output [n days]
!  SL monthly GIS output



   real(8),    save, dimension(:,:),   allocatable :: canstor! canopy water storage, mm
   real(8),    save, dimension(:,:),   allocatable :: preinf! precipitation adjusted for canopy storage, mm
   real(8),    save, dimension(:,:),   allocatable :: hsumul! sum of upper limit water content in soil, calc in subbasin, mm
   real(8),    save, dimension(:,:),   allocatable :: hsumfc! sum of field capacity in soil, calc in subbasin, mm

   real(8),    save, dimension(:,:,:), allocatable :: ano3 ! nitrate (NO3-N) content in a layer
   real(8),    save, dimension(:,:,:), allocatable :: anora! active org. N content in a layer, kg/ha
   real(8),    save, dimension(:,:,:), allocatable :: anors! stable organic N in sub j, HRU je, layer l
   real(8),    save, dimension(:,:,:), allocatable :: plab ! labile P content in a layer, kg/ha
   real(8),    save, dimension(:,:,:), allocatable :: porg ! organic P in a layer, kg/ha
   real(8),    save, dimension(:,:,:), allocatable :: pma  ! active mineral P in a layer, kg/ha
   real(8),    save, dimension(:,:,:), allocatable :: pms  ! stable mineral P in a layer, kg/ha

   real(8),    save, dimension(:,:,:), allocatable :: hrtc ! return flow travel time, calc. hrflowtt, h

   real(8),    save, dimension(:,:,:), allocatable :: dflow ! monthly flows for water and N (see writhru.f)
   real(8),    save, dimension(:,:,:), allocatable :: dfloy ! annual sums, analogue to dflow
   real(8),    save, dimension(:,:,:), allocatable :: dflav ! total period average sums, analogue to dflow

   real(8),    save, dimension(:,:,:), allocatable :: hwss ! fun(field cap), calc in subbasin from hsumfc(j,jea)

   integer, save, dimension(:),     allocatable    :: ihydRot     ! crop rotation type of the hydrotope
   integer, save, dimension(:),     allocatable    :: ihydFert    ! fertility class of the hydrotope
   integer, save, dimension(:),     allocatable    :: ihydRotCrp  ! hydrotope's crop taken from ihydRot


!**** End: Arrays allocated with mb (mb) & meap (max number of hydrotopes in subbasin)
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with mch

   real(8),    save, dimension(:),     allocatable :: chd  ! channel depth, m
   real(8),    save, dimension(:),     allocatable :: chss ! channel slope, m/m
   real(8),    save, dimension(:),     allocatable :: chnn ! channel N value
   real(8),    save, dimension(:),     allocatable :: chxk ! channel USLE K factor
   real(8),    save, dimension(:),     allocatable :: chc  ! channel USLE C factor

   real(8),    save, dimension(:,:),   allocatable :: srch ! monthly REACH outputs (dif. components)
   real(8),    save, dimension(:,:),   allocatable :: syrch! annual REACH outputs (dif. components)
   real(8),    save, dimension(:,:),   allocatable :: strch! total REACH outputs = SUM(syrch)

!**** End: Arrays allocated with mch
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with mb (mb) & ms & mcrdb

   real(8),    save, dimension(:,:,:), allocatable :: avyld! av yld per sub,soil,crop & aryld(j,k,icr): frac. area
   real(8),    save, dimension(:,:,:), allocatable :: aryld! fraction of area by crop per sub,soil

!**** End: Arrays allocated with mb (mb) & ms & mcrdb
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with mhyd

   integer, save, dimension(:),     allocatable    :: icodes ! code to switch between routing subroutines
   integer, save, dimension(:),     allocatable    :: ihouts ! Hyd. Storage Location
   integer, save, dimension(:),     allocatable    :: inum1s ! Reach No.
   integer, save, dimension(:),     allocatable    :: inum2s ! Inflow Hydrograph
   integer, save, dimension(:),     allocatable    :: inum3s ! Irrigation transfer code: not used in SWIM, ih=1,...,mhyd
   integer, save, dimension(:),     allocatable    :: inum4s ! Reservoir No. for hyd. out: not used in SWIM, ih=1,...,mhyd
   real(8), save, dimension(:),     allocatable    :: rnum1s ! Flow amount transferred: not used in SWIM, ih=1,...,mhyd

   real(8),    save, dimension(:),     allocatable :: dart   ! drainage area for subbasin, km2

   real(8),    save, dimension(:),     allocatable :: qdilast! = qdinp(,) in the last day of the year, m3
   real(8),    save, dimension(:),     allocatable :: qdolast! = qdout(,) in the last day pof the year, m3
   real(8),    save, dimension(:),     allocatable :: qsilast! = qssinp(,) in the last day of the year, m3
   real(8),    save, dimension(:),     allocatable :: qsolast! = qssout(,) in the last day of the year, m3

   real(8),    save, dimension(:,:),   allocatable :: varoute! varoute(1:19,ih) = variables for routing

   real(8),    save, dimension(:,:),   allocatable :: qdinp  ! surface flow - daily input in reaches, m3
   real(8),    save, dimension(:,:),   allocatable :: qdout  ! surface flow - daily output in reaches, m3
   real(8),    save, dimension(:,:),   allocatable :: qssinp ! subsurface + g-w flow - daily input in reaches, m3
   real(8),    save, dimension(:,:),   allocatable :: qssout ! subsurface + g-w flow - daily output in reaches, m3

!**** End: Arrays allocated with mhyd
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with: soilstot = total number of soils

   integer, save, dimension(:), allocatable        :: snum  ! position of the soil (storing the soil number)
   integer, save, dimension(:), allocatable        :: ns    ! number of soil layers
   integer, save, dimension(:), allocatable        :: nsolgr
   real(8),    save, dimension(:), allocatable     :: ek
   real(8),    save, dimension(:), allocatable     :: bd
   real(8),    save, dimension(:), allocatable     :: abd
   real(8),    save, dimension(:), allocatable     :: swin
   real(8),    save, dimension(:), allocatable     :: sumul
   real(8),    save, dimension(:), allocatable     :: sumfc
   integer, save, dimension(:), allocatable        :: nsa   ! number of layers for arable soils

   real(8),    save, dimension(:,:),allocatable    :: avylds ! av. yld per soil,crop, kg/ha
   real(8),    save, dimension(:,:),allocatable    :: arylds !  fraction of area by crop per soil

   real(8),    save, dimension(:,:),allocatable    :: silt ! silt content, %
   real(8),    save, dimension(:,:),allocatable    :: clay ! clay content, %
   real(8),    save, dimension(:,:),allocatable    :: sand ! sand content, %
   real(8),    save, dimension(:,:),allocatable    :: psz  ! particle size distribution

!**** End: Arrays allocated with: sopilstot = total number of soils
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with: ml=10 & soilstot = number of soils

!**** Number of soil hydrological groups (A,B,C,D)
   real(8),    save, dimension(:,:), allocatable   :: z
   real(8),    save, dimension(:,:), allocatable   :: cla
   real(8),    save, dimension(:,:), allocatable   :: sil
   real(8),    save, dimension(:,:), allocatable   :: san
   real(8),    save, dimension(:,:), allocatable   :: bden
   real(8),    save, dimension(:,:), allocatable   :: poros
   real(8),    save, dimension(:,:), allocatable   :: awc
   real(8),    save, dimension(:,:), allocatable   :: fc
   real(8),    save, dimension(:,:), allocatable   :: cbn
   real(8),    save, dimension(:,:), allocatable   :: wn
   real(8),    save, dimension(:,:), allocatable   :: sc
   real(8),    save, dimension(:,:), allocatable   :: wno3
   real(8),    save, dimension(:,:), allocatable   :: ap
   real(8),    save, dimension(:,:), allocatable   :: wmn
   real(8),    save, dimension(:,:), allocatable   :: wpo
   real(8),    save, dimension(:,:), allocatable   :: pmn
   real(8),    save, dimension(:,:), allocatable   :: op
   real(8),    save, dimension(:,:), allocatable   :: hum
   real(8),    save, dimension(:,:), allocatable   :: wp
   real(8),    save, dimension(:,:), allocatable   :: up
   real(8),    save, dimension(:,:), allocatable   :: por
   real(8),    save, dimension(:,:), allocatable   :: stin
   real(8),    save, dimension(:,:), allocatable   :: ul
   real(8),    save, dimension(:,:), allocatable   :: hk

!**** End: Arrays allocated with: ml=10 & soilstot = number of soils
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with: mc = number of land use types

   real(8),    save, dimension(:,:,:), allocatable :: cn2   ! Curve Numbers for soil k, land use n, and subbasin
   real(8),    save, dimension(:),   allocatable   :: alai0 ! initial Leaf Area Index for land use type
   
   integer, save, dimension(:,:), allocatable      :: idop  ! day of operation
   integer, save, dimension(:,:), allocatable      :: iopc  ! operation code: 1 - planting, 2 - harvest & kill
   integer, save, dimension(:,:), allocatable      :: ncrp  ! crop number

   integer, save, dimension(:,:), allocatable      :: idfe  ! day of fertilization
   real(8),    save, dimension(:,:), allocatable   :: fen   ! amount of min N fertilizers applied, kg N/ha
   real(8),    save, dimension(:,:), allocatable   :: fep   ! amount of P fertilizers applied, kg P/ha
   real(8),    save, dimension(:,:), allocatable   :: feno  ! amount of org N fertilizers applied, kg N/ha

!**** End: Arrays allocated with: mc = number of land use types
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with: mcrdb = max number of crops in the database

!  crop.dat parameters
   integer, save, dimension(:),allocatable         :: icnum ! crop number
   character(len=4),save,dimension(:),allocatable  :: cnam  ! crop name (abbreviation)
   real(8),     save, dimension(:),   allocatable  :: be    ! biomass-energy ratio for crop, kg m2 MJ-1 ha-1 d-1
   real(8),     save, dimension(:),   allocatable  :: hi    ! harvest index for crop (from database)
   real(8),     save, dimension(:),   allocatable  :: to    ! optimal temperature for plant growth, degrees C
   real(8),     save, dimension(:),   allocatable  :: tb    ! base temp. for plant growth, degree C
   real(8),     save, dimension(:),   allocatable  :: blai  ! max LAI for crop
   real(8),     save, dimension(:),   allocatable  :: dlai  ! fraction of growing season, when LAI declines
   real(8),     save                               :: dlp1 = 0 ! complex number: fraction of grow. season, max corresp. LAI
   real(8),     save                               :: dlp2 = 0 ! complex number: fraction of grow. season, max corresp. LAI
   real(8),     save, dimension(:),   allocatable  :: bn1   ! normal fraction of N in plant biomass at emergence
   real(8),     save, dimension(:),   allocatable  :: bn2   ! nitrogen uptake parameter #2: normal fraction of N
   real(8),     save, dimension(:),   allocatable  :: bn3   ! nitrogen uptake parameter #3: normal fraction of N
   real(8),     save, dimension(:),   allocatable  :: bp1   ! normal fraction of P in plant biomass at emergence
   real(8),     save, dimension(:),   allocatable  :: bp2   ! phosphorus uptake parameter #2: normal fraction of P
   real(8),     save, dimension(:),   allocatable  :: bp3   ! phosphorus uptake parameter #3: normal fraction of P
   real(8),     save, dimension(:),   allocatable  :: cnyld ! fraction of nitrogen in yield, kg N/kg yield
   real(8),     save, dimension(:),   allocatable  :: cpyld ! fraction of phosphorus in yield, kg P/kg yield
   real(8),     save, dimension(:),   allocatable  :: rdmx  ! maximum root depth, mm
   real(8),     save, dimension(:),   allocatable  :: cvm   ! minimum value of C factor for water erosion, readcrp
   real(8),     save, dimension(:),   allocatable  :: almn  ! minimum Leaf Area Index (for forest and natural vegetation)
   real(8),     save, dimension(:),   allocatable  :: sla   ! specific leaf area, m2/kg, LAI/SLA  in kg/m2
   real(8),     save, dimension(:),   allocatable  :: pt2   ! 2nd point on radiation use efficiency curve
   integer,  save, dimension(:),   allocatable     :: hun   ! potential heat units required for maturity of crop

!  other parameters allcated with mrcdb
   integer,  save, dimension(:),   allocatable     :: ilcc  ! land cover category for vegetation iv
   real(8),     save, dimension(:),   allocatable  :: ald1  ! shape parameter for the LAI developement equation
   real(8),     save, dimension(:),   allocatable  :: ald2  ! shape parameter for the LAI developement equation
   real(8),     save, dimension(:),   allocatable  :: bnu1  ! coef used to calculate sp1 - S-curve parameter
   real(8),     save, dimension(:),   allocatable  :: bnu2  ! coef used to calculate sp2 - S-curve parameter
   real(8),     save, dimension(:),   allocatable  :: bpu1  ! shape parameter to calc optimal P fraction in crop biomass
   real(8),     save, dimension(:),   allocatable  :: bpu2  ! shape parameter to calc optimal P fraction in crop biomass

   real(8),     save, dimension(:),   allocatable  :: avyldc! av yld per crop, kg/ha
   real(8),     save, dimension(:),   allocatable  :: aryldc! fraction of area by crop
   real(8),     save, dimension(:,:), allocatable  :: avylda! av yld per year, crop, kg/ha
   real(8),     save, dimension(:,:), allocatable  :: arylda! fraction of area by crop per year

!**** End: Arrays allocated with: mcrdb = max number of crops in the database
!******************************************************************************

!******************************************************************************



!**** Curve Numbers for 4 soil classes A,B,C,D & 15 lans use types
!   parameters moved to input file lut.dat
!   read in subroutine: readlut.f90, called from mainpro.f90
   real(8),    save, dimension(mc)                 :: cn2a 
   real(8),    save, dimension(mc)                 :: cn2b 
   real(8),    save, dimension(mc)                 :: cn2c
   real(8),    save, dimension(mc)                 :: cn2d
   real(8),    save, dimension(mc)                 :: canmax! canopy maximum storage for interception, mm, calc in init
   real(8),    save, dimension(mc)                 :: canmx    ! maximum canopy storage for land use type
   integer, save, dimension(mc)                    :: veg_code ! vegetation code (crop.dat) for each land use class, read from input file

!**** Parameters for random number generator
    integer, save, dimension(4)                    :: k1  = (/9, 98, 915, 92/)
    integer, save, dimension(4)                    :: k2  = (/135, 28, 203, 85/)
    integer, save, dimension(4)                    :: k3  = (/43, 54, 619, 33/)
    integer, save, dimension(4)                    :: k4  = (/645, 9, 948, 65/)
    integer, save, dimension(4)                    :: k5  = (/885, 41, 696, 62/)
    integer, save, dimension(4)                    :: k6  = (/51, 78, 648, 0/)
    integer, save, dimension(4)                    :: k7  = (/227, 57, 929, 37/)
    integer, save, dimension(4)                    :: k8  = (/20, 90, 215, 31/)
    integer, save, dimension(4)                    :: k9  = (/320, 73, 631, 49/)
    integer, save, dimension(4)                    :: k10 = (/73, 24, 881, 52/)
    integer, save, dimension(4)                    :: k11 = (/6, 302, 597, 3/)
   
    real(8), save, dimension(366,100)              :: runo   ! readcli: water discharge in the basin outlet, m3/sec
    integer, save, dimension(100)                  :: obssb  ! subbasin numbers of observed discharge
    integer, save                                  :: nqobs = 1  ! number of observed discharge columns, default 1 (only outlet)

!**** Distribution of soil types into 4 soil groups (for CN method)
!     Done according to Leitpro-4 parameters (BUEK-1000)
!     A: 1,2,6,10,12,17,20,22,24,25,26,28,31,33,34,45,53,55,57,63,70,72
!     B: 7,13,15,19,23,29,32,36,37,40,42,43,44,46,47,48,49,56,59,61,64,71
!     C: 3,4,5,8,9,11,14,27,41,58
!     D: 38,51,65,66
!     All czech soils are preliminarily assigned to group B, check later!

!  alt: Saale

 
! Elbe / Zschopau   
!   integer, save, dimension(ms) ::  nsolgr =(/1,1,3,3,3,1,2,3,3,1, &
!                 3,1,2,3,2,0,1,0,2,1, &
!                 0,1,2,1,1,1,3,1,2,0, &
!                 1,2,1,1,0,2,2,4,0,2, &
!                 3,2,2,2,1,2,2,2,2,0, &
!                 4,0,1,0,1,2,1,3,2,0, &
!                 2,0,1,2,4,4,0,0,0,1, &
!                 2,1,2,3,4,3,2,2,2,4, &
!                 3,1,2,4,2,1,2,2,2,2, &
!                 2,2/)

!C**** Crop indices 
      integer, parameter :: iww   = 45
      integer, parameter :: iwb   = 36
      integer, parameter :: iwr   = 42
      integer, parameter :: isba  = 22
      integer, parameter :: imai  = 25
      integer, parameter :: ipo   = 20
      integer, parameter :: irp   = 40
      integer, parameter :: icc   = 51
      integer, parameter :: ifod  = 59
      integer, parameter :: ifoe  = 60
      integer, parameter :: ifom  = 61
      integer, parameter :: imea  = 46
      integer, parameter :: ipas  = 47
      integer, parameter :: iwet  = 55
      integer, parameter :: iwetf = 63
      integer, parameter :: ihei  = 50


!*****************************************************************************

CONTAINS

!*****************************************************************************


!*****************************************************************************
integer function get_days_in_month(month,year)
   integer, intent(in) :: month,year
   integer, dimension(12) :: ndays,ndays_leap

   get_days_in_month = 0
   ndays = (/31,28,31,30,31,30,31,31,30,31,30,31/)
   ndays_leap = (/31,29,31,30,31,30,31,31,30,31,30,31/)
   if ( leapyear(year) ) then
      get_days_in_month = ndays_leap(month)
   else
      get_days_in_month = ndays(month)
   end if

end function get_days_in_month
!*****************************************************************************

!*****************************************************************************
integer function get_day_of_month(month,year)
   integer, intent(in) :: month,year
   integer             :: n

   ! ida       = current day (day of year) used in SWIM as global variable
   ! nc(month) = number of days passed in the beginning of month (common.f90) global variable in SWIM
   n = nc(month)
   if ( .NOT.leapyear(year) .AND. month > 2 ) n = n - 1
   get_day_of_month = ida - n

end function get_day_of_month
!*****************************************************************************

!*****************************************************************************
logical function leapyear(year)
implicit none
! returns .true. if year is a leap year, otherwise .false.
   integer, intent(in) :: year
   logical :: cond1, cond2, cond3

   cond1 = .false.
   cond2 = .false.
   cond3 = .false.
   leapyear = .true.

  if (mod(year,4) == 0)   cond1 = .true.
  if (mod(year,100) == 0) cond2 = .true.
  if (mod(year,400) == 0) cond3 = .true.

  if (cond1 .eqv. .false.) then
     leapyear = .false.
  else
     if ((cond2).and.(cond3 .eqv. .false.)) leapyear = .false.
  end if

end function leapyear
!*****************************************************************************

!*****************************************************************************
integer function count_nbr_rows(funit,header)
IMPLICIT NONE
! Author: stefan.liersch@pik-potsdam.de
! Date: 2009-09-20
! PURPOSE: counts the number of rows in input file with given file unit
! The file must already be open!!!
   integer, intent(in)          :: funit  ! file unit
   logical, intent(in)          :: header ! true if input file contains a header
   integer                      :: status ! file status
   integer                      :: n      ! counter
   character(len=1)             :: a      ! dummy for reading

   status = 0
   n = 0
   ! rewind file in order to be sure to read the file from the first line
   rewind(funit)

   ! count number of rows in file: fname
   do
      read(funit,*,IOSTAT=status) a
      if (status /= 0) EXIT
      n = n + 1
   end do

   rewind(funit)

   count_nbr_rows = n
   if (header) count_nbr_rows = n - 1

end function count_nbr_rows
!*****************************************************************************


!*****************************************************************************
function get_ihouts(nsubs, subnr)
   ! Function returns an array with the hydrograph storage locations (.fig file)
   ! for subbasins listed in subnr
   integer, intent(in) :: nsubs             ! number of subbasins asking for ihout
   integer ,intent(in) :: subnr(nsubs)      ! 1-dim array containing subbasin numbers
   integer             :: get_ihouts(nsubs) ! array of hydrograph storage locations
   integer             :: i
   
   get_ihouts = 0
   
   ! identify hydrograph storage location for each output gauge (subbasin)
   do i = 1, mhyd
      if ( icodes(i) == 2 ) then ! ROUTE command
         do j = 1, nsubs
            if ( inum1s(i) == subnr(j) ) then
               ! The desired hydrograph storage location is always the next (+1)
               !ihouts_out(j) = ihouts(i) + 1
               get_ihouts(j) = ihouts(i) + 1
            end if
         end do
      end if
   end do

   ! Some subbasins will certainly not be captured by the algorithm above.
   ! Normally these are headwater subbasins.
   ! The following loop captures those headwater subbasins.
   do j = 1, nsubs
      if (get_ihouts(j) == 0) then
         get_ihouts(j) = subnr(j)
      end if
   end do
   
end function get_ihouts
!*****************************************************************************



!*****************************************************************************
end module common_par
!*****************************************************************************
