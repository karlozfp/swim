!     FILE readbas.f
!
!     SUBROUTINES IN THIS FILE          CALLED FROM
!     subroutine  readbas               main
!     subroutine  readbsnswitches      main



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

subroutine readbsnswitches
use common_par
use mod_reservoir !#### RESERVOIR MODULE ####
use mod_snow      !#### SNOW MODULE ####
implicit none

   character(len=80) :: titldum
   integer           :: subcatch_switch, landmgt_switch, res_switch
   integer           :: snow_switch, dormancy_switch, runoffDat_switch, subfiles_switch

!**** READ 4 - basin data (unit 4 - basin_name.bsn)
   !open(4,file=trim(inputPath)//basndat, status='old', ERR=83)
   read(4,*) titldum
   read(4,*) isc
   read(4,*) icn
   read(4,*) idlef
   read(4,*) iicep
   read(4,*) iemeth
   read(4,*) idvwk
   read(4,*) subcatch_switch  ! subcatchment calibration
   read(4,*) landmgt_switch   ! crop rotations / land management parameters from external file
   read(4,*) res_switch       ! Reservoir switch
   read(4,*) snow_switch      ! Reservoir switch
   read(4,*) radiation_switch ! 0 = read radiation data from clim1; 1 = calculate radiation after Hargreaves (latitude in degrees in file stat-outdat.csv) required!
   read(4,*) dormancy_switch  ! day length threshold for dormancy of natural vegetation: 0 = do not limit or extent; 1 = calculate from subbasin latitude
   read(4,*) runoffDat_switch ! 0 = do not read runoff.dat; 1 = read runoff.dat (required only to calculate performance criteria during runotime)
   read(4,*) subfiles_switch  ! 0 = read single .sub, .rte, .gw files from 'Sub' directory; 1 = read only three files from directory 'Sub'

   !write(*,*) 'BASIN PARAMETERS & CODES:'
      
   bSubcatch = .false.
   if (subcatch_switch > 0) then
      bSubcatch = .true.
      write(*,*) "Subcatchment calibration switched ON"
   end if

   bLandMgt = .false.
   if (landmgt_switch > 0) then
      bLandMgt   = .true.
      write(*,*) "Land management (landmgt.csv) switched ON"
   end if

   bRsvModule = .false.
   if (res_switch > 0) then 
      bRsvModule = .true.
      write(*,*) "Reservoir module switched ON"
   end if
   
   if ( radiation_switch > 0 ) write(*,*) "Radiation is calculated according to Hargreaves from subbasin latitude"

   bSnowModule = .false.
   if (snow_switch > 0) then 
      bSnowModule = .true.
      write(*,*) "Snow module switched ON"
   end if
   
   bDormancy = .false.
   if (dormancy_switch > 0) then 
      bDormancy = .true.
      write(*,*) "Dormancy threshold for natural vegetation based on subbasin latitude"
   end if 
   
   bRunoffDat = .false.
   if ( runoffDat_switch > 0 ) bRunoffDat = .true.

   b3SubFiles = .false.
   if (subfiles_switch > 0) then 
      b3SubFiles = .true.
      write(*,*) "Reading subbasin parameters from 3 input files in directory <Sub>"
   end if 

end subroutine readbsnswitches
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


subroutine readbas
!**** PURPOSE: THIS SUBROUTINE READS:
!              1) BASIN DATA from basin_name.bsn and
!              2) HYDROTOPE STRUCTURE from basin_name.str (& reads str.cio)
!**** CALLED IN:  MAIN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      PARAMETERS & VARIABLES
!
!      >>>>> COMMON PARAMETERS & VARIABLES
!      abf0    = alpha factor for groundwater (global, 1 for basin)
!      af      = 1000. * da, basin area (1000*km**2)
!      bff     = baseflow factor for basin, used to cal! subsurface flow
!      chcc0   = correction coef. for chnnel USLE ! factor
!      chwc0   = coefficient to correct channel width
!      chxkc0  = correction coef. for chnnel USLE K factor
!      cnum1   = init. CN for cropland, cond 1
!      cnum2   = init. CN for cropland, cond 2
!      cnum3   = init. CN for cropland, cond 3
!      co2ref  = atm CO2 in the reference period, ppm
!      co2sce  = atm CO2 in the scenario period, ppm
!      da      = basin area, km**2
!      da9     = 100. * da = basin area in ha
!      degngrw = N degradation in groundwater, 1/day
!      degnsub = N degradation in subsurface flow, 1/day
!      degnsur = N degradation in surface flow, 1/day
!      degpsur = P degradation in surface flow, 1/day
!      ecal    = general calibration factor for potential evap
!      ekc0    = soil erodibility correction factor
!      frar(j,je) = fractional area of hydrotope je in subbasin j
!      gwq0    = initial groundwater flow contribution to streamflow
!                (1 for basin)
!      ialpha  = switch parameter: to cal! CO2 effect on net photosynthesis?
!      ibeta   = switch parameter: to cal! CO2 effect on transpiration?
!      ic3c4   = switch parameter: 3/4 - C3 or C4 crop?
!      icn     = switch code for CN: 0: CN dif for soils (standard method)
!                                    1: CN=const from cnum1, cnum3
!      idlef   = code for the day length effect in crop: 0 - without, 1 - with
!      iicep   = switch for including interception module, 0 - no, 1 - yes
!      is!     = code for saturated cond.: 0 - read, 1 - calc
!      idvwk   = code for evaporation calculus: 0 - classic, 1 - DVWK-M 238
! block k1,..., k9 = parameters for random number generator
!      mstruc(j,je,2) = HRU structure vector: land use and soil in HRU
!      nbyr    = number of years to simulate
!      nbr_hru_sub(j) = number of HRUs in subbasin
!      prcor   = correction factor for precipitation (currently not used)
!      prf     = coef. to estimate peak runoff in stream
!      rdcor   = correction factor for root depth (currently not used)
!      retngrw = N retention in groundwater, days
!      retnsub = N retention in subsurface flow, days
!      retnsur = N retention in surface flow, days
!      retpsur = P retention in surface flow, days
!      roc1    = coef. for routing: correction coef to calculate xkm
!      roc2    = coef. for routing: correction coef to calculate xkm
!      roc3    = coef. for routing: correction coef to calculate xkm
!      roc4    = coef. for routing: correction coef to calculate xkm
!      subarea(j) = subbasin area, m2
!      sccor   = correction factor for saturated conductivity (all soils)
!      smrate  = snow melt rate for the degree-day method
!      snow1   = initial snow content, mm
!      spcon   = rate parameter for estimation of sediment transport
!      spexp   = exponent for estimation of sediment transport
!      stinco  = init. water content coef., later stin()=fc()*stinco
!      storc1  = storage correction coef
!      th!     = correction factor for thermal atmospheri! back-radiation
!                range for thc: (0.8-1.0), value 1. specified by R. Muttiah
!      tmelt   = threshold temperature for snow melt
!      tsnfall = threshold temperature for snow fall
! block ub      = 3.065 (water use rate-depth parameter), used in wstress
!      uob     = function of ub(ub = water use rate-depth parameter)
!      >>>>>

!      >>>>> STATI! PARAMETERS
!      ar      = hydrotope area, m2
!      j,subb  = subbasin
!      jea     = hydrotop number
!      soilnum = soil
!      lut     = land use
!      ncell   = number of cells
!      titldum = title (dummy)
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      use mod_snow      !#### SNOW MODULE ####
      implicit NONE
      integer :: i,j,jea,mbeap
      character(len=80) ::  titldum
      character(len=1)  :: a
      ! mstruc variables
      integer :: lu,soil,wet,mgt,elev,glac,ncell
      real(8)    :: ar

      ! coefs to read in from *.bsn (if not individual subbasin parameters for subcatch)
      real(8) :: ecal_,thc_,cncor_,roc2_,roc4_
      real(8) :: tsnfall_,tmelt_,smrate_,gmrate_
      real(8) :: bff_

      write(6,*) 'BASIN PARAMETERS & CODES:'

!**** READ 4 - basin data (unit 4 - basin_name.bsn)
      read(4,99) titldum
      read(4,99) titldum
      read(4,99) titldum
      read(4,*) da
      read(4,99) titldum
      read(4,*) cnum1,cnum2,cnum3
      read(4,99) titldum
      read(4,*) ecal_,thc_, epco, ec1
      read(4,99) titldum
      read(4,*) gwq0,abf0,bff_
      read(4,99) titldum
      read(4,*) ekc0,prf,spcon,spexp
      read(4,99) titldum
      read(4,*) snow1,storc1,stinco  
      read(4,99) titldum
      read(4,*) chwc0,chxkc0,chcc0,chnnc0
      read(4,99) titldum
      read(4,*) roc1,roc2_,roc3,roc4_
      read(4,99) titldum
      read(4,*) sccor_,prcor,rdcor 
      read(4,99) titldum
      read(4,*)  retNsur,retNsub,retNgrw,retPsur
      read(4,99) titldum
      read(4,*)  degNsur,degNsub,degNgrw,degPsur
      read(4,99) titldum
      read(4,*)  tsnfall_,tmelt_,smrate_,gmrate_
      read(4,99) titldum
      read(4,*)   xgrad1,tgrad1,ulmax0,rnew
      tmelt0 = tmelt_
      read(4,99) titldum
      read(4,*)  tlrch,evrch,tlgw
      read(4,99) titldum
      read(4,*)  rzmaxup    
      read(4,99) titldum
      read(4,99) titldum
      read(4,99) titldum
      read(4,99) titldum
      read(4,99) titldum
      read(4,*)  ialpha, ibeta, ic3c4, co2ref, co2sce

      write(32,*) 'READBAS parameters, file No.2'
      write(32,107) nbyr, da
      write(32,108) bff
      write(32,109) k1, k2, k3, k4, k5, k6, k7, k8, k9      
      af = 1000. * da
      da9 = 100. * da
      uob = 1. - exp(-ub)

! sl begin
      ! If subcatch function is not active (=0) then following parameters
      ! are set to global values read from *.bsn.
      ! Otherwise subbasin specific values are assigned in subcatch.f90
      if ( .NOT. bSubcatch ) then
         ecal    = ecal_
         thc     = thc_
         sccor   = sccor_
         roc2    = roc2_
         roc4    = roc4_
         bff     = bff_
         tsnfall = tsnfall_
         tmelt   = tmelt_
         smrate  = smrate_
         gmrate  = gmrate_
      end if
      ! In the SWIM manual it is suggested to set roc1 and roc3 values to 0.      
      roc1 = 0.
      roc3 = 0.
! sl end

!**** WRITE
      if (isc.eq.0) write(6,*) 'Codes: SC - read, isc =', isc
      if (isc.eq.1) write(6,*) 'Codes: SC - calculated, isc =', isc

      if (icn.eq.0) write(6,*) 'Codes: CN - standard, icn =', icn
      if (icn.eq.1) write(6,*) 'Codes: CN = f(cnum1,cnum3), icn =', icn

      if (iicep.eq.1) write(6,*) 'Codes: Interception - yes, iicep =', iicep
      if (iicep.eq.0) write(6,*) 'Codes: Interception - no, iicep =', iicep

      if (iemeth.eq.0) write(6,*) 'Codes: Evaporation - Priestley-Taylor, iemeth =', iemeth
      if (iemeth.eq.1) write(6,*) 'Codes: Evaporation - Turc-Ivanov, iemeth =', iemeth

      if (idvwk.eq.1) write(6,*) 'Codes: Evaporation variables after DVWK-M 238, idvwk =', idvwk
      if (idvwk.eq.0) write(6,*) 'Codes: Evaporation variables from legacy method, idvwk =', idvwk

!      if (idlef.eq.0) write(6,*) 'Codes: WITHOUT the day length factor'
!      if (idlef.eq.1) write(6,*) 'Codes: WITH the day length factor'

      write(6,102)
      write(6,106)sccor_,gwq0,abf0       
      write(6,103)
      write(6,106)chwc0,roc2_,roc4_
      write(6,104)
      write(6,116)retNsur,retNsub,retNgrw,retPsur    
      write(6,105)
      write(6,106)degNsur,degNsub,degNgrw,degPsur
      write(6,120)
      write(6,121)ialpha, ibeta, ic3c4, co2ref, co2sce
          

!**** READ HYDROTOPE STRUCTURE & WRITE SEPARATELY for every subbasin 
!     READ 7 - basin_name.str (Hydrotope structure) 
!     READ 8 - str.cio (file names for subbasins structure) & write in files 9
      mbeap = mb*meap
      maxhru = 0
      open(7,file=trim(hydrotopePath)//struct, status='old', ERR=84)
      read(7,*) a ! skip header line

      do i = 1,mbeap
         read(7,*) j,lu,soil,mgt,wet,elev,glac,ar,ncell
         if ( j > 0 ) then
            neap(j) = neap(j) + 1  ! number of HRUs per subbasin
            sbar(j) = sbar(j) + ar ! total subbasin area in m^2

            if(neap(j).gt.meap) goto 81
            if(j.gt.mb) goto 82

            frar(j,neap(j))     = ar   ! area in m^2 !!! recalculated below as fraction of subbasin
            mstruc(j,neap(j),1) = lu   ! land use ID
            mstruc(j,neap(j),2) = soil ! soil type ID
            mstruc(j,neap(j),3) = wet  ! wetland
            mstruc(j,neap(j),4) = mgt  ! LU management
            mstruc(j,neap(j),5) = elev ! HRU mean elevation
            mstruc(j,neap(j),6) = glac ! initial glacier depth

            if(neap(j).gt.maxhru) maxhru = neap(j)
         else
            EXIT
         end if ! (j > 0)
      end do ! i = 1,mbeap
      close(7)

      write(6,*) 'Actual number of subbasins = ',mb
      write(6,*) 'Actual max number of hydrotopes = ', maxhru
      write(32,*) 'NUMBER of HRUs in SUBBASINS =',(neap(j),j=1,mb)
      write(32,*) 'SUBBASIN AREA =',(sbar(j),j=1,mb)
      write(32,*) 'MSTRUC'
      do j = 1,mb
         ! fraction of subbasin area of total catchment area
         flu(j) = sbar(j) / (da*10**6)
         do jea = 1,neap(j)
            ! fraction of hydrotop area of subbasin area
            frar(j,jea)=frar(j,jea)/sbar(j)
            write(32,101) j,jea,mstruc(j,jea,1),mstruc(j,jea,2),frar(j,jea)
         end do
      end do

      write(6,*) '===> Basin parameters & Hydrotope structure - READ!'
      write(6,*) ' '       

  101 format (4i5,f10.3)
  102 format('     sccor      gwq0      abf0')      
  103 format('     chwc0      roc2      roc4')
  104 format('   retNsur   retNsub   retNgrw   retPsur')   
  105 format('   degNsur   degNsub   degNgrw   degPsur')
  106 format (4f10.3)      
  116 format (4f10.0)      
  107 format (t10,'NO YRS = ',i4/t10,'BASIN AREA = ',f9.3,' KM**2')
  108 format (t10,'BASEFLOW FACTOR = ',f8.3)
  109 format (//t10,'GENERATOR SEEDS'/(15x,4i4)/)
  120 format('    ialpha     ibeta     C3/C4   CO2-ref  CO2-scen')
  121 format(2i10, 3f10.0)
   99 format (a)
      return
      
   81 continue
      write(6,*)'ERROR in readbas:'
      write(6,*)'   number of hydrotopes = ',neap(j),' in subbasin ',j
      write(6,*)'   > meap - max number specified in common.f'
      write(6,*)'SWIM STOPS'
      pause
      stop     
   82 continue
      write(6,*)'ERROR in readbas:'
      write(6,*)'   number of subbasins ',j,' > mb - max number'
      write(6,*)'SWIM STOPS'
      pause
      stop     
   84 continue
      write(6,*)'ERROR in readbas:'
      write(6,*)'   reopening of str-file failed '
      pause
      stop     
   
      end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
