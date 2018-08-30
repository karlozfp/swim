!     FILE hydrotop.f
!
!     SUBROUTINES IN THIS FILE       CALLED FROM
!     subroutine  hydrotop(j,jea,k,n)   subbasin
!     subroutine  init_mgt(j,jea)    hydrotop



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine hydrotop(j,jea,k,n,wet)
!**** PURPOSE: THIS SUBROUTINE CALCULATES ALL PROCESSES in HYDROTOPES
!**** CALLED IN:  SUBBASIN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!
!   >>>>> COMMON PARAMETERS & VARIABLES
!   bcv(j,je)   = lag factor (residue and snow effect on temp)
!   canstor(j,je)  = canopy water storage, mm
!   cn       = current CN
!   cn2(k,n,j) = Curve Numbers for soil k, land use n, and subbasin
!   cva(j,je)   = vegetation cover, kg/ha
!   dflow(j,je,20) = monthly flows for water and N (see writhru.f)
!   eo       = potential evapotranspiration, mm
!   ep       = plant transpiration, mm
!   es       = soil evaporation, mm
!   icc      = index for cover crop corr. number in crop database
!   ida      = current day
!   idfe(n,if)     = day of fertilization, if - number of fertilisation
!   FORD        = index for deciduous forest corr. number in crop database
!   FORE        = index for evergreen forest corr. number in crop database
!   FORM        = index for mixed forest corr. number in crop database
!   RNGB        = index for heather corr. number in crop database
!   imea        = index for meadows corr. number in crop database
!   ipas        = index for pasture corr. number in crop database
!   iwet        = index for wetland corr. number in crop database
!   iwetf       = index for forested wetland corr. number in crop database
!   nn       = number of soil layers
!   nveg(j,je)     = number of vegetation from crop database
!   percn       = N-NO3 leaching to g-w, kg/ha
!   precip      = precipitation, mm
!   preinf(j,je)   = precipitation adjusted for canopy storage, mm
!   prk      = lateral subsurface flow, mm, calc in perc for 4mm layers
!   qd       = surface flow in HYDROTOPE, mm
!   qi       = surface flow in HYDROTOPE, mm
!   rain        = preinf(j,je) - qd, mm
!   sep      = percolation, mm
!   sml      = snow melt, calc in snom(), mm
!   snoa(j,jea)    = snow water content in HYDROTOPE, mm
!   snoev       = snow evap. in HYDROTOPE, mm
!   ssf      = subsurface flow in HYDROTOPE, mm
!   ssfn        = N-NO3 in subsurface flow, kg/ha
!   ste(j,je,l)    = water storage, recalc here, mm
!   strsn       = N stress for plant growth
!   strsp       = P stress for plant growth
!   sumfc(k)    = sum of field capacity in soil, mm
!   swe(j,je)   = soil water content, mm
!   swind       = soil water index for hydrotope: swind=swe()/sumfc()
!   ts       = temp. stress
!   tx(j)       = daily average temperature in the subbasin, degree C
!   uno3        = N uptake by plants, kg/ha
!   vb       = CN max - for output in main
!   vl       = CN min - for output in main
!   ws(j,je)    = water stress factor
!   yno3        = N-NO3 loss with surface flow, kg/ha
!   ysp      = soluble P leaching, kg/ha
!   >>>>>

!   >>>>> STATI! PARAMETERS
!   ii  = local par
!   l   = local par
!   xx  = local par
!   zz  = local par
!   >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
use mod_snow      !#### SNOW MODULE ####
implicit NONE
   integer :: j   ! subbasin number
   integer :: jea ! number of hydrotope in subbasin
   integer :: n   ! land use number
   integer :: k   ! soil number
   integer :: wet ! wetland code 0/1
   integer :: ii,l
   real(8)    :: xx,zz

   !###########################
   !#### SNOW MODULE       ####
   !###########################
   if ( bSnowModule ) then
     tx_tmp = tmit
   else
     tx_tmp = tx(j)
   end if
   !###########################

!**** INITIALIZATION       
   qd = 0.
   qi = 0.
   ssf = 0.
   sep = 0.
   ep=0.
   es=0.
   snoev = 0.
   ysp = 0.
   yno3 = 0.
   uno3 = 0.
   ssfn = 0.
   percn = 0.
   ws(j,jea) = 1.
   ts = 1.
   strsn = 1.
   strsp = 1.

!**** INITIZALIZE MANAGEMENT OPERATIONS
!sl begin
   if ( bLandMgt .AND. n == 5 ) then
      call init_cropland_mgt(j,jea)
   end if
!sl end

!#### CALL CURNO: to set Curve Number parameters
   if ( bSubcatch ) then
      cn2(k,n,j) = cn2(k,n,j) * cncor(j)
      if ( cn2(k,n,j) * cncor(j) > 100.) cn2(k,n,j) = 100.
      if ( cn2(k,n,j) * cncor(j) < 1.)   cn2(k,n,j) = 1.
   end if
   call curno(cn2(k,n,j),j,jea,k,n)

!**** CALC bcv() - lag factor for soil temperature
   bcv(j,jea) = cva(j,jea) / (cva(j,jea) + exp(7.563-1.297e-4*cva(j,jea)))
   if (snoa(j,jea).gt.0.) then
      if (snoa(j,jea).le.120.) then
         xx = snoa(j,jea) / (snoa(j,jea)+exp(6.055-.3002*snoa(j,jea)))
      else
         xx = 1.
      end if
      bcv(j,jea) = amax1(xx,bcv(j,jea))
   end if

!#### CALL SOLT - COMPUTE TEMP. of SOIL LAYERS
   call solt(zz,j,jea,k,n)
           

!#### CALL VOLQ TO CALC RUNOFF VOLUME, 
!#### CALL ECKLSP TO CALC COMBINED CKLSP factor for hydrotope
      ! preinf = precipitation adjusted for canopy storage
      !        = precipitation + snow melt + canopy water storage

   !###########################
   !#### SNOW MODULE       ####
   !###########################
   if ( bSnowModule ) then
      !preinf(j,jea) = precipe + canstor(j,jea)
      !preinf(j,jea) = precipe + canstor(j,jea) + vsn
      preinf(j,jea) = precipe + canstor(j,jea)
      !if ( vsn > 0. ) preinf(j,jea) = vsn + canstor(j,jea)
   else
     preinf(j,jea) = precip + sml + canstor(j,jea)
   end if
      !preinf(j,jea) = precip + sml + canstor(j,jea)
      !###########################

   if ( tx_tmp  > 0.) then
     if ( preinf(j,jea) > 0. ) then
        call volq(j,jea,k,n)
        qi=qd
        call ecklsp(j,jea,k,n)
        if (cn.ge.vl) then
           if (cn.lt.vb) go to 40
              vb = cn
        else
           vl = cn
        end if                  
     end if      
   end if
   40 continue
   
!#### CALL PURK TO PERFORM SOIL WATER ROUTING 
   rain = preinf(j,jea) - qd
   call purk(j,jea,k)
   ssf = prk
      
   !#### CALC swe = soil water content [mm]
   ! swe is re-calculated after vegetation growth below
   ! ste = water storage per layer [mm], calculated in purk
   swe(j,jea)= 0.
   swe(j,jea) = sum(ste(j,jea,:))
   
   call evap(j,jea,k,n)
   ! re-calc soil water content
   swe(j,jea)= 0.
   swe(j,jea) = sum(ste(j,jea,:))

   !#### CALL FERT, CRPMD & VEGMD to calculate vegetation growth & fertilisation
   !     LAND USE TYPES in SWIM:
   !     n =  1 - water
   !     n =  2 - settlement
   !     n =  3 - industry
   !     n =  4 - road
   !     n =  5 - cropland
   !     n =  6 - set-aside (Brache)
   !     n =  7 - grassland, extensive use (meadow, Wiese)
   !     n =  8 - grassland, intensive use (pasture, Weide)
   !     n =  9 - forest mixed
   !     n = 10 - forest evergreen
   !     n = 11 - forest deciduous
   !     n = 12 - wetland nonforested
   !     n = 13 - wetland forested
   !     n = 14 - heather (Heide, grass + brushland)
   !     n = 15 - bare soil

   select case(n)
      case (1)    ! WATER
      es = eo ! = ETact = potential evapotranspiration

      case (2,3,4,15)    ! NO VEGETATION --> plant transpiration = 0.
      ep = 0.

      case (5) !#### CROPLAND: CALL FERT & CRPMD
      do ii = 1,mfe
         if ( ida == idfe(n,ii) ) call fert(j,jea,n,k,ii)
      end do
      call crpmd(j,jea,k,n,wet)
      call leaching(j,jea,k,n,wet)

      case (6) !#### SET-ASIDE, GRASS: CALL VEGMD
      nveg(j,jea) = veg_code(6) ! icc
      call vegmd(j,jea,k,n,wet)
      call leaching(j,jea,k,n,wet)

      case (7) !#### GRASSLAND, EXTENSIVE USE (MEADOW): CALL VEGMD
      nveg(j,jea) = veg_code(7) ! imea ! OR use 50 = RNGE instead
      call vegmd(j,jea,k,n,wet)
      call leaching(j,jea,k,n,wet)

      case (8) !#### GRASSLAND, INTENSIVE USE (PASTURE): CALL VEGMD
      nveg(j,jea) = veg_code(8) ! ipas
      call vegmd(j,jea,k,n,wet)
      call leaching(j,jea,k,n,wet)

      case (9) !#### FOREST MIXED: CALL VEGMD
      nveg(j,jea) = veg_code(9) !  ifom ! FORM
      call vegmd(j,jea,k,n,wet)
      call leaching(j,jea,k,n,wet)

      case (10) !#### FOREST EVERGREEN: CALL VEGMD
      nveg(j,jea) = veg_code(10) ! ifoe ! FORE
      call vegmd(j,jea,k,n,wet)
      call leaching(j,jea,k,n,wet)

      case (11) !#### FOREST DECIDUOUS: CALL VEGMD
      nveg(j,jea) = veg_code(11) ! ifod ! FORD
      call vegmd(j,jea,k,n,wet)
      call leaching(j,jea,k,n,wet)

      case (12) !#### WETLAND NONFORESTED: CALL VEGMD
      nveg(j,jea) = veg_code(12) !  iwet
      call vegmd(j,jea,k,n,wet)
      call leaching(j,jea,k,n,wet)

      case (13) !#### WETLAND FORESTED: CALL VEGMD
      nveg(j,jea) = veg_code(13) !  iwetf
      call vegmd(j,jea,k,n,wet)
      call leaching(j,jea,k,n,wet)

      case (14) !#### HEATHER (HEIDE): CALL VEGMD
      nveg(j,jea) = veg_code(14) ! ihei !RNGB
      call vegmd(j,jea,k,n,wet)
      call leaching(j,jea,k,n,wet)
   end select

             
!**** RE-CALC SOIL WATER CONTENT swe() & SOIL WATER INDEX swind
   swe(j,jea) = 0.
   swe(j,jea) = sum(ste(j,jea,:))

   swind = swe(j,jea)/sumfc(k)

   ! dflow(j,je,20) = monthly flows for water and N (see writhru.f)
   dflow(j,jea,1) = dflow(j,jea,1)+ qd
   dflow(j,jea,2) = dflow(j,jea,2)+ ssf
   dflow(j,jea,3) = dflow(j,jea,3)+ sep
   dflow(j,jea,4) = dflow(j,jea,4)+ es + ep

end subroutine hydrotop

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

subroutine leaching(j,jea,k,n,wet)
use common_par
implicit none
   integer, intent(in) :: j,jea,k,n,wet
   !#### CALC NITRATE LEACHING, SOLUBLE P LEACING, N & P CYCLES
   call nlch(j,jea,k,n)
   call psollch(j,jea,k,n)
   call ncycle(j,jea,k,n)
   call pcycle(j,jea,k,n)
end subroutine leaching

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

subroutine init_cropland_mgt(nsub,nhru)
!-------------------------------------------------------------------------------
! Author  : stefan.liersch@pik-potsdam.de
! Date    : 2009-11-25
! modified: 2009-12-08
!
! PURPOSE : THIS SUBROUTINE INITIALIZES CROP MANAGEMENT DATA AT HRU LEVEL
!
! CALLED  : from subroutine hydrotop
!-------------------------------------------------------------------------------

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!
!      >>>>> COMMON PARAMETERS & VARIABLES
!      fen(5,if)  = amount of min N fertilizers applied, kg N/ha
!      feno(5,if) = amount of org N fertilizers applied, kg N/ha
!      fep(5,if)  = amount of P fertilizers applied, kg P/ha
!      icc        = index for cover crop corr. number in crop database
!      idfe(5,if) = day of fertilization
!      idop(5,io) = day of operation
!      iopc(5,io) = operation code: 1 - planting, 2 - harvest & kill
!                                   3 - harvest,  4 - kill

use common_par
implicit none
   integer, intent(in) :: nsub,nhru
   integer             :: i = 0, j = 0
   ! number of years of rotation (3) + one initialization year (year 0)
   integer             :: rotation = nrotyrs + 1
   integer             :: management

   ! find the correct index for cropland management
   ! this depends on the mgt_id and current rotation year
   management = mstruc(nsub,nhru,4)
   i = management * rotation - rotation + 1
   if ( iy > 1 ) i = i + iyrrot ! iyrrot is calculated in mainpro

   cur_nop = mgt_nop(i) ! hydrotop-specific number of mgt operations

   do j = 1, mgt_nop(i)
      idop(5,j) = mgt_idop(i,j)
      iopc(5,j) = mgt_iopc(i,j)
      ncrp(5,j) = mgt_ncrp(i,j)
      idfe(5,j) = mgt_idfe(i,j)
      fen(5,j)  = mgt_fen(i,j)
      feno(5,j) = mgt_feno(i,j)
      fep(5,j)  = mgt_fep(i,j)
   end do

end subroutine init_cropland_mgt

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
