!-------------------------------------------------------------------------------
! Snow module (snow)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Author  : Shaochun Huang
! Version : 0.1
! Edited by: stefan.liersch@pik-potsdam.de
! Modified: 2015-02-07:
!-------------------------------------------------------------------------------

module mod_snow
use common_par

implicit none


!#############################################################################
!
! VARIABLES & PARAMETERS
!
!#############################################################################

logical, save                                :: bSnowModule

! ! Day when snow is converted into ice and output is written into GIS file
integer, parameter                           :: gla_day_out = 273 ! This might be different in different regions
! ! Daily factor used to convert accumulated snow (snoa) to glacier ice, dimensionless.
! !real(8),    parameter                           :: snow2ice = 0.001
! real(8),    parameter                           :: snow2ice = 1./365.25
! 
! ! Total glacier melt (gla_melt_bottom = melt from snow on top of the glacier), mm.
! real(8), save                                   :: gla_melt_total
! 
real(8), save                                   :: tmit      ! mean T, °C
real(8), save                                   :: tx_tmp    ! mean T, °C
real(8), save                                   :: tmax      ! Max T, °C
real(8), save                                   :: tmax_tmp  ! mean T, °C
real(8), save                                   :: tmin      ! Min T, °C
real(8), save                                   :: tmin_tmp  ! mean T, °C
real(8), save                                   :: precipe   ! Mean precipitation, mm
real(8), save                                   :: precip_elev_cor ! elevation-based corrected HRU precipitation, mm
real(8), save                                   :: hsn0      ! snow depth, cm
real(8), save                                   :: smle      ! snow melt, mm
real(8), save                                   :: gmle      ! glacier melt, mm

real(8), save                                   :: xprecip   ! weighted subbasin precipitation, mm
real(8), save                                   :: xprecip_elev_cor ! weighted subbasin elevation-based corrected precipitation, mm
real(8), save                                   :: xtmax     ! weighted subbasin max T, °C   
real(8), save                                   :: xtmit     ! weighted subbasin mean T, °C  
real(8), save                                   :: xtmin     ! weighted subbasin min T, °C  
real(8), save                                   :: xsml      ! weighted subbasin snow melt, mm
real(8), save                                   :: xsnow     ! weighted subbasin snow, mm
real(8), save                                   :: psnow     ! weighted subbasin snow fall as precpitation, mm      
real(8), save                                   :: vsn       ! hydrotope snow melt from snow pack, mm
real(8), save                                   :: xvsn      ! weighted subbasin snow melt from snow pack, mm

!real(8), save                                   :: gmrate    ! glacier melt rate for the degree-day method  NOW DEFINED IN COMMON.F90
real(8), save                                   :: xgrad1    ! precipitation factor
real(8), save                                   :: tgrad1    ! tempreture factor 
real(8), save                                   :: ulmax0    ! holding capacity 
real(8), save                                   :: rnew      ! fresh snow density 
real(8), save                                   :: tmelt0    ! 
! 
! real(8),save                                    :: runsb(6)
! 
real(8),    save, dimension(:,:),   allocatable :: rsn       ! snow water density in HYDROTOPE, (g/cm3)
real(8),    save, dimension(:,:),   allocatable :: sul       ! volumetric content of liquid water in snow (g/cm3)
real(8),    save, dimension(:,:),   allocatable :: suz       ! volumetric content of ice in snow (g/cm3)
real(8),    save, dimension(:,:),   allocatable :: gla       ! glacier water equivalent, mm


real(8), save                                   :: balanc,hsn,ulmax    ! for output only, otherwise local variables
! counter for GIS output
integer, save                                :: ieapg = 0  ! for glacier output (snow module)
!#############################################################################

!*****************************************************************************

CONTAINS

!*****************************************************************************

!------------------------------------------------------------------------------
subroutine snow_init

  open(350,file=trim(swimPath)//'Res/snow_module.out',status='REPLACE')
  if ( gis_y > 0 ) open(351,file=trim(swimPath)//'GIS/glacier.out',status='REPLACE')

  allocate(rsn(mb,maxhru))
  rsn = 0.
  allocate(sul(mb,maxhru))
  sul = 0.
  allocate(suz(mb,maxhru))
  suz = 0.
  allocate(gla(mb,maxhru))
  gla = 0.
  gla = mstruc(:,:,6) ! initialise glacier depth [mm]

  snoa = snow1

end subroutine snow_init
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
subroutine snow_initsub
   xprecip = 0.
   xprecip_elev_cor = 0.
   xtmax   = 0.
   xtmit   = 0.
   xtmin   = 0.
   xsml    = 0.
   xsnow   = 0.
   psnow   = 0.
   xvsn    = 0.
end subroutine snow_initsub
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 subroutine snow_calc_snowprocesses(j,jea)
   integer, intent(in) :: j,jea
   real(8)             :: elevh

   smle = 0.
   snow = 0.
   gmle = 0.
   elevh = mstruc(j,jea,5) ! hydrotope elevation

   ! elevation-based correction of precipitation and air temperature
   if ( precip > 0. ) then
      precipe = precip * (1. + xgrad1 * (elevh - elev0(j)))
   else
      precipe = 0.
   end if
   precipe = max(0.,precipe)
   precip_elev_cor = precipe

   tmit    = tx(j)  + tgrad1 * (elevh-elev0(j))
   tmax    = tmx(j) + tgrad1 * (elevh-elev0(j))
   tmin    = tmn(j) + tgrad1 * (elevh-elev0(j))

   ! rsn  = snow water density in HYDROTOPE, (g/cm3)
   ! snoa = snow water content, mm
   if ( rsn(j,jea) > 0. ) then
      hsn0 = snoa(j,jea) * .1 / rsn(j,jea) ! hsn0 = snow depth, cm
   else
      hsn0 = 0.
   end if

   if ( tmit <= tsnfall(j) ) then
     snoa(j,jea) = snoa(j,jea) + precipe * prcor! snow ! add precipitation to snow water content, mm
     snow        = precipe * prcor    ! actual snow fall, mm
     precipe     = 0.
   end if

   if ( snoa(j,jea) > 0. ) then
      call snow_snom(j,jea)
   else
      if ( gla(j,jea)  > 0. ) call snow_glacier(j,jea)
   end if

   if( gis_y > 0 .AND. j==isb1 .AND. jea==ih1 ) then
      if(ida==1.and.iy==1) write(350,127)
      write(350,130) iyr,ida,tx(j),tmit,precip,precipe,precip_elev_cor,snow,snoa(j,jea),gla(j,jea),gmle,vsn, &
         hsn*10.,rsn(j,jea),smle,sul(j,jea),suz(j,jea), &
         ulmax,balanc
   end if   
 
  127 format('Year DAY tmit tmit_cor precip precipe pcp_elev_cor snow snoa gla gmle vsn hsn rsn smle  sul  suz  ulmax   balance') 
  130 format(2i4,20f12.2)
  
end subroutine snow_calc_snowprocesses
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
subroutine snow_snom(j,jea)
   integer, intent(in) :: j,jea
   integer :: n
   real(8)    :: ph,smli,sml0,ulmax,tsn,sfreez,svz,svl,snoa0
   real(8)    :: precipe0,smr

   sfreez = 0.
   tsn = 0.
   ph = 0.
   svz = 0.
   svl = 0.
   smli = 0.
   sml0 = 0.
   hsn = 0.
   vsn = 0.
!    gla_melt_total = 0.
   !#### snow pack depth, hsn, hsn0 (last step), cm
   ! snow, mm
   ! hsn0, snow depth, cm  
   ! hsn, cm
   hsn      = hsn0 + snow * .1 / rnew   ! actual snow depth, cm
   snoa0    = hsn0 * rsn(j,jea) * 10.   ! previous day snow water content, mm
   precipe0 = precipe                   ! current day precipitation
   
   !#### different snowmeltrate for forest and opensite (cm/degree)
   smr = smrate(j)  ! cm/°C

   !#### snow temperature
   if ( tmit < 0. ) tsn = tmit
!#### calculate ulmax (holding capacity of snowpack)
   ulmax = ulmax0  ! 0 or 1
   if ( rsn(j,jea) /= 0. .AND. ulmax0 == 1.) ulmax = -0.11 * rsn(j,jea) + 0.11
   if ( rsn(j,jea) == 0. .AND. ulmax0 == 1.) ulmax = 0.
   ulmax = max(0.,ulmax)

!#### calculate intensity of snow melt (cm/day)      
   if ( tmit > tmelt(j) ) then
      smli = smr * (tmit-tmelt(j))  ! cm (cm/°C * °C )
   end if  

!#### calculate intensity of freezing of liquid water in snowpack (cm/day)
   if ( tmit < 0. )                     sfreez = smr * sqrt(-tmit)  ! cm/day
   if ( sfreez >= (sul(j,jea) * hsn0) ) sfreez = sul(j,jea) * hsn0  ! is actually g/cm2, similar to cm/day?

!#### calculate the compression sml0 (cm/day)
!#### the snowmelt water percolates into pack,hits the ice, and freezes until 
!#### the temperature of the pack is 0.       
   if (suz(j,jea) > 0.) then
      if (hsn0 > 0.) then
         ph = .15*rsn(j,jea)*hsn0**2./exp(-0.08*tsn+21.*rsn(j,jea))
      end if
      sml0 = smli/(suz(j,jea)*0.917)+ph
      if (sml0 > hsn) sml0 = hsn
      hsn = hsn - sml0    
   else
      smli = 0.
      hsn = snow*0.1/rnew       
   end if
      
   if (hsn == 0.) go to 18  ! SL: equals or lower equal?

!#### recalculate suz (volumetric content of ice in snow g/cm3)
   svz = 0.917*suz(j,jea)*hsn0 +snow*0.1 + sfreez
   if (smli.gt.svz) smli = svz
   suz(j,jea) = (svz-smli)/(hsn*0.917)
      
!#### recalculate sul (volumetric content of liquid water in snow g/cm3, svl cm)
   svl = sul(j,jea)*hsn0 + precipe*0.1 + smli
   if (sfreez > svl) sfreez = svl
   sul(j,jea) = (svl-sfreez)/hsn

!#### water outflow from snowpack
   if ( sul(j,jea) > ulmax ) then
      vsn = hsn * 10. * (sul(j,jea)-ulmax)
      if(vsn > precipe0) smle = vsn - precipe0
      sul(j,jea) = ulmax
   end if
   go to 19
      
   18 continue

   sul(j,jea) = 0.
   suz(j,jea) = 0.
   vsn        = rsn(j,jea)*hsn0*10. + precipe0
   smle       = rsn(j,jea)*hsn0*10.
      
   19 continue

!#### recalculate rsn
   rsn(j,jea) = sul(j,jea) + 0.917*suz(j,jea)
   if (hsn == 0.) rsn(j,jea) = 0.
            
!#### recalculate the total precipitation from snowpack
   precipe = vsn
   snoa(j,jea) = hsn * 10. * rsn(j,jea) 
   balanc      = snow + precipe0 + (snoa0 - snoa(j,jea))-vsn ! SL: balanc is never used hereafter
      
!#### conversion from snow to ice
!   if(ida.eq.273) then
   if (ida == gla_day_out) then
      ! SL: Maybe one should not convert all the snow into ice at day gla_day_out
      !     but every day a fraction only (snow2ice)!
      !if ( snoa(j,jea) > 2000. ) then
      gla(j,jea)  = gla(j,jea)  + snoa(j,jea) ! * snow2ice
      snoa(j,jea) = 0. !snoa(j,jea) * (1. - snow2ice)
      hsn         = 0. !snoa(j,jea) * .1 / rsn(j,jea)
      rsn(j,jea)  = 0.
   end if
 !end if

end subroutine snow_snom
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
subroutine snow_glacier(j,jea)
!**** PURPOSE: THIS SUBROUTINE COMPUTES DAILY ICE MELT  
!              WHEN AVERAGE AIR TEMP EXCEEDS 0 DEGREES C 
!**** CALLED IN:   SUBBASIN 
!     snow melt rate in SWAT = 4.57: sml = 4.57 * tmax
!     snow melt rate in HBV = 3.2
!     current version in SWIM: sml = 3.2 * tx(j
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!   
!      >>>>> COMMON PARAMETERS & VARIABLES
!      precipe    = precipitation in the hydrotop, mm
!      gmle       = ice melt, mm
!      gla(j,jea)    = glacier water equivalent, mm
!      tmax    = daily max temp, read in readcli, degree C
!      tmit     = daily average temp, read in readcli, degree C
!      >>>>>

!      >>>>> STATIC PARAMETERS 
!      gmr       = ice melt rate, coef for the degree-day method, mm/degree
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
   integer, intent(in) :: j,jea

   if ( tmit > tmelt(j) ) then
      gmle = gmrate(j) * (tmit-tmelt(j))
      if (gmle > gla(j,jea)) gmle = gla(j,jea)
      gla(j,jea) = gla(j,jea) - gmle
      precipe = precipe + gmle
   else
     gmle = 0.
   end if
      
!    gla_melt_bottom = 0.
! 
!    if ( gla(j,jea) > 1000. ) then
!       ! Calculate glacier melt at the bottom of the ice mass.
!       ! Melting water is added to surface runoff in curn.f (subroutine volq)
!       ! This process is independent from air temperature because melting depends on the pressure of the glacier on the bottom ice layer.
!       ! Possibly existing snow cover prevents melting from the glacier surface.
!       ! Here, I assume that the bigger the ice depth the bigger the meltrate.
!       ! Largest meltrate at 10m depth = gmrate.
!       ! Lowest meltrate at 1m = gmrate/4
!       if ( gla(j,jea) > 10000. ) then
!          gla_melt_bottom = gmrate
!       else
! !         gla_melt_bottom = normalise(gla(j,jea),10000.,1000.,gmrate,.25*gmrate)
!          gla_melt_bottom = normalise(gla(j,jea),10000.,1000.,gmrate,.5*gmrate)
!       end if
! 
!       if ( gla_melt_bottom > gla(j,jea) ) gla_melt_bottom = gla(j,jea)
!       if ( gla_melt_bottom > gmrate ) gla_melt_bottom = gmrate
!       gla(j,jea) = gla(j,jea) - gla_melt_bottom
!       ! add glacier melt from bottom layer to total glacier runoff, mm
!       gla_melt_total = gla_melt_total + gla_melt_bottom
!    end if
! 
!    ! calculate melt from surface layer if snow cover does not exist.
! !   if ( tmit > tmelt .AND. snoa(j,jea) <= 0.) then
! !   if ( tmit > tmelt .AND. snoa(j,jea) <= 100.) then
!    if ( tmit > tmelt ) then
!       gmr = normalise(snoa(j,jea),0.,100.,gmrate,0.) ! sim30
!       !gmr = normalise(snoa(j,jea),0.,3000.,gmrate,0.) ! sim31
!       gmr = max(0.,gmr)
!       gmle = gmr * (tmit-tmelt)
!       !gmle = gmrate * (tmit-tmelt)
!       if (gmle > gla (j,jea)) gmle = gla(j,jea)
!       gla(j,jea) = gla(j,jea) - gmle
!       !precipe    = precipe + gmle * 0.3 ! half of glacier melt to precipitation and the other half to surface runoff
!       ! add glacier melt to glacier runoff, mm
!       !gla_melt_total = gla_melt_total + gmle * 0.7
!       gla_melt_total = gla_melt_total + gmle
!    else
!       gmle = 0.
!    end if
! 
!    vsn = vsn + gla_melt_total
!    gla_melt_total = 0.
! 
! !    if (tmit.gt.tmelt) then
! !       gmle = gmrate * (tmit-tmelt)
! !       if (gmle.gt. gla (j,jea)) gmle = gla(j,jea)
! !       gla(j,jea) = gla(j,jea) - gmle
! !       precipe = precipe + gmle
! !    else
! !       gmle = 0.
! !    end if
!   
end subroutine snow_glacier
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
subroutine gla_gis(j,jea)
!**** PURPOSE: Write glacier water equivalent for hydrotops (for GRASS)
!**** CALLED IN:  SUBBASIN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      gla(j,je) = glacier equivalent at day 220 for Tinguiririca (Chile) in hydrotope, mm
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   integer, intent(in) :: j,jea
       
   write (351,123) ieapg,int(gla(j,jea)),gla(j,jea)

   ieapg = ieapg + 1

   123 format(i8, ' = ', i8,f12.2)
   return
end subroutine gla_gis
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
end module mod_snow
!------------------------------------------------------------------------------

