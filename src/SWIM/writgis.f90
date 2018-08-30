!     FILE writgis.f
!
!     SUBROUTINES IN THIS FILE          CALLED FROM
!     subroutine hydro_gis(j,jek)	subbasin
!     subroutine crop_gis(j,jek,k)	subbasin



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine hydro_gis(j,jek)
!**** PURPOSE: Write annual sums of water flows for hydrotops (for GRASS) 
!**** CALLED IN:  SUBBASIN 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!   
!      >>>>> COMMON PARAMETERS
!      evasum(j,je) = annual sum of evapotranspiration in hydrotope, mm
!      ieapu        = counter
!      gwrsum(j,je) = annual sum of percolation to g.w. in hydrotope, mm
!      presum(j,je) = annual sum of precip in hydrotope, mm
!      runsum(j,je) = annual sum of runoff (sur + subsur) in hydrotope, mm 
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      nevap    = local parameter
!      nperc    = local parameter
!      nprec    = local parameter
!      nrunf    = local parameter
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
implicit NONE
   integer j,jek,nevap,nperc,nprec,nrunf

   nprec = int(presum(j,jek))
   nevap = int(evasum(j,jek))
   nrunf = int(runsum(j,jek))
   nperc = int(gwrsum(j,jek))

   write (36,123) ieapu,nprec,nprec
   write (37,123) ieapu,nevap,nevap
   write (38,123) ieapu,nrunf,nrunf
   write (39,123) ieapu,nperc,nperc

   ieapu=ieapu+1

  123 format(i8, ' = ', 2i8)

end subroutine hydro_gis

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


subroutine hydro_gis_monthly()
! called from mainpro at the end of the annual loop
use common_par
implicit NONE
   integer             :: i,hruID,subb,hru

   do i=1,12 ! loop over 12 months
      hruID = 0
      do subb=1,mb
         do hru=1,neap(subb)
            hruID = hruID + 1 ! mstruc(subb,hru,6)
            write(120,123) hruID,int(presummon(subb,hru,i)),int(presummon(subb,hru,i))
            write(121,123) hruID,int(runsummon(subb,hru,i)),int(runsummon(subb,hru,i))
            write(122,123) hruID,int(evasummon(subb,hru,i)),int(evasummon(subb,hru,i))
            write(123,123) hruID,int(petsummon(subb,hru,i)),int(petsummon(subb,hru,i))
            write(124,123) hruID,int(gwssummon(subb,hru,i)),int(gwssummon(subb,hru,i))
            write(125,124) hruID,swisummon(subb,hru,i),swisummon(subb,hru,i)
            write(126,123) hruID,npredays01(subb,hru,i),npredays01(subb,hru,i)
            write(127,123) hruID,npredays05(subb,hru,i),npredays05(subb,hru,i)
            write(128,123) hruID,npredays10(subb,hru,i),npredays10(subb,hru,i)
            write(129,123) hruID,npredays20(subb,hru,i),npredays20(subb,hru,i)
         end do
      end do
   end do

   presummon  = 0.
   runsummon  = 0.
   evasummon  = 0.
   petsummon  = 0.
   gwssummon  = 0.
   swisummon  = 0.
   npredays01 = 0
   npredays05 = 0
   npredays10 = 0
   npredays20 = 0


  123 format(i8, ' = ', 2i8)
  124 format(i8, ' = ', 2f8.1)

end subroutine hydro_gis_monthly

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine crop_gis(j,jek,n,k)
!**** PURPOSE: Write crop yield for GRASS output 
!**** CALLED IN:  SUBBASIN  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!   
!      >>>>> COMMON PARAMETERS & VARIABLES
!      ieap       = counter
!      ndgro      = number days of growth -  calc. in crop 
!      tsav(j,je) = temp  stress, sum for the growth period
!      wsav(j,je) = water stress, sum for the growth period
!      ylda(j,k)  = crop yield, kg/ha
!      >>>>>

!      >>>>> STATIC PARAMETERS 
!      mts     = mean temp  stress for the growth period, in %
!      mws     = mean water stress for the growth period, in %
!      myld    = ylda(j,k)/100 - yield in kg/dt, integer
!      xyld    = crop yield, dt/ha
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
implicit NONE
integer :: j,jek,n,k,mts,mws,myld
real(8)    :: xyld

   if (n.eq.5.and.k.lt.ms.and.ndgro.gt.0) then
      xyld = ylda(j,k)/100. +0.5
      myld = int(xyld)
      mws = wsav(j,jek)/ndgro*100
      mts = tsav(j,jek)/ndgro*100
   else
      myld = 0
      mws = 0
      mts = 0
   end if

   write(33,123) ieap,myld,myld
!      write(34,123) ieap,mws,mws
!      write(35,123) ieap,mts,mts

   ieap=ieap+1
   wsav(j,jek) = 0.             
   tsav(j,jek) = 0.

  123 format(i8, ' = ', 2i8,10i5)
end subroutine crop_gis

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine gis_mean(subb,hru,nyears)
!-------------------------------------------------------------------------------
! Author  : stefan.liersch@pik-potsdam.de
! Date    : 2010-01-18
! modified: 2010-01-18
!
! PURPOSE:  Write GIS output (total annual means over the simulation period)
! CALLED  : from subroutine subbasin
!-------------------------------------------------------------------------------
use common_par
integer, intent(in) :: subb,hru,nyears

   write(101,123) giscounter,int(evamean(subb,hru)/nyears),int(evamean(subb,hru)/nyears)
   write(102,123) giscounter,int(pcpmean(subb,hru)/nyears),int(pcpmean(subb,hru)/nyears)
   write(103,123) giscounter,int(petmean(subb,hru)/nyears),int(petmean(subb,hru)/nyears)
   write(105,123) giscounter,int(gwrmean(subb,hru)/nyears),int(gwrmean(subb,hru)/nyears)
   giscounter = giscounter + 1

  123 format(i8, ' = ', 2i8)
end subroutine gis_mean

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


