!     FILE readcrp.f90
!
!     SUBROUTINES IN THIS FILE          CALLED FROM
!     subroutine  readcrp		main



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine readcrp
!**** PURPOSE: THIS SUBROUTINE READS CROP PARAMETERS from crop.dat 
!**** CALLED IN:   MAIN  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      PARAMETERS & VARIABLES
!
!      >>>>> COMMON PARAMETERS & VARIABLES
!      ald1(ic)  = shape parameter for the LAI developement equation
!      ald2(ic)  = shape parameter for the LAI developement equation
!      almn(ic)  = minimum Leaf Area Index (for forest and natural vegetation)
!      be(ic)    = biomass-energy ratio
!      blai(ic)  = maximum Leaf Area Index for crop
!      bn1(ic)   = nitrogen uptake parameter #1: normal fraction of N 
!                  in crop biomass at emergence,kg N/kg biomass
!      bn2(ic)   = nitrogen uptake parameter #2: normal fraction of N 
!                  in crop biomass at 0.5 maturity,kg N/kg biomass
!      bn3(ic)   = nitrogen uptake parameter #3: normal fraction of N 
!                  in crop biomass at maturity,kg N/kg biomass
!      bnu1(ic)  = shape parameter to calc optimal N fraction in crop biomass
!      bnu2(ic)  = shape parameter to calc optimal N fraction in crop biomass
!      bp1(ic)   = phosphorus uptake parameter #1: normal fraction of P
!                  in crop biomass at emergence,kg P/kg biomass
!      bp2(ic)   = phosphorus uptake parameter #2: normal fraction of P
!                  in crop biomass at 0.5 maturity,kg P/kg biomass
!      bp3(ic)   = phosphorus uptake parameter #3: normal fraction of P
!                  P in crop biomass at maturity,kg P/kg biomass
!      bpu1(ic)  = shape parameter to calc optimal P fraction in crop biomass
!      bpu2(ic)  = shape parameter to calc optimal P fraction in crop biomass
!      cnyld(ic) = fraction of nitrogen in yield, kg N/kg yield
!      cpyld(ic) = fraction of phosphorus in yield, kg P/kg yield
!      cvm(ic)   = minimum value of C factor for water erosion
!      dlai(ic)  = fraction of growing season when leaf area declines
!      dlp1      = complex number: fraction of grow. season, max corresp. LAI
!      dlp2      = complex number: fraction of grow. season, max corresp. LAI
!      hi(ic)    = harvest index
!      hun(ic)   = potential heat units required for maturity of crop
!      icnum(ic) = crop number
!      ilcc(ic)  = land cover category:
!           (1)    annual crop (row crop / small grain)
!           (2)    annual winter crop (row crop / small grain)
!           (3)    perennial (grass, brush, urban, water)
!           (4)    woods
!           (5)    annual legumes (row crop)
!           (6)    annual winter legumes (row crop)
!           (7)    perennial legumes (grass)
!      pt2(ic)   = 2nd point on radiation use efficiency curve:
!                  complex number: The value to the left of (.) is a CO2 
!                  atm. concentration higher than the ambient (in units of 
!                  microliters CO2/liter air). The value to the right of (.) 
!                  is the corresponding biomass-energy ratio divided by 100
!      rdmx(ic)  = maximum root depth (m), then converted to mm
!      sla(ic)   = specific leaf area, m2/kg
!      tb(ic)    = base temperature for plan growth, degrees C
!      to(ic)    = optimal temperature for plant growth, degrees C
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      b1      = intermediate parameter
!      b2      = intermediate parameter
!      b3      = intermediate parameter
!      ic      = count parameter
!      titl    = text
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
implicit NONE
   integer ic
   real(8) b1,b2,b3
   real(8) x3,x4
   character*400 titl

   x3 = .5
   x4 = 1.

   write (6,*) 'CROP PARAMETERS:'
   write (6,101)

!**** READ 5 - crop parameter database (unit 5-crop.dat)
   read(5,*) titl
   do ic=1,mcrdb
      read(5,*) icnum(ic),cnam(ic),ilcc(ic),be(ic),hi(ic),to(ic),tb(ic), &
         blai(ic),dlai(ic),dlp1,dlp2,bn1(ic),bn2(ic),bn3(ic), &
         bp1(ic),bp2(ic),bp3(ic),cnyld(ic),cpyld(ic),rdmx(ic),cvm(ic), &
         almn(ic),sla(ic),pt2(ic),hun(ic)
      
      cvm(ic) = dlog(cvm(ic))
      rdmx(ic) = rdmx(ic) * 1000.

      !**** RECALCULATE:  dlp1, dlp2 need to be broken into two variables
      !**** Isolate number to left of decimal: b1, b2 - fraction of the grow. season
      b1 = .01 * Int(dlp1)         
      b2 = .01 * Int(dlp2)         
      !**** Isolate number to right of decimal, dlp1,2 is now max LAI corr. to b1,2                                     
      dlp1 = dlp1 - Int(dlp1)      
      dlp2 = dlp2 - Int(dlp2)      
                                       
      !#### determine shape parameters for the LAI developement equation
      call ascrv(dlp1, dlp2, b1, b2, ald1(ic), ald2(ic))

      !**** Calc nitrogen uptake parameters
      b1 = bn1(ic) - bn3(ic)                 
      b2 = 1. - (bn2(ic) - bn3(ic)) / b1
      b3 = 1. - .00001 / b1
      !#### CALL ASCRV
      call ascrv(b2, b3, x3, x4, bnu1(ic), bnu2(ic))

      !**** CORRECT bp3 & Calc phosphorus uptake parameters
      if (bp2(ic) - bp3(ic) < .0001) bp3(ic) = .75 * bp3(ic)
      b1 = bp1(ic) - bp3(ic)                
      b2 = 1. - (bp2(ic) - bp3(ic)) / b1
      b3 = 1. - .00001 / b1
      !#### CALL ASCRVg
      if ( b2 > 0. .AND. b2 < 1.) then
         call ascrv(b2, b3, x3, x4, bpu1(ic), bpu2(ic))
      else
         write(*,*) "Value of b2 in call ascrv() is causing an error.",b2
         write(*,*) "see section: CORRECT bp3 & Calc phosphorus uptake parameters in readcrp.f"
         write(1000,*) "Value of b2 in call ascrv() is causing an error.",b2
         write(1000,*) "ic:",ic,"b1:",b1,"bp2:",bp2,"bp3:",bp3
         STOP
      end if

      !**** WRITE selected parameters on screen
      if ( ic.eq.20 ) write(6,100) icnum(ic),cnam(ic), be(icnum(ic)), to(icnum(ic)), &
         tb(icnum(ic)), blai(icnum(ic)),dlai(icnum(ic)) 
      if (ic.eq.22) write(6,100) icnum(ic),cnam(ic), be(icnum(ic)), to(icnum(ic)), &
         tb(icnum(ic)),blai(icnum(ic)),dlai(icnum(ic)) 
      if (ic.eq.25) write(6,100) icnum(ic),cnam(ic), be(icnum(ic)), to(icnum(ic)), &
         tb(icnum(ic)),blai(icnum(ic)),dlai(icnum(ic)) 
      if (ic.eq.36) write(6,100) icnum(ic),cnam(ic), be(icnum(ic)), to(icnum(ic)), &
         tb(icnum(ic)),blai(icnum(ic)),dlai(icnum(ic)) 
      if (ic.eq.42) write(6,100) icnum(ic),cnam(ic), be(icnum(ic)), to(icnum(ic)), &
         tb(icnum(ic)),blai(icnum(ic)),dlai(icnum(ic)) 
      if (ic.eq.45) write(6,100) icnum(ic),cnam(ic), be(icnum(ic)), to(icnum(ic)), &
         tb(icnum(ic)), blai(icnum(ic)),dlai(icnum(ic)) 

   end do ! ic=1,mcrdb
   close(5)

   write(6,*) '===> Crop parameters in crop.dat - READ!'
   write (6,*) ' '

100 format(i5,2x,a4,5f6.2)           
101 format(' cnum  CROP    be    to    tb  blai  dlai')      

end subroutine readcrp
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
