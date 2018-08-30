!     FILE init.f
!
!     SUBROUTINES IN THIS FILE       CALLED FROM
!     subroutine  init			main
!     subroutine  initsums		main
!     subroutine  initsub		subbasin	



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine init
!**** PURPOSE:    this subroutine initialises variables
!**** CALLED IN:  main
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
IMPLICIT NONE
   integer j
   real(8) randn

   htpmon = 0.

   sbp = 0.
   revapst = 0.     
   smq = 0.
   smsq = 0.
   sym = 0.
   syq = 0.
   sysq = 0.     
   syy = 0.
   sq = 0.
   ssq = 0.
   sy = 0.
   tmpNsur =0.005
   tmpNsub =0.005
   tmpNgrw =0.005
   tmpPsur =0.005

   susb = 0.

   sub = 0.
   smm = 0.
   smy = 0.
   sm  = 0.

   igro = 0
   idorm = 0
   s1 = 0.
   s2 = 0.
   tv = 0.
   canstor = 0.
   preinf = 0.
   hsumul = 0.
   hsumfc = 0.        
   
   dflow = 0.
   dfloy = 0.
   dflav = 0.

   qdinp = 0.
   qdout = 0.
   qssinp = 0.
   qssout = 0.

   accf = 0.
   
   if ( icrop == 1 ) then
      avyld = 0.
      aryld = 0.
   endif
   
   avylds = 0.
   avyldc = 0.
   arylds = 0.
   aryldc = 0.

   canmax = 0.
   
   if (iicep.gt.0) then
      do j=1,mc
         canmax(j)=canmx(j)
      end do
   end if

   vl = 100.
   vb = 0.
   v1 = randn(k2)
   v3 = randn(k3)
   v5 = randn(k4)
   v7 = randn(k5)

end subroutine init



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

subroutine initsums
!**** PURPOSE:    this subroutine initialises variables for GIS output 
!**** CALLED IN:  main 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
implicit NONE

   presum = 0.
   runsum = 0.
   evasum = 0.
   gwrsum = 0.
   wsav = 0.
   tsav = 0.     

end subroutine initsums



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine initsub
!**** PURPOSE:    this subroutine initialises subbasin variables
!**** CALLED IN:  subbasin 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
   use common_par
   implicit NONE

   sumcn  = 0.
   xqd = 0.
   xqi = 0.
   xssf = 0.
   qtl = 0.
   yd = 0.
   yon = 0.
   yph = 0.
   xsep = 0.
   xeo = 0.
   xet = 0.
   xsnoev = 0.
   xswind = 0.
   xyno3 = 0.
   xysp = 0.
   xssfn = 0.
   xpercn = 0.
   xnorg = 0.
   xnorgp = 0.
   xporg = 0.
   xporgp = 0.
   xpsed = 0.
   xpsedp = 0.
   xcklsp = 0.
   sml = 0.
   canev = 0.

end subroutine initsub


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

