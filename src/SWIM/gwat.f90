! FILE gwat.f
!
! SUBROUTINES IN THIS FILE          CALLED FROM
! subroutine  gwmod(j)              subbasin



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


subroutine gwmod(j,percolation,ETact,ETpot)
!**** PURPOSE:  TO ESTIMATE GROUNDWATER CONTRIBUTION TO STREAMFLOW
!**** CALLED IN:   SUBBASIN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PARAMETERS & VARIABLES
!
!  >>>>> COMMON PARAMETERS & VARIABLES
!  abf(j)     = alpha factor for g-w = reaction factor * delta(T)
!  abf1(j)    = exp fun of alpha factor: abf1(j) = exp(-abf(j))
!  delay(j)   = groundwater delay, days
!  gwchrg     = groundwater recharge, mm
!  gwht(j)    = groundwater height, mm
!  gwq(j)     = groundwaterw contribution to stream, in mm
!  gwseep     = groundwater seepage, mm
!  rchrg(j)   = groundwater recharge to the aquifer, mm
!  rchrgc(j)  = fraction of root zone perc that goes into deep g-w
!  revap      = revaporation from groundwater, mm
!  revapc(j)  = fraction of root zone perc that goes to revap
!  revapst(j) = revap storage, mm
!  syld(j)    = specific yield for g-w
!  xet        = evapotranspiration (actual), mm
!  xsep       = subbasin percolation, mm
!  >>>>>

!  >>>>> STATIC PARAMETERS
!  rchrg1    = groundwater recharge to the aquifer
!  >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
implicit NONE
   integer, intent(in) :: j
   real(8), intent(in)    :: percolation, ETact, ETpot ! [mm]
   real(8)    :: rchrg1 ! = rchrg of previous time step

   rchrg1 = rchrg(j)

!  Calc recharge
   rchrg(j) = percolation !xsep

!  Calc g-w seepage
   gwseep = rchrg(j) * rchrgc(j)

!! compute revap to soil profile/plant roots
   revap = revapc(j) * ETpot ! according to SWAT revap depends on ETpot not ETact
   if ( revap > (ETpot-ETact) ) revap = ETpot - ETact
   revap = max(0.,revap)

!! compute gw recharge level
   rchrg(j) = (1.-delay(j)) * rchrg(j) + delay(j) * rchrg1
   if (rchrg(j) < 1.e-6) rchrg(j) = 0.

   gwchrg = rchrg(j)

!  Calc g-w height
   gwht(j) = gwht(j) * abf1(j) + rchrg(j) * (1.-abf1(j)) &
             / (800.*syld(j) + 1.e-6*abf(j) + 1.e-6)
   gwht(j) = max(1.e-6, gwht(j))

!  Calc g-w contribution to streamflow
   gwq(j) = gwq(j) * abf1(j) + (rchrg(j)) * (1.-abf1(j))

!  Calc revap storage
   revapst(j) = revapst(j) + rchrg(j)

!! remove ground water flow from shallow aquifer storage
   revapst(j) = revapst(j) - gwq(j)
   if ( revapst(j) < 0. ) then
      gwq(j) = gwq(j) + revapst(j)
      revapst(j) = 0.
   end if

   ! remove seepage to deep aquifer from revap storage (shallow aquifer)
   revapst(j) = revapst(j) - gwseep
   if ( revapst(j) < 0. ) then
      gwseep = gwseep + revapst(j)
      revapst(j) = 0.
   end if

!! remove revap to soil profile (capillary rise) from shallow aquifer storage
   if ( revapst(j) < revapmn(j) ) then
      revap = 0.
   else
      revapst(j) = revapst(j) - revap
      if (revapst(j) < revapmn(j) ) then
         revap = revap + revapst(j) - revapmn(j)
         revapst(j) = revapmn(j)
      end if
   end if
! add revap to actual evapotranspiration otherwise it does not occur in water balance output
   xet = max(0.,ETact+revap)

end subroutine gwmod



!subroutine gwmod(j,percolation,ETact)
!**** PURPOSE:  TO ESTIMATE GROUNDWATER CONTRIBUTION TO STREAMFLOW
!**** CALLED IN:   SUBBASIN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PARAMETERS & VARIABLES
!
!  >>>>> COMMON PARAMETERS & VARIABLES
!  abf(j)     = alpha factor for g-w = reaction factor * delta(T)
!  abf1(j)    = exp fun of alpha factor: abf1(j) = exp(-abf(j))
!  delay(j)   = groundwater delay, days
!  gwchrg     = groundwater recharge, mm
!  gwht(j)    = groundwater height, mm
!  gwq(j)     = groundwaterw contribution to stream, in mm
!  gwseep     = groundwater seepage, mm
!  rchrg(j)   = groundwater recharge to the aquifer, mm
!  rchrgc(j)  = fraction of root zone perc that goes into deep g-w
!  revap      = revaporation from groundwater, mm
!  revapc(j)  = fraction of root zone perc that goes to revap
!  revapst(j) = revap storage, mm
!  syld(j)    = specific yield for g-w
!  xet        = evapotranspiration (actual), mm
!  xsep       = subbasin percolation, mm
!  >>>>>

!  >>>>> STATIC PARAMETERS
!  rchrg1    = groundwater recharge to the aquifer
!  >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
! use common_par
! implicit NONE
!    integer, intent(in) :: j
!    real(8), intent(in)    :: percolation, ETact ! [mm]
!    real(8)    :: rchrg1 ! = rchrg of previous time step
! 
!    rchrg1 = rchrg(j)
! 
! !  Calc recharge
!    rchrg(j) = percolation !xsep
! 
! !  Calc g-w seepage
!    gwseep = rchrg(j) * rchrgc(j)
! 
! !  Calc revap
!    revap = revapc(j) * ETact !xet
! 
!    rchrg(j) = (1.-delay(j)) * rchrg(j) + delay(j) * rchrg1
! 
!    gwchrg = rchrg(j)
! 
! !  Calc g-w height
!    gwht(j) = gwht(j) * abf1(j) + rchrg(j) * (1.-abf1(j)) &
!              / (800.*syld(j) + 1.e-6*abf(j) + 1.e-6)
! 
! !  Calc g-w contribution to streamflow
!    gwq(j) = gwq(j) * abf1(j) + rchrg(j) * (1.-abf1(j))
! 
! !  Calc revap storage
!    revapst(j) = revapst(j) + rchrg(j) 
!    revapst(j) = revapst(j) - gwq(j) 
! 
!    if(revapst(j).lt.0.) then
!      gwq(j) = gwq(j) + revapst(j)
!      revapst(j) = 0.
!    end if
! 
!    revapst(j) = revapst(j) - gwseep
! 
!    if(revapst(j).lt.0.) then
!      gwseep = gwseep + revapst(j)
!      revapst(j) = 0.
!    end if
! 
!    revapst(j) = revapst(j) - revap
! 
!    if(revapst(j).lt.0.) then
!      revap = revap + revapst(j)
!      revapst(j) = 0.
!    end if
! 
! end subroutine gwmod

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

