!     FILE route.f
!
!     SUBROUTINES IN THIS FILE          		CALLED FROM
!     subroutine  route(icode,ihout,inum1,inum2)	main
!     subroutine  add(icode,ihout,inum1,inum2)		main
!     subroutine  transfer(icode,ihout,inum1,inum2)	(main, not active)



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine route(icode,ihout,inum1,inum2)
!**** PURPOSE: THIS SUBROUTINE CONTROLS THE CHANNEL ROUTING 
!     STEPS: 1) xxqd = varoute(2,) 
!            2) xxqd recalc in rthyd 
!            3) varoute(2,)=xxqd
!**** CALLED IN:  MAIN 
!     ATTN! Look at the final line in the .fig file to define where
!           the final basin output is written: in route() or in add()
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!
!      >>>>> PARAMETERS IN TITLE
!      icode   = code to switch between routing subroutines (here: 2)
!      ihout   = hydrological storage location
!      inum1   = reach number
!      inum2   = inflow hydrograph(inum2 hydrograph is routed through inum1)
!      >>>>>

!      >>>>> COMMON PARAMETERS & VARIABLES
!      accf(1)     = accumulated runoff, m3/sec.
!      accf(2)     = accumulated sediment, t
!      accf(3)     = accumulated NO3-N, kg
!      da          = drainage area, km2
!      dart(ih)    = drainage area for subbasin, km2
!      diver       = diversion (not active)
!      evp         = evaporation from river surface, m3
!      flu(j)      = fraction of subbasin area in the basin
!      ida         = current day
!      iy          = current year as counter (1,...,nbyr)
!      iyr         = current year
!      revapst(j)  = transmission losses in the reach, mm
!      rflow       = return flow (not acive)
!      rl          = seepage (not acive)
!      runs(ida)   = runoff simulated for the basin, m3/sec.
!      sdti        = inflow + storage, calc in rthyd, m3/sec.
!      srch(18,j)  = monthly reach outputs:      
!      srch(1,j)   = water inflow, m3/s
!          (2,j)   = water outflow, m3/s 
!          (3,j)   = sediment in, t
!          (4,j)   = sediment out, t
!          (5,j)   = sediment conc, mg/l 
!          (6,j)   = org N in, kg
!          (7,j)   = org N out, kg
!          (8,j)   = sed P in, kg
!          (9,j)   = sed P out, kg
!          (10,j)  = evap, m3/s
!          (11,j)  = transmission losses, m3/s
!          (12,j)  = seepage, m3/s
!          (13,j)  = diversions, m3/s
!          (14,j)  = return flow, m3/s
!          (15,j)  = NO3-N in, kg
!          (16,j)  = NO3-N out, kg
!          (17,j)  = sol P in, kg
!          (18,j)  = sol P out, kg
!      tlc         = transmission losses, m3
!      varoute(1:8,ih) = variables for routing:
!      Name             Units          Definition
!      varoute(2,ih)    |(m^3)         |surface flow  
!      varoute(3,ih)    |(tons)        |sediment
!      varoute(4,ih)    |(kg)          |organic N  
!      varoute(5,ih)    |(kg)          |organic P  
!      varoute(6,ih)    |(kg)          |nitrate N  
!      varoute(7,ih)    |(kg)          |soluble P
!      varoute(8,ih)    |(m^3)         |subsurface + g-w flow
!      xxnit       = NO3-N amount in the reach, kg
!      xxqd        = water amount in the reach, m3
!      xxssf       = subsurface flow, m3
!      xysp        = soluble P, kg
!      yd          = daily soil loss from subbasin caused by water erosion, t
!      ydi         = sediment, t
!      yon         = org N loss with erosion, kg
!      yph         = P org. loss with erosion, kg
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      cnit      = local par
!      ivar2neg  = local variable: negative surface runoff
!      ivar8neg  = local variable: negative subsurface runoff   
!      j         = local par, reach no.
!      sedcon    = local variable: sediment concentration, t/m3
!      xx        = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
implicit NONE
integer, intent(in) :: icode,ihout,inum1,inum2
integer             :: ivar2neg,ivar8neg,j
real(8)                :: cnit,sedcon,xx
real(8)                :: tl_mm  ! transmission losses, mm
real(8)                :: evp_mm ! evaporative losses from river surface, mm

!**** INITIALIZATION
   j = inum1
   
   tl_mm  = 0.
   evp_mm = 0.
   tlc    = 0.
   evp    = 0.

   xxqd = varoute(2,inum2)  
   yd   = varoute(3,inum2)  
   yon = varoute(4,inum2)  
   yph = varoute(5,inum2)  
   xxnit = varoute(6,inum2)  
   xysp = varoute(7,inum2)  
   xxssf = varoute(8,inum2)  

   if (ida.eq.1.and.j.eq.1) ivar2neg = 0
   if (ida.eq.1.and.j.eq.1) ivar8neg = 0       

   if(ida.eq.1.and.j.eq.1) then
      accf(1) = 0.
      accf(2) = 0.
      accf(3) = 0.
   end if               

!#### CALL RTHYD to route hydrograph to basin outlet: recalc xxqd, xxssfn 
   call rthyd(j,ihout,inum1,inum2)

!**** COMPUTE transmission losses in the reach
   if ( tlrch > 0. .OR. evrch > 0. ) then
      call tran_river (j)
      tl_mm  = tlc / sbar(j) * 1000. ! convert m3 to mm
      evp_mm = evp / sbar(j) * 1000. ! convert m3 to mm

      select case (tlgw)
         case (0) ! add transmission losses to shallow ground water storage
            revapst(j) = revapst(j) + tl_mm          ! add to revap storage
            susb(18,j) = susb(18,j) + tl_mm          ! add to gw recharge
            sub(18)    = sub(18)    + tl_mm * flu(j)

         case (1) ! add transmission losses to deep ground water storage (lost from the system)
            susb(17,j) = susb(17,j) + tl_mm          ! add to deep gw (gwseep)
            sub(17)    = sub(17)    + tl_mm * flu(j)
            ! add transmission losses to transmission losses variables
            susb(10,inum1) = susb(10,inum1) + tl_mm
            sub(10) = sub(10) + tl_mm * flu(j)

         case (2) ! add transmission losses to both shallow and deep ground water storage to equal parts
            revapst(j) = revapst(j) + tl_mm * .5     ! add to revap storage
            susb(18,j) = susb(18,j) + tl_mm * .5     ! add to gw recharge
            sub(18)    = sub(18)    + tl_mm * .5 * flu(j)
            susb(17,j) = susb(17,j) + tl_mm          ! add to deep gw (gwseep)
            sub(17)    = sub(17)    + tl_mm * flu(j)
            ! add transmission losses to transmission losses variables
            susb(10,inum1) = susb(10,inum1) + tl_mm * .5
            sub(10) = sub(10) + tl_mm * .5 * flu(j)
      end select

      ! add evaporative losses from river surface to ETact
      susb(13,j) = susb(13,j) + evp_mm
      sub(13)    = sub(13)    + evp_mm * flu(j)
   end if ! ( tlrch > 0. .OR. evrch > 0. )

!#### CALL RTSED, ENRRT, RTORGN, RTPSED to route sediments, org N & org P 
!     rtsed - recalc yd
!     enrrt - calc er - enrichment coefficient 
!     rtorgn - recalc yon
!     rtpsed - recalc yph
   if (sdti.gt.0.) then
      ydi = yd
      call rtsed(j,ihout,inum1,inum2)
      call enrrt(j,ihout,inum1,inum2)
      if (varoute(3,inum2).gt.0.) then
         xx = yd / varoute(3,inum2)
      else
         xx = 0.
      end if
      call rtorgn
      yon = yon * xx
      if (yon.gt.varoute(4,inum2)) yon = varoute(4,inum2)        
      call rtpsed        
      yph = yph * xx
      if (yph.gt.varoute(5,inum2)) yph = varoute(5,inum2)
   end if
      
!**** Calculate monthly reach outputs
!     output 5 is changed from deposition in tons to concentration in ppm
   srch(1,inum1) = srch(1,inum1) + varoute(2,inum2)/ 86400.
   srch(2,inum1) = srch(2,inum1) + xxqd/ 86400.
   srch(3,inum1) = srch(3,inum1) + varoute(3,inum2)
   srch(4,inum1) = srch(4,inum1) + yd
   if(xxqd.gt.0.1) then
      sedcon = yd / xxqd * 1.e6
   else
      sedcon = 0.
   end if
   if(sedcon.gt.200000.) sedcon = 200000.
      srch(5,inum1) = srch(5,inum1) + sedcon
      srch(6,inum1) = srch(6,inum1) + varoute(4,inum2)
      srch(7,inum1) = srch(7,inum1) + yon * dart(ihout) * 100.
      srch(8,inum1) = srch(8,inum1) + varoute(5,inum2)
      srch(9,inum1) = srch(9,inum1) + yph * dart(ihout) * 100.
      srch(10,inum1) = srch(10,inum1) + evp / 86400.
      srch(11,inum1) = srch(11,inum1) + tlc / 86400.
      srch(12,inum1) = srch(12,inum1) + rl / 86400.
      srch(13,inum1) = srch(13,inum1) + diver / 86400.
      srch(14,inum1) = srch(14,inum1) + rflow / 86400.
      srch(15,inum1) = srch(15,inum1) + varoute(6,inum2)
      srch(16,inum1) = srch(16,inum1) + xxnit
      srch(17,inum1) = srch(17,inum1) + varoute(7,inum2) * 100.
      srch(18,inum1) = srch(18,inum1) + xysp * dart(ihout) * 100.
! sl begin
      srch(19,inum1) = srch(19,inum1) + (varoute(2,inum2)+varoute(8,inum2))/86400.
! sl end

!**** RECALCULATE varoute in Hydrological Storage Location ihout  
      varoute(2,ihout) = xxqd
      varoute(3,ihout) = yd
      varoute(4,ihout) = yon
      varoute(5,ihout) = yph
      varoute(6,ihout) = xxnit
      varoute(7,ihout) = xysp
      varoute(8,ihout) = xxssf

!**** WRITE daily to reach file unit=74: rch.prn
      if(j.eq.1) then
        write (74,300)j,ida,ihout,xxqd/86400.,varoute(2,ihout)     
      endif

!**** WRITE DAILY OUTPUT in outlet (if j=1) to river output file unit=75
!     WRITE is possible in variants - needed one should be opened 
      if(j.eq.1) then
         runs(ida)=(varoute(2,ihout)+varoute(8,ihout))/86400.
         accf(1) = accf(1) + runs(ida)
         accf(2) = accf(2) + varoute(3,ihout)
         accf(3) = accf(3) + varoute(6,ihout)

!**** CALC N CONCENTRATION
         cnit = varoute(6,ihout) / runs(ida) / 86.4

!**** WRITE WATER DISCHARGE
         if (iy.eq.1.and.ida.eq.1) write(75,601)  
         write (75,301) iyr,ida, runs(ida)  

!****  WRITE WATER & SEDIMENTS
!      if (iy.eq.1.and.ida.eq.1) write(75,602) 
!      write (75,301) iyr,ida, runs(ida), varoute(3,ihout)

!****  WRITE WATER, SEDIMENTS & N LOAD
!      if (iy.eq.1.and.ida.eq.1) write(75,603)  
!      write (75,301) iyr,ida, runs(ida), varoute(3,ihout),      
!     *              varoute(6,ihout)       

!****  WRITE DAY, WATER DISCHARGE, m3/s; 
!      SEDIMENTS, kg; ACC SED, t; N LOAD, kg; ACC N LOAD, t      
!      if (iy.eq.1.and.ida.eq.1) write(75,604)  
!      write (75,304) iyr,ida, runs(ida),  
!     *     varoute(3,ihout), accf(2)/1000.,     
!     *     varoute(6,ihout),accf(3)/1000.       

!      WRITE WATER, N LOAD & N concentration
!      if (iy.eq.1.and.ida.eq.1) write(76,605) 
!      write (76,301) iyr,ida, runs(ida), varoute(6,ihout), cnit      

         if (varoute(2,ihout).lt.0.) ivar2neg = ivar2neg +1      
         if (varoute(8,ihout).lt.0.) ivar8neg = ivar8neg +1      
      end if
             
  300 format('--->>>REACH ',3i4,2f15.3)
  301 format(2i5,3f15.3)
  304 format(2i5,6f15.3)
  601 format (' YEAR  DAY           QSIM')
  602 format (' YEAR  DAY',11X,'QSIM',12X,'SED')
  603 format (' YEAR  DAY',11X,'QSIM',9X,'SED,kg',8X,'NMIN,kg')
  604 format (' YEAR  DAY',11X,'QSIM',9X,'SED,kg',7X,'accSED,t',8X,'NMIN,kg',6X,'accNMIN,t')
  605 format (' YEAR  DAY',11X,'QSIM',8X,'NMIN,kg',7X,'Ncon,mg/l')
        return 
end subroutine route



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


       
subroutine add(icode,ihout,inum1,inum2)
!**** PURPOSE: THIS SUBROUTINE ADDS OUTPUTS FOR ROUTING 
!**** CALLED IN:   MAIN 
!     ATTN! Look at the final line in the .fig file to define 
!           where the output has to be written: in route() or in add()
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!
!      >>>>> PARAMETERS IN TITLE
!tit   icode   = code to switch between routing subroutines (here: 2)
!tit   ihout   = hydrological storage location
!tit   inum1   = reach number
!tit   inum2   = inflow hydrograph(inum2 hydrograph is routed through inum1)
!      >>>>> 

!      >>>>> COMMON PARAMETERS & VARIABLES
!      accf(4)         = accumulated org. N, m3/sec.
!      accf(5)         = accumulated org. P, t
!      accf(6)         = accumulated NO3-N, kg
!      ida             = current day
!      iy              = current year as counter (1,...,nbyr)
!      iyr             = current year
!      runs(ida)       = runoff simulated for the basin, m3/sec.  
!      varoute(1:8,ih) = vector for routing:
!      Name             Units          Definition
!      varoute(2,ih)    |(m^3)         |surface flow  
!      varoute(3,ih)    |(tons)        |sediment
!      varoute(4,ih)    |(kg)          |organic N  
!      varoute(5,ih)    |(kg)          |organic P  
!      varoute(6,ih)    |(kg)          |nitrate N  
!      varoute(7,ih)    |(kg)          |soluble P
!      varoute(8,ih)    |(m^3)         |subsurface + g-w flow
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      cnit  = NO3-N concentration, mg/l
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
use mod_reservoir ! ### RESERVOIR MODULE
implicit NONE
   integer, intent(in) :: icode,ihout,inum1,inum2
   integer             :: i,k
   real(8)             :: cnit
   logical             :: bAdd ! ### RESERVOIR MODULE

   if(ida.eq.1.and.inum2.eq.1) then
      accf(4) = 0.
      accf(5) = 0.
      accf(6) = 0.
   end if
      
!#######################
! ### RESERVOIR MODULE
!#######################
   bAdd = .true.

   if ( bRsvModule ) then
      if ( inum2 <= mb ) then
         if ( rsvSubbasin(inum2) /= 0 .AND. RSV_is_operational(iyr,ida,inum2) ) then
            varoute(2,ihout) = varoute(2,inum1)
            varoute(3,ihout) = varoute(3,inum1)
            varoute(4,ihout) = varoute(4,inum1)
            varoute(5,ihout) = varoute(5,inum1)
            varoute(6,ihout) = varoute(6,inum1)
            varoute(7,ihout) = varoute(7,inum1)
            varoute(8,ihout) = varoute(8,inum1)
            bAdd = .false. !!! DO NOT CALL add_varoute !!!
         else
         
         end if ! ( rsvSubbasin(inum2) /= 0 )
      end if ! if inum2 > mb

   else ! if ( .NOT.bRsvModule )
         
   end if ! ( bRsvModule )
!#######################! END IF ( bRsvModule )

   if ( bAdd ) call add_varoute !**** ADD FLOWS: 2 - surface flow, 8 - subsurface flow   

!*** Overwrite simulated routed discharge with observed, eg. for calibration
   if (bRunoffdat .and. nqobs>1) then
      do i=2,nqobs
         if ( inum2.eq.obssb(i) .and. runo(ida,i)>=0. ) then
            varoute(2,ihout) = runo(ida,i)*86400.
            varoute(8,ihout) = 0.
         endif
      enddo
   endif

!**** WRITE DAILY OUTPUT in outlet (if inum2=1) to river output file unit=76
!     WRITE is possible in variants - needed one should be opened 
      if ( inum2 == 1 ) then  ! if inum2 is outlet (usually the last add command in the .fig file)

         if (bRsvModule) then ! if reservoir module is active
            if ( rsvSubbasin(inum2) /= 0 ) then ! if subbasin inum2 is a reservoir
               runs(ida)=(varoute(2,inum1)+varoute(8,inum1))/86400.
            else
               runs(ida)=(varoute(2,ihout)+varoute(8,ihout))/86400.
            end if
         else
            runs(ida)=(varoute(2,ihout)+varoute(8,ihout))/86400.
         end if
         !runs(ida)=(varoute(2,ihout)+varoute(8,ihout))/86400.
         accf(4) = accf(4) + runs(ida) 
         accf(5) = accf(5) + varoute(3,ihout) 
         accf(6) = accf(6) + varoute(6,ihout) 

!**** CALC N CONCENTRATION
!        varoute(6,) - kg, runs - m3/sec, cnit - mg/l
         cnit = varoute(6,ihout) / runs(ida) / 86.4

         ! Q_gauges_sel_ output
         do i = 1, ngaugesout
            if (bRsvModule) then
               if ( rsvSubbasin(gaugesout(i)) /= 0 ) then ! if subbasin is a reservoir
                  gaugesout_runoff(i)=(varoute(2,inum1s(gaugesouthyd(i)))+varoute(8,inum1s(gaugesouthyd(i))))/86400.
               else
                  gaugesout_runoff(i)=(varoute(2,gaugesouthyd(i))+varoute(8,gaugesouthyd(i)))/86400.
               end if
            else
               gaugesout_runoff(i)=(varoute(2,gaugesouthyd(i))+varoute(8,gaugesouthyd(i)))/86400.
            end if
         end do

         ! Q_gauges_all_ output
         if ( bAllSubbasinsOut ) then
            do i = 1, mb
               if (bRsvModule) then
                  if ( rsvSubbasin(i) /= 0 ) then ! if subbasin is a reservoir
                     runsub_m3s(i)=(varoute(2,inum1s(subouthyd(i)))+varoute(8,inum1s(subouthyd(i))))/86400.
                  else
                     runsub_m3s(i)=(varoute(2,subouthyd(i))+varoute(8,subouthyd(i)))/86400.
                  end if
               else
                  runsub_m3s(i)=(varoute(2,subouthyd(i))+varoute(8,subouthyd(i)))/86400.
               end if
            end do

            !**** Write out to Q_gauges_all... file when at outlet (inum2=1)
            write(761,'(2i5,999999f15.3)') iyr, ida, (runsub_m3s(k),k=1,mb)
         end if
         !end if ! if (inum2 == 1)

!**** WRITE WATER DISCHARGE
      if (iy.eq.1.and.ida.eq.1) write(76,601) 
      write (76,301) iyr,ida, runs(ida) 
	additionalGwUptake(1:mb)=0

!***  write file: Q_gauges.csv
!      if ( ihout==mhyd-1) then
         write(100,102,advance='NO') iyr,",",ida,",",runo(ida,1),","
         do i = 1, ngaugesout-1
            write(100,101,advance='NO') gaugesout_runoff(i),","
         end do
         write(100,fmt='(f15.3)',advance='YES') gaugesout_runoff(ngaugesout)
!      end if


      end if ! (inum2.eq.1)


  101 format(f15.3,a)
  102 format(i4,a,i3,a,f15.3,a,f15.3,a)
  103 format(f15.3)

  301 format(2i5,3f15.3)
  304 format(2i5,6f20.3)
  601 format (' YEAR  DAY           QSIM')
  602 format (' YEAR  DAY',11X,'QSIM',12X,'SED')
  603 format (' YEAR  DAY',11X,'QSIM',9X,'SED,kg',8X,'NMIN,kg')
  604 format (' YEAR  DAY',11X,'QSIM',9X,'SED,kg',7X,'accSED,t',8X,'NMIN,kg',6X,'accNMIN,t')
  605 format (' YEAR  DAY',11X,'QSIM',8X,'NMIN,kg',7X,'Ncon,mg/l')        


CONTAINS

!--------------------------------------------------------
subroutine add_varoute
   varoute(2,ihout) = varoute(2,inum1) + varoute(2,inum2)
   varoute(3,ihout) = varoute(3,inum1) + varoute(3,inum2)
   varoute(4,ihout) = varoute(4,inum1) + varoute(4,inum2)
   varoute(5,ihout) = varoute(5,inum1) + varoute(5,inum2)
   varoute(6,ihout) = varoute(6,inum1) + varoute(6,inum2)
   varoute(7,ihout) = varoute(7,inum1) + varoute(7,inum2)
   varoute(8,ihout) = varoute(8,inum1) + varoute(8,inum2)
end subroutine add_varoute
!--------------------------------------------------------

end subroutine add


       
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


     
      subroutine transfer(icode,ihout,inum1,inum2)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!**** PURPOSE: THIS SUBROUTINE CONTROLS THE CHANNEL ROUTING, NOT USED! 
!**** CALLED IN:   MAIN, "sleeping subroutine" 
      integer icode,ihout,inum1,inum2
      return
      end
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




subroutine tran_river(j)
! *** PURPOSE: this subroutine calculate the transmssion and evaporation losses in river reaches (by Huang, 2013, based on SWAT code)
! *** CALLED IN:   ROUTE
!     METHOD:  SWAT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!   
!      >>>>> COMMON PARAMETERS & VARIABLES
!      chl(2,j)         = channel length, km
!      evrch       |none          |Reach evaporation adjustment factor.
!                               |Evaporation from the reach is multiplied by
!                               |EVRCH. This variable was created to limit the
!                               |evaporation predicted in arid regions.
!      phi(5,j)         = "normal" flow
!      sdti             = water inflow + storage in the reach, m3/sec.
!      sdtsav(ir)       = water storage in the reach, m3
!      xxqd             = surface flow - daily input in reaches, m3
!      xxssf            = subsurface + g-w flow - daily input in reaches, m3
!      tlc         = transmission losses, m3
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      nreach  = reach No.
!      yy      = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

! *** Include common parameters
use common_par
implicit NONE
   integer :: j,nreach
   real(8)    :: d,b,volrt,rchdep,p,rh,a,rcharea,rttime1
   real(8)    :: adddep,addp,addarea,tlc1,tlc2,evp1,evp2,topw
   real(8)    :: qman,qdandssf,qd_ssf
   real(8)    :: x,w,Vconvert,k_convert,intercept,xx,decay,slope
   real(8)    :: slope2,intercept2,chside

   nreach = j

   chside = 2.
   d      = chd(j)
   b      = chw(2,j) - 2. * d * chside
   volrt  = sdti
   sdti   = 0.
   rchdep = 0.
   p      = 0.
   rh     = 0.
   qd_ssf = 0.
   tlc    = 0.
   evp    = 0.

!**** CALC bottom width b & channel side chside
   if (b <= 0.) then
      b = .5*chw(2,j)
      chside = (chw(2,j)-b)/(2.*d)
   end if

!**** calculate the current water level 
   if ( volrt >= phi(5,j) ) then
      rcharea = b * d + chside * d * d
      rchdep = d
      p = b + 2. * d * sqrt(chside*chside+1)
      rh = rcharea / p
      sdti = phi(5,j)
      !! this is the time to empty the volume of water at the bankfull Q
      rttime1 = volrt*86400./(3600.*sdti)
      if (rttime1 > 24.) then
         !! perform flood plain simulation
         !! increase Q in flood plain until all the volume can be emptied in one day
         adddep = 0.
         do while ( rttime1 > 24.)
            !! 1 cm interval 
            adddep = adddep + 0.01
            addarea = rcharea + ((chw(2,j)*5.) + chside*adddep)*adddep
            addp = p + chw(2,j)*4. + 2.*adddep*sqrt(17.)
            rh = addarea / addp
            sdti = qman(addarea,rh,chnn(j),chss(j))
            rttime1 = volrt*86400./(3600.*sdti)
         end do
         rcharea = addarea
         rchdep = rchdep + adddep
         p = addp
      end if
          
   else
      do while (sdti .lt. volrt)
         !! find the cross sectional area and depth for volrt
         !! 1 cm interval depth
         rchdep = rchdep + 0.01
         rcharea = (b+chside*rchdep) *rchdep
         p = b + 2. *rchdep *sqrt(chside*chside+1)
         rh = rcharea / p
         sdti = qman(rcharea,rh,chnn(j),chss(j))
      end do
   end if ! ( volrt >= phi(5,j) )
        
!**** sdti 
   sdti = volrt
         
   if (sdti.gt.0.) then

!**** calculate the transmission losses in rivers
      qd_ssf = xxqd/(xxqd+xxssf)

      if ( tlrch > 0. ) then
         qdandssf = xxqd+xxssf
         if (qdandssf.gt.0.) then   
            if (volrt.ge.phi(5,j)) then ! SWAT method only for flood conditions
               tlc = tlrch*24.*chk(2,j)*chl(2,j)*p
            else     ! WASA method for normal conditions, water is within the channel
               x = chl(2,j)*1000*0.00062 !river lenth, conversion kilo meter to miles 1000*0.00062
               w = 10.**(log10(sdti)*0.494+1.031)*3.281  ! river width, convertion meter to feet, 3.281
               Vconvert = sdti*0.00081*86400  ! conversion m3/s to acre-feet
               k_convert = tlrch*0.0394*chk(2,j)  ! mm/hr to inch/hr

               intercept = -0.00465*k_convert*24.
               xx = (1-0.00545*(k_convert*24/Vconvert))

               if ( xx .gt.0.) then
                  decay = -1.09*log(1.-0.00545*(k_convert*24./Vconvert))
                  slope = exp(-decay)
                  slope2 = exp(-decay*w*x)

                  if((1.-slope).gt..01) then
                     intercept2 = (intercept/(1.-slope))*(1.-slope2)
                  else 
                     intercept2 = (intercept/0.01)*(1.-slope2)
                  end if

                  tlc = (Vconvert-(intercept2+slope2*Vconvert)) * 1234.6 !acre-feet to m3
               else
                  tlc = 0.
               end if
            end if ! (volrt.ge.phi(5,j))
                     
            tlc2 = tlc*sdtsav(nreach)/(xxqd+xxssf+sdtsav(nreach))
               
            if (sdtsav(nreach).le.tlc2) then
               tlc2 = sdtsav(nreach)
               sdtsav(nreach) = sdtsav(nreach) - tlc2
               tlc1 = tlc -tlc2
               if(qdandssf .le. tlc1) then
                  tlc1 = xxqd+xxssf
                  xxqd = 0.
                  xxssf = 0.
               else
                  xxqd = xxqd - tlc1*qd_ssf
                  xxssf = xxssf - tlc1*(1.-qd_ssf)
               end if
            else
               sdtsav(nreach) = sdtsav(nreach) - tlc2
               tlc1 = tlc -tlc2
               if (qdandssf .le. tlc1) then
                  tlc1 = xxqd+xxssf
                  xxqd = 0.
                  xxssf = 0.
               else
                  xxqd = xxqd - tlc1*qd_ssf
                  xxssf = xxssf - tlc1*(1.-qd_ssf)
               end if 
            end if
            tlc = tlc1 + tlc2
         end if ! (qdandssf.gt.0.)
      end if ! ( tlrch > 0. )

!**** calculate the evaporation losses in rivers (by Huang, 2013, based on SWAT code)  
      if ( evrch > 0. ) then
         qdandssf = xxqd+xxssf
         if (qdandssf.gt.0.) then
            !! calculate width of channel at water level
            topw = 0.
            if (rchdep .le. d) then
            !topw = b + 2. *rchdep*chside      !SWAT version
               topw = 10.**(log10(sdti)*0.494+1.031)  ! river width, WASA version
            else
               topw = 5.*chw(2,j)+ 2.*(rchdep-chd(j))*4.
            end if

            evp = evrch *pet_day(j)*chl(2,j)*topw
            evp2 = evp*sdtsav(nreach)/(xxqd+xxssf+sdtsav(nreach))
                     
            if (sdtsav(nreach).le.evp2) then
               evp2 = sdtsav(nreach)
               sdtsav(nreach) = sdtsav(nreach) - evp2
               evp1 = evp -evp2
               if (qdandssf .le. evp1) then
                  evp1 = xxqd+xxssf
                  xxqd = 0.
                  xxssf = 0.
               else
                  xxqd = xxqd - evp1*qd_ssf
                  xxssf = xxssf - evp1*(1-qd_ssf)
               end if
            else
               sdtsav(nreach) = sdtsav(nreach) - evp2
               evp1 = evp -evp2
               if (qdandssf .le. evp1) then
                  evp1 = xxqd+xxssf
                  xxqd = 0.
                  xxssf = 0.
               else
                  xxqd = xxqd - evp1*qd_ssf
                  xxssf = xxssf - evp1*(1.-qd_ssf)
               end if 
            end if

            evp = evp1 + evp2
         end if ! (qdandssf.gt.0.)
      end if ! evrch > 0.
 
   else
      xxqd = 0.
      xxssf = 0.
      sdti = 0.
   end if ! (sdti.gt.0.)
      
   if(xxqd .lt. 0.) xxqd = 0.
   if(xxssf .lt. 0.) xxssf = 0.
   if(sdti .lt. 0.) sdti = 0.
   if(sdtsav(nreach) .lt. 0.) sdtsav(nreach) = 0.

end subroutine tran_river

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
