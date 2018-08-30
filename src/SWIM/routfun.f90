!     FILE routfun.f
!
!     SUBROUTINES IN THIS FILE                          CALLED FROM
!     subroutine  rthyd(j,ihout,inum1,inum2)            route
!     subroutine  rtsed(j,ihout,inum1,inum2)            route
!     subroutine  enrrt(j,ihout,inum1,inum2)            route
!     subroutine  rtorgn                                route
!     subroutine  rtpsed                                route
!     subroutine  ttcoefi(j)                            main
!     subroutine  coefs(qq1,q2,tt1,tt2,p1,pp2)          ttcoefi(j)
!     subroutine  qman(a,rh,xn,chslope)                 ttcoefi(j)



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine rthyd(j,ihout,inum1,inum2)
!**** PURPOSE: this subroutine routes hydrograph to basin outlet 
!**** CALLED IN:   ROUTE
!     METHOD:  MUSKINGUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!   
!tit  j=inum1   = Reach No.
!tit  inum2     = Inflow Hyd.(inum2 hydrograph is routed through 
!                 inum1 hydrograph)

!      >>>>> COMMON PARAMETERS & VARIABLES
!      chl(2,j)         = channel length, km
!      ida              = current day
!      iyr              = current year
!      phi(5,j)         = "normal" flow
!      phi(10,j)        = prelim. estimation of xkm for bankfull depth
!      phi(13,j)        = prelim. estimation of xkm for 0.1 bankfull depth
!      qdilast(ih)      = qdinp(,) in the last day of the year, m3
!      qdinp(ih,ida)    = surface flow - daily input in reaches, m3
!      qdolast(ih)      = qdout(,) in the last day pof the year, m3
!      qdout(ih,ida)    = surface flow - daily output in reaches, m3
!      qsilast(ih)      = qssinp(,) in the last day of the year, m3
!      qsolast(ih)      = qssout(,) in the last day of the year, m3
!      qssinp(ih,ida)   = subsurface + g-w flow - daily input in reaches, m3
!      qssout(ih,ida)   = subsurface + g-w flow - daily output in reaches, m3
!      roc1             = 0
!      roc2             = calibration parameter for routing
!      roc3             = 0
!      roc4             = calibration parameter for routing
!      sdti             = water inflow + storage in the reach, m3/sec.
!      sdtsav(ir)       = water storage in the reach, m3
!      xxqd             = surface flow - daily input in reaches, m3
!      xxssf            = subsurface + g-w flow - daily input in reaches, m3
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      c1      = local par
!      c2      = local par
!      c3      = local par
!      c4      = local par
!      det     = local par
!      nreach  = reach No.
!      qdin    = xxqd
!      ssfin   = xxssf
!      xat     = dimentionless weighting factor for routing
!      xkm     = storage time constant for the reach
!      yy      = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      implicit NONE
      integer :: j,ihout,inum1,inum2,nreach
      real(8)    :: c1,c2,c3,c4,qdin,ssfin,xkm !,det,xat,yy
      
      ! SL only a preliminary solition
      if ( xxqd <= 0. ) then
         xxqd  = max(0.00006,xxqd)
         if ( bErrorFile ) write(1000,*)ida,j,"1. rthyd(): xxqd < 0.0 --> set to 0.00006"
      end if
      if ( xxssf <= 0. ) then
         xxssf = max(0.00006,xxssf)
         if ( bErrorFile ) write(1000,*)ida,j,"1. rthyd(): xxssf < 0.0 --> set to 0.00006"
      end if
      
      nreach = inum1
      qdinp(ihout,ida) = xxqd
      qssinp(ihout,ida) = xxssf
      qdin = xxqd
      ssfin = xxssf

!**** COMPUTE sdti = INFLOW + STORAGE, transfer m3 --> m3/sec
      sdti = (qdin+ssfin+sdtsav(nreach)) / 86400.
      
!*********************************************************** STEP 1  
!**** COMPUTE SURFACE flow routing
! SL  The variables xkm, c1, c2, c3, and c4 were calculated for surface and subsurface routing
!     every time step this function is called.
!     This is actually not necessary because they need to be computed based on subbasins'
!     channell parameters only once during the pre-processing.
!     This is now done in subroutine ttcoefi()

      if(iy.eq.1.and.ida.eq.1) then
         xxqd = c1_qd(j)*xxqd + c2_qd(j)*sdtsav(inum1) + c3_qd(j)*sdtsav(inum1) + c4_qd(j)
      else
         if (ida.gt.1) then      
            xxqd = c1_qd(j)*xxqd + c2_qd(j)*qdinp(ihout,ida-1) + c3_qd(j)*qdout(ihout,ida-1)
         else
            xxqd = c1_qd(j)*xxqd + c2_qd(j)*qdilast(ihout) + c3_qd(j)*qdolast(ihout)
         end if
      end if

!*********************************************************** STEP 2  
!**** COMPUTE SUBSURFACE flow routing
! SL  The variables xkm, c1, c2, c3, and c4 were calculated for surface and subsurface routing
!     every time step this function is called.
!     This is actually not necessary because they need to be computed based on subbasins'
!     channell parameters only once during the pre-processing.
!     This is now done in subroutine ttcoefi()

      if(iy.eq.1.and.ida.eq.1) then
         xxssf = c1_ssf(j)*xxssf + c2_ssf(j)*sdtsav(inum1) + c3_ssf(j)*sdtsav(inum1) + c4_ssf(j)
      else
         if (ida.gt.1) then
            xxssf = c1_ssf(j)*xxssf + c2_ssf(j)*qssinp(ihout,ida-1) + c3_ssf(j)*qssout(ihout,ida-1)
         else
            xxssf = c1_ssf(j)*xxssf + c2_ssf(j)*qsilast(ihout) + c3_ssf(j)*qsolast(ihout)         
         end if
      end if


!sl begin
!!!!!! DO NOT ALLOW NEGATIVE FLOWS
!!!!!! This shouldn't be a long-term solution but an attempt to make the user aware of
!!!!!! obviously wrong parameter settings
      if (xxqd < 0.) then
         if ( bErrorFile ) write(1000,*)ida, j, "2. rthyd(): xxqd is negative! and set to almost zero",xxqd
         xxqd = 0.000006
      end if
      if (xxssf < 0.) then
         if ( bErrorFile ) write(1000,*)ida, j, "2. rthyd(): xxssf is negative! and set to almost zero",xxssf
         xxssf = 0.000006
      end if
!sl end

!**** CALC outputs   
      qdout(ihout,ida) = xxqd
      qssout(ihout,ida) = xxssf

!**** CALC water storage in the reach
      sdtsav(nreach) = sdtsav(nreach) + qdin + ssfin - xxqd - xxssf
      if (sdtsav(nreach).lt.0.1) then
        sdtsav(nreach) = 0.
      endif

!**** STORE input-output in the last day of the year:
      if(mod(iyr,4).eq.0.and.ida.eq.366) then
        qdilast(ihout)=qdinp(ihout,ida)
        qdolast(ihout)=qdout(ihout,ida)
        qsilast(ihout)=qssinp(ihout,ida)
        qsolast(ihout)=qssout(ihout,ida)
      endif
      if(mod(iyr,4).ne.0.and.ida.eq.365) then
        qdilast(ihout)=qdinp(ihout,ida)
        qdolast(ihout)=qdout(ihout,ida)
        qsilast(ihout)=qssinp(ihout,ida)
        qsolast(ihout)=qssout(ihout,ida)
      endif
     
end subroutine rthyd

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      

      subroutine rtsed(j,ihout,inum1,inum2)
!**** PURPOSE: THIS SUBROUTINE ROUTES SEDIMENT FROM SUB-BASIN TO BASIN OUTLET 
!**** CALLED IN:   ROUTE
!      METHOD:  from J. WILLIAMS (SWAT98)      
!      DEPOSITION IS BASED ON FALL VELOCITY AND DEGRADATION - ON STREAM POWER 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      PARAMETERS & VARIABLES
!   
!       >>>>> COMMON PARAMETERS & VARIABLES
!       chc(j)     = channel USLE C factor
!       chnn(j)    = channel N value
!       chss(j)    = channel slope, m/m
!       chw(2,j)   = average width of main channel, m
!       chxk(j)    = channel USLE K factor
!       ida        = current day
!       irout      = switch code to print from routfun()
!       prf        = coef. to estimate peak runoff in stream      
!       sdti       = water inflow + storage in the reach, m3/sec.
!       sdtsav(ir) = water storage in the reach, m3
!       spcon      = rate parameter for estimation of sediment transport, readbas    
!       spexp      = exponent for estimation of sediment transport, readbas
!       xxqd       = surface flow - daily input in reaches, m3
!       yd         = daily soil loss from subbasin caused by water erosion, t
!       >>>>>

!       >>>>> STATIC PARAMETERS
!       ach      = local par
!       bwd      = local par
!       cych     = local par
!       cyin     = local par
!       d        = f(sdti,chnn,chw,chss)
!       deg      = degradation
!       dep      = deposition
!       depnet   = local par
!       nsz      = local par
!       prst     = peak runoff in stream
!       qdin     = surface inflow + water storage
!       vc       = f(sdti,chw,d)
!       vs       = local par
!       ydin     = yd
!       >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      implicit NONE
      integer j,ihout,inum1,inum2
      real(8) ach,cych,cyin,d,deg,dep,depnet,prst,qdin
      real(8) vc,ydin

      prst = prf * sdti
      qdin = xxqd + sdtsav(inum1)
      ydin = yd
      if (qdin.le.0.01) return

!**** COMPUTE FLOW DEPTH 
      d = ((prst*chnn(j))/(chw(2,j)*chss(j)**.5)) ** .6
      ach = d * chw(2,j)
      if(d.lt..010 ) then
        vc = 0.01
      else
        vc = prst / ach
      endif
      if (vc.gt.3.) vc = 3. 
      
!**** CALCULATE DEPOSITION & DEGRADATION (either - or) 
      cyin = ydin / qdin
      cych = spcon * vc ** spexp
      depnet = qdin * (cych - cyin)
      if (depnet.gt.0.) then
        deg = depnet * chxk(j) * chc(j)
        dep = 0.
      else
        dep = -depnet
        deg = 0.
      endif

      yd = yd + deg - dep
      if (yd.lt.0.) yd = 0.
      
      if(irout.eq.1) then
      if (j.eq.1.and.ydin.gt.0.01) then 
         write(49,100) ida,j,ydin,yd,yd/ydin, depnet,deg,dep
      endif
      endif
                      
  100 format('rtsed: ',2i5,2f12.3,f8.3,3f12.3)
      return
      end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




      subroutine enrrt(j,ihout,inum1,inum2)
!**** THIS SUBROUTINE CALCULATES ENRICHMENT RATIO
!**** CALLED IN:  ROUTE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      PARAMETERS & VARIABLES
!   
!       >>>>> COMMON PARAMETERS & VARIABLES
!       da9        = drainage area, ha
!       er         = enrichment coefficient
!       sdtsav(ir) = water storage in the reach, m3
!       xxqd       = surface flow - daily input in reaches, m3
!       ydi        = yd, daily soil loss caused by water erosion, t, 
!                    in route before call rtsed 
!       yd         = daily soil loss caused by water erosion, t, recalc in rtsed
!       >>>>>

!       >>>>> STATIC PARAMETERS 
!       cy    = local par
!       dr    = local par
!       qdin  = inflow + watre storage
!       x1    = local par
!       x2    = local par
!       >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      implicit NONE
      integer j,ihout,inum1,inum2
      real(8) cy,dr,qdin,x1,x2

      qdin = xxqd + sdtsav(inum1)
      if (qdin.le.0.1E-20) return

!**** CALC enrichment coefficient
      dr = yd / (ydi+1.e-10)
      if (dr.le.0.01) dr = .01
      if (dr.ge.10.) dr = 10.
      cy = 100000. * ydi / (da9*qdin)
      x2 = -log10(dr) / 2.699
      x1 = 1. / .25 ** x2
      er = x1 * (cy+1.e-10) ** x2
      if (er.lt.1.) er = 1.
      if (er.gt.3.) er = 3.

      return
      end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine rtorgn
!**** PURPOSE:  THIS SUBROUTINE CALCULATES ORGANIC N ROUTING
!**** CALLED IN:    ROUTE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      PARAMETERS & VARIABLES
!   
!       >>>>> COMMON PARAMETERS & VARIABLES
!       conn  = xnorg * er, g/t: N org. in I layer for subbasin, 
!               corrected for enrichment
!       da9   = drainage area, ha
!       er    = enrichment coefficient
!       ydi   = yd, daily soil loss caused by water erosion, t, 
!               in route before call rtsed 
!       yon   = org N loss with erosion, kg/ha, routed
!       >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      implicit NONE

      yon = .001 * conn * er * ydi / da9
      return
      end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine rtpsed
!**** PURPOSE: THIS SUBROUTINE CALCULATES PHOSPHORUS ROUTING 
!**** CALLED IN:  ROUTE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      PARAMETERS & VARIABLES
!   
!       >>>>> COMMON PARAMETERS & VARIABLES
!       cpp  = xporg * er, g/t: P org. in I layer in subbasin, g/t
!                  corrected for enrichment
!       da9  = drainage area, ha
!       ydi  = yd, daily soil loss caused by water erosion, t, 
!                 in route before call rtsed 
!       yph  = P org. loss with erosion, kg/ha
!       >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      implicit NONE

      yph = .001 * cpp * ydi / da9
      return
      end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine ttcoefi(j)
!**** THIS SUBROUTINE COMPUTES TRAVEL TIME COEFFICIENTS FOR ROTING,
!      CALLS COEFS and QMAN 
!**** CALLED IN:   MAIN 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      PARAMETERS & VARIABLES
!   
!       >>>>> COMMON PARAMETERS & VARIABLES
!       chd(j)          = channel depth, m
!       chl(2,j)        = main channel length, km
!       chnn(j)         = channel N value
!       chss(j)         = channel slope, m/m
!       chw(2,j)        = average width of main channel, m
!       phi(1,j),phi(2,j) = coefs for travel time estimation, if higher than 
!                           normal flow = 1.2 depth
!       phi(3,j),phi(4,j) = coefs for travel time estimation, if lower than 
!                           normal flow = 0.1 depth
!       phi(5,j)        = flow rate when reach is at bankfull depth, m3/sec
!       phi(6,j)        = bottom width of main channel, m
!       phi(7,j)        = depth of water when reach is at bankfull depth, m 
!       phi(8,j)        = average velocity when reach is at bankfull depth, m/s
!       phi(9,j)        = wave celerity when reach is at bankfull depth, m/s
!       phi(10,j)       = prelim. estimation of xkm for bankfull depth:
!                         storage time constant for reach at bankfull depth 
!                         (ratio of storage to discharge), h
!       phi(11,j)       = average velocity when reach is at 0.1 bankfull depth 
!                         (low flow), m/s
!       phi(12,j)       = wave celerity when reach is at 0.1 bankfull depth 
!                         (low flow), m/s
!       phi(13,j)       = prelim. estimation of xkm for 0.1 bankfull depth:
!                         storage time constant for reach at 0.1 bankfull depth 
!                         (low flow) (ratio of storage to discharge), h
!       >>>>>

!       >>>>> STATIC PARAMETERS
!       a      = local par
!       a1     = local par
!       aa     = local par
!       b      = local par
!       celer  = celerity
!       chside = local par
!       d      = local par
!       fpn    = local par
!       fps    = local par
!       p      = local par
!       p1     = from coefs
!       pp2    = from coefs
!       q2     = average flow
!       qq1    = local par
!       rh     = hydraulic radius (m)
!       tmne   = local par
!       tt1    = local par
!       tt2    = local par
!       velos  = velocity
!       ykm    = xkm in hours (I estimation of xkm)
!       >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
!**** Include common parameters
      use common_par
      implicit NONE
      integer j, i
      real(8) a,a1,aa,b,celer,chside,d,fpn,fps,p,p1
      real(8) pp2,q2,qq1,rh,tmne,tt1,tt2,velos,ykm
      real(8) qman
      real(8) :: det,xat,yy

      chside = 2.
      fps = 4.
      fpn = chnn(j)
      d = chd(j)
      b = chw(2,j) - 2. * d * chside

!**** CALC bottom width b & channel side chside
      if(b.le.0.) then
        b=.5*chw(2,j)
        chside=(chw(2,j)-b)/(2.*d)
      endif

!**** CALC average flow q2 
      phi(6,j) = b
      phi(7,j) = d
      p = b + 2. * d * sqrt(chside*chside+1)
      a = b * d + chside * d * d
      rh = a / p
      q2 = qman(a,rh,chnn(j),chss(j))

!**** CALC velocity, celerity and storage time constant for bankfull depth 
      aa= 1.      
      velos =  qman(aa,rh,chnn(j),chss(j))
      celer = velos * 5. / 3.
!      UNITS: chl() -in km!  km/ --> m/sec    /3600 *1000
      ykm =  chl(2,j)  / celer / 3.6   
      phi(5,j) = q2
      phi(8,j) = velos
      phi(9,j) = celer
      phi(10,j) = ykm
      tt2 = chl(2,j) * a / q2

!**** CALC velocity, celerity and storage time constant for 1.2 bankfull depth
!      depth = 1.2 * chd
!#### CALL COEFS
      d = 1.2 * chd(j)
      a1 = chw(2,j) * chd(j) + fps * (d-chd(j)) ** 2
      p1 = 2. * chd(j) * sqrt(fps*fps+1)
      rh = (a+a1) / (p+p1)
      tmne = (chnn(j)*a+fpn*a1) / (a+a1)
      a = a + a1
      qq1 = qman(a,rh,tmne,chss(j))      
      tt1 = chl(2,j) * a / qq1
      call coefs(qq1,q2,tt1,tt2,p1,pp2)            
      phi(1,j) = p1
      phi(2,j) = pp2

!**** CALC velocity, celerity and storage time constant for 0.1 bankfull depth
!      depth = 0.1 * chd
!#### CALL COEFS
      d = 0.1 * chd(j)
      p = b + 2. * d * sqrt(chside*chside+1)
      a = b * d + chside * d * d
      rh = a / p
      qq1 = qman(a,rh,chnn(j),chss(j))
      tt1 = chl(2,j) * a / qq1
      call coefs(qq1,q2,tt1,tt2,p1,pp2)            
      phi(3,j) = p1
      phi(4,j) = pp2
      aa = 1.      
      velos =  qman(aa,rh,chnn(j),chss(j))
      celer = velos * 5. / 3.
!      UNITS: chl() -in km!  km/ --> m/sec    /3600 *1000
      ykm =  chl(2,j)  / celer / 3.6   
      phi(11,j) = velos
      phi(12,j) = celer
      phi(13,j) = ykm


! SL BEGIN ------------------------------------------------------------------------
      xat = 0.05 !0.1 ! 0.2  ! dimensionless weighting factor for routing
      det = 24.
      
!#### Compute MUSKINGUM ROUTING PARAMETERS for surface flow component
      xkm_qd(j)  = phi(10,j)*roc1 + phi(13,j)*roc2(j)
      yy = 2.*xkm_qd(j)*(1.-xat) + det
      c1_qd(j)  = (det - 2.*xkm_qd(j)*xat) / yy
      c2_qd(j)  = (det + 2.*xkm_qd(j)*xat) / yy
      c3_qd(j)  = (2.*xkm_qd(j)*(1.-xat) - det) / yy
      c4_qd(j)  = phi(5,j)*chl(2,j)*det / yy
!*** Correction to avoid oszillation and negative discharge
      if (c3_qd(j) < 0. ) then
         c2_qd(j) = c2_qd(j) + c3_qd(j)
         c3_qd(j) = 0.
      end if
      if (c1_qd(j) < 0.) then
         c2_qd(j) = c2_qd(j) + c1_qd(j)
         c1_qd(j) = 0.
      end if

!#### Compute MUSKINGUM ROUTING PARAMETERS for sub-surface flow component
      xkm_ssf(j) = phi(10,j)*roc3 + phi(13,j)*roc4(j)
      yy = 2.*xkm_ssf(j)*(1.-xat) + det
      c1_ssf(j)  = (det - 2.*xkm_ssf(j)*xat) / yy
      c2_ssf(j)  = (det + 2.*xkm_ssf(j)*xat) / yy
      c3_ssf(j)  = (2.*xkm_ssf(j)*(1.-xat) - det) / yy
      c4_ssf(j)  = phi(5,j)*chl(2,j)*det / yy
!*** Correction to avoid oszillation and negative discharge
      if (c3_ssf(j) < 0. ) then
         c2_ssf(j) = c2_ssf(j) + c3_ssf(j)
         c3_ssf(j) = 0.
      end if
      if (c1_ssf(j) < 0.) then
         c2_ssf(j) = c2_ssf(j) + c1_ssf(j)
         c1_ssf(j) = 0.
      end if
!*** Correction on Muskingum routing
! SL END --------------------------------------------------------------------------
      
!      if(irout.eq.1) write(49,102) qq1,tt1,a,rh,chnn(j),chss(j)      
!      if(irout.eq.1) write(49,103) j,(phi(i,j),i=1,7),chw(2,j)
!      if(irout.eq.1) write(49,103) j,(phi(i,j),i=1,13)      

  102 format (4x,6f10.3)
  103 format (i5,15f10.3)
      return
      end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine coefs(qq1,q2,tt1,tt2,p1,pp2)
!**** PURPOSE:  THIS SUBROUTINE CALCULATES coefficients p1 and pp2
!**** CALLED IN:   TTCOEFI
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      implicit NONE
      real(8) qq1,q2,tt1,tt2,p1,pp2

      pp2 = dlog(tt1/tt2) / dlog(qq1/q2)
      if (pp2.lt.-1.) pp2 = -1.
      if (pp2.gt.1.5) pp2 = 1.5
      p1 = tt1 / (qq1**pp2)
      return
      end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      function qman(a,rh,xn,chslope)
!**** PURPOSE: THIS SUBROUTINE CALCULATES FLOW USING MANNINGS EQUATION 
!**** CALLED IN:   TTCOEFI  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      implicit NONE
      real(8) a,rh,xn,chslope,qman

      qman = a * rh ** .6666 * sqrt(chslope) / xn
      return
      end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&






