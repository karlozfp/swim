!     FILE vegfun.f
!
!     SUBROUTINES IN THIS FILE                  CALLED FROM
!     subroutine wstress(j,je,k,n)              crpmd, vegmd
!     subroutine tstress(tgx,j,je,n)            crpmd, vegmd
!     subroutine npstress(u1,u2,uu)             nuptake, puptake
!     subroutine scurve(x1,x2,x3,x4)            crpmd, adjustbe
!     subroutine ascrv(x1,x2,x3,x4,x5,x6)       readcrp
!     subroutine adjustbe(j,je,xtem,beadj)      crpmd



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine wstress(j,je,k,n,wet)
!**** PURPOSE: THIS SUBROUTINE ESTIMATES WATER STRESS FACTOR.
!              IT DISTRIBUTES POTENTIAL PLANT EVAPORATION THROUGH
!              THE ROOT ZONE AND CALCULATES ACTUAL PLANT WATER USE
!              BASED ON SOIL WATER AVAILABILITY
!**** CALLED IN:  CRPMD, VEGMD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!
!      >>>>> COMMON PARAMETERS & VARIABLES
!      alfa        = calculated in adjustbe
!      co2ref      = CO2 in reference  period, ppm
!      co2sce      = CO2 in  simulated period, ppm
!      ep          = plant evaporation, mm
!      g(j,je)     = fraction heat units accumulated, 1/1
!      humi(j)     = air humidity, %
!      ialpha      = switch parameter: to calc CO2 effect on net photosynthesis?
!      ibeta       = switch parameter: to calc CO2 effect on transpiration?
!      ida         = current day
!      ilcc(iv)    = land cover category for vegetation iv
!      iwshd       = number of hydrotope to print from wstress(), if iwstr = 1
!      iwssb       = number of subbasin to print from wstress(), if iwstr = 1
!      iwstr       = switch code to print from wstress()
!      nn          = number of soil layers
!      nucr(j,je)  = crop number (database)
!      nveg(j,je)  = vegetation number (database)
!      potentl     = plant evaporation, potential, mm
!      precip      = precipitation, mm
!      rd(j,je)    = root depth, mm
!      rdmx(iv)    = max root depth, mm
!      sep         = water percolation
!      ste(j,je,l) = water storage, recalc here, mm
!block ub          = 3.065
!      ul(l,k)     = upper limit water content in layer, mm
!      uob         = pap calc in readbas from ub, ub=3.065 blockdata
!      ws(j,je)    = water stress factor
!      z(l,k)      = depth of layer, mm
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      beta   = beta factor for plant transpiration under higher CO2
!      ccfi   = local par
!      epin   = local par
!      gx     = local par
!      ir     = local par
!      l      = local par
!      num    = local par
!      sum    = local par
!      u      = local par |mm H2O        |water uptake by plants in each soil layer
!      ul4    = local par
!      xx     = local par |mm H2O        |water uptake by plants from all layers
!      xx1    = local par
!      xx2    = local par
!      xx3    = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
!       use mod_water_allocation !#### WATER ALLOCATION MODULE ####
      implicit NONE
      integer j,je,n,k,ir,l,num,wet
      real(8) beta,ccfi,epin,gx,sum,ul4,xx,xx1,xx2,xx3
      real(8) areahyd,additionalUptake,co2sce_interpolated
      real(8), dimension(10) :: u
      real(8) zu, sump ! epco as a global calibration parameter in *.bsn
      real(8) rdmx_temp
!      real(8)                       :: area = 0. !#### WATER ALLOCATION MODULE ####
!      real(8)                       :: irr_fac !#### WATER ALLOCATION MODULE ####
!      TYPE (TSubbasin), POINTER  :: pS,pSs, pSd  ! water allocation module !#### WATER ALLOCATION MODULE ####
!      TYPE (TWaterUser), POINTER :: pWU ! pointer on actual TWU   !#### WATER ALLOCATION MODULE ####

      u = 0.
      potentl = ep

! The factor "epco" is introduced to limit plant water demand
! compensation from lower layers.
! Without epco (or epco=1) water demand which could not be met
! by an individual layer was moved to the next lower layer.
! epco=0 means no compensation. epco=0.5 means 50% compensation
! from next lower layer.
! It is basically a calibration parameter and reflects
! the plants flexibility to respond to water limitations
! with increased root growth. -- Pia Gottschalk
!**** SL
      !epco=0.25
      ! epco is now a calibration paramter in *.bsn
!**** SL

!**** Including factor beta for plant transpiration under higher CO2
!               as a constant beta coupled to alfa
      if (ialpha.eq.1.and.ibeta.eq.1) then
! Calculation of transient CO2 increase = simple interpolation
! from start year CO2 concentration (co2ref)
! to end year CO2 concentration (co2sce):
        co2sce_interpolated=((co2sce-co2ref)/(nbyr-1))*(iy-1)+co2ref
!        beta = alfa * co2sce / co2ref
! Correction of calculation of beta, Pia Gottschalk, 23/08/2011
        beta = alfa * co2ref/co2sce_interpolated
        ep = ep * beta
      end if

      epin=ep
      xx = 0.

      if (n.eq.5) then
         num=nucr(j,je)
      else
         num=nveg(j,je)
      end if
!**** CALC plant transpiration ep and water stress ws()
!     Correction from F. Badeck: ul4 = ul()*0.6, 60% instead of 25% before
!     Correction from P. Gottschalk, 19/07/11:
!     ul4 = ul()*0.5, 50% instead of 25% (SWIM original) before
!     Correction from P. Gottschalk, 19/07/11, is based on
!     "Leitfaden zur Beregnung landwirtschaftlicher Kulturen",
!     LVLF, September 2005, Tabelle 3
      if (ep.le.0.) then
         ws(j,je) = 1.
      else
         l    = 1
         ir   = 0
         sum  = 0.
         sump = 0.
!       select case (ilcc(num))
!          case (1, 2, 4, 5)
!             rd(j,je) = 2.5 * g(j,je) * rdmx(num)
!             if ( rd(j,je) > rdmx(num) ) rd(j,je)= rdmx(num)
!             !if (sol_rd < 10.) sol_rd = 10.
!          case default
!             rd(j,je) = rdmx(num)
!       end select
! Pia: rdmx (max. rooting depth) is adjusted to the depth
! of the arable layer or max rooting depth (from crop.dat)
! if the depth of the arable layer is greater than the
! crop specific max root depth.
! To adjust hydrotop specific water uptake the variable
! "rdmx_temp" is introduced here locally.
! This prevents that the loop over the arable layer to calculate
! water uptake generates a water stress artefact.
! Before, water demand was calculated along the whole root profile
! but the loop only ran over the arable layer.
! Note, this effectively reduces plant water stress
! because max. root depth for winter wheat is set to 2 m
! whereas the arable layer is usually set to a shallower layer.
      rdmx_temp=min(rdmx(num),z(nn,k))
      if (ilcc(num).le.2.or.ilcc(num).eq.4.or.ilcc(num).eq.5) then
         rd(j,je) = 2.5 * g(j,je) * rdmx_temp
         if (rd(j,je).le.rdmx_temp) go to 20
      end if
      rd(j,je) = rdmx_temp

   20 xx = 0.
      zu = 0.
      do l = 1, nn
         if (ir.gt.0) go to 40
         if (rd(j,je).le.z(l,k)) then
            gx = rd(j,je)
            ir = l
         else
            gx = z(l,k)
         end if
         if (rd(j,je).le.0.) then
            sum = ep / uob
         else
            if (num.eq.69) then
               sum = ep * (1.-exp(-ub)) / uob
            else
               sum = ep * (1.-exp(-ub*gx/rd(j,je))) / uob
            end if
         end if
         
         sump = sum
!          u(l) = sum - xx
         u(l) = sum - sump + (sump-xx)*epco
! Pia: Commented out as the threshold from which plant water
! uptake is reduced due to increasing matrix potential (here 0.5)
! refers to the plant available water (=fc) and not to saturation (=ul)
! Note, this reduces plant water stress.
!         ul4 = ul(l,k) * 0.5
         ul4 = fc(l,k) * 0.75

! Pia: Commented out: linear decline of plant water uptake below fc*0.5
!            u(l) = u(l) * ste(j,je,l) / ul4
! Pia: Now using SWAT approach which features a exponential decline
! of plant water uptake below fc*0.5
         if ( ste(j,je,l).lt.ul4 )  u(l) = u(l) *exp(5*(ste(j,je,l)/ul4-1))
         
         if ( ste(j,je,l).lt.u(l) ) u(l) = ste(j,je,l)
         ste(j,je,l) = ste(j,je,l) - u(l)
         xx = xx + u(l)
         zu = z(l,k)
      end do ! l = 1, nn
      
   40   ws(j,je) = xx / ep
        ep = xx

!         !#################################
!         !#### WATER ALLOCATION MODULE ####
!         !#################################
!         ! calculate plant water demand
!         if ( wam_bTransfer ) then
!            if ( wam_is_transfer_subbasin(j) ) then
!               if ( n == 5 .AND. mstrucx(j,je,1) >= 1 ) then ! if hydrotope is irrigated cropland
!                  area = mstrucx(j,je,2)                     ! hydrotope area, m^2
!                  pS => wam_get_pointer_subbasin(j)          ! pointer on current subbasin (could be source or destination)
!                  pWU => TWU(pS%pos_irr)                     ! pointer on water user
!                  
!                  !-----------------------------------------------------------------
!                  ! Plant and total irrigation requirements are calculated here.
!                  ! If irr_opt == 1, these requirements are written to the output files, but
!                  ! the amount of water used for irrigation will be overwritten by input time series in: wam_withdraw_Transfer_Out()!!!
!                  !-----------------------------------------------------------------
!                  if ( pWU%wu_opt == 4 ) then ! if source is not external but another subbasin
!                     ! calculate plant demand but only for output purposes because under this option,
!                     ! water is provided (input time series) independently from requirements.
!                     pWU%plantDemand(daycounter)      = pWU%plantDemand(daycounter) + (xx-ep)/1000.*area/86400. * -1. ! mm -> m3/s
!                     ! calculate actual irrigation demand including losses depending irrigation practices
!                     pWU%irrigationDemand(daycounter) = wam_correct_irrigationDemand(pWU,pWU%plantDemand(daycounter))
! 
!                     ! summarise total plant demand and irrigation demand for summary output file
!                     wam_plantDemand_summary(daycounter)      = wam_plantDemand_summary(daycounter)      + pWU%plantDemand(daycounter)
!                     wam_irrigationDemand_summary(daycounter) = wam_irrigationDemand_summary(daycounter) + pWU%irrigationDemand(daycounter)
!                     
! !                     if ( pWU%subs > 0 ) then ! input from source subbasin
! !                        pSs => wam_get_pointer_subbasin(pWU%subs) ! pointer on source subbasin
! !                        if ( pWU%irr_opt == 1 ) pSs%irrDemand(daycounter) = pWU%data(daycounter) ! overwrite with value from input time series
! !                        if ( pWU%irr_opt >  1 ) pSs%irrDemand(daycounter) = pWU%irrigationDemand(daycounter) ! m3/s
! !                     end if
! !                     if ( pWU%subs == 0 ) then ! input from external source
! !                        pSd => wam_get_pointer_subbasin(pWU%subd) ! pointer on source subbasin
! !                        pSd%irrDemand(daycounter) = pWU%irrigationDemand(daycounter) ! m3/s
! !                     end if
! 
!                  end if ! ( pWU%wu_opt == 4 )
!               end if ! ( n == 5 .AND. mstrucx(j,je,1) >= 1 )
!            end if ! ( wam_is_transfer_subbasin(j) )
!         end if ! ( wam_bTransfer )
!         !#################################

      end if

!**** Correction on air humidity
      if (ida.gt.100.and.humi(j).gt.75.) then
         xx2 = potentl - ep
!        ccfi = (humi(j) - 75.)/10.
!        ccfi = 2.*(humi(j) - 75.)/10.
         ccfi = 1.
         if (ccfi.gt.1.) ccfi = 1.
         if (xx2.gt.0.) then
!           xx3=min(sep,ccfi*xx2)
            xx3=ccfi*xx2
            if (xx3.gt.sep) xx3 = sep
            ep  =  ep + xx3
            sep = sep - xx3
            if (sep.lt.0.) sep = 0.
            ws(j,je) = ep / potentl
         end if
      end if

!**** Riparian zone, Version 2 (Fred Hattermann):
!     ep is increased for soils 3-17 (Auenboden),
!     water is taken from sep, sep can become negative
!      if (k.ge.3.and.k.le.17.and.rd(j,je).gt.2.*rdmx(num)/3.)
!     *    then
!          xx1 = potentl - ep
!         if (xx1.gt.0.) then
!            ep = ep + 0.1 * xx1
!            sep = sep - 0.1 * xx1
!            ws(j,je) = ep / potentl
!         endif
!      endif

!       new Riperian zone
      if (wet==1.and.gwqLastday(j)>0) then

!       calculate transpiration deficite xx1
             xx1 = potentl - ep
         if (xx1.gt.0.) then
!       uptake is limitited to rzmaxup
                    if (xx1>rzmaxup) xx1=rzmaxup
                    ep = ep +  xx1
!       calc hydrotop area
                    areahyd=frar(j,je)*dart(j)
!       calculate additional uptake in m3
                    additionalUptake= xx1*areahyd*1000

!       to avoid negative flow
                if ((gwqLastday(j)-additionalUptake)<0) then
                   ep = ep -  xx1
                   additionalUptake=gwqLastday(j)
!       increase Et by additional uptake
                   ep=ep+additionalUptake/(areahyd*1000)
                end if

                gwqLastday(j)         = gwqLastday(j) - additionalUptake
                additionalGwUptake(j) = additionalGwUptake(j) + additionalUptake
            ws(j,je) = ep / potentl
             end if
          end if





      if(sep.lt.0..and.k.gt.17) then
        write(44,104) ida,j,n,k,humi(j),xx2,ep,sep
      endif

      if (iwstr.eq.1.and.j.eq.iwssb.and.je.eq.iwshd) write(50,101) ida,ep,rd(j,je),xx,ws(j,je),precip,g(j,je)

  101 format(i5,7f10.3)
  104 format('vegfun ',4i5,20f10.3)
      return
      end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine tstress(tgx,j,je,n)
!**** PURPOSE: COMPUTES TEMPERATURE STRESS FOR CROP/VEGETATION GROWTH
!**** CALLED IN:  GROWTH, VEGMD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!
!      >>>>> COMMON PARAMETERS & VARIABLES
!      avt(j)     = average annual daily temerature., degree C
!      nucr(j,je) = crop number
!      nveg(j,je) = vegetation number
!      tb(iv)     = base temp for plant growth, degree C
!      tmn(j)     = daily min temp, from  readcli, degree C
!      to(iv)     = opt temp for growth, degree C
!      ts         = temp stress factor
!      tx(j)      = daily aver temp, degree C
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      num    = local par
!      rto    = local par
!      rto1   = local par
!      rto2   = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      use mod_snow
      implicit NONE
      integer j,je,n,num
      real(8) tgx,rto,rto1,rto2
      !###########################
      !#### SNOW MODULE       ####
      !###########################
      if ( bSnowModule ) then
        tx_tmp   = tmit
        tmin_tmp = tmin
      else
        tx_tmp   = tx(j)
        tmin_tmp = tmn(j)
      end if
      !###########################
   
      if (n.eq.5) then
         num=nucr(j,je)
      else
         num=nveg(j,je)
      endif

      rto=0.
      rto1=0.
      rto2=0.

!**** CALC temp stress factor ts
      if (tx_tmp.le.to(num)) then
        tgx = (to(num)+tb(num))/(to(num)-tb(num))
        rto1 = tgx*(to(num)-tx_tmp)**2
        rto2 = rto1/(tx_tmp+1.e-6)
        if (rto2.le.200.) then
          ts = exp(-0.1054*rto2)
        else
          ts = 0.
        end if
      else
        tgx = 2.* to(num)-tb(num)-tx_tmp
        rto = ((to(num)-tx_tmp)/(tgx+1.e-6)) ** 2
        if (rto.le.200.) then
          ts = exp(-0.1054*rto)
        else
          ts = 0.
        end if
      endif

      if (tmin_tmp.le.(avt(j)-15.)) ts = 0.

      return
end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine npstress(u1,u2,uu)
!**** PURPOSE: THIS FUNCTION CALCULATES THE NUTRIENT STRESS FACTOR uu
!              CAUSED BY LIMITED SUPPLY of N or P
!**** CALLED IN:  NUPTAKE, PUPTAKE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!
!      u1  = N or P uptake in current day (SUPPLY), kg/ha
!      u2  = optimal uptake of N or P until current day (DEMAND), kg/ha
!      uu  = nutrient stress factor
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      implicit NONE
      real(8) u1,u2,uu

      uu = 200. * (u1/(u2+.0001)-.5)
      if (uu.le.0.) then
        uu = 0.
      else
        uu = uu / (uu+exp(3.535-.02597*uu))
      end if

      return
end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine scurve(x1,x2,x3,x4)
!**** PURPOSE: THIS SUBROUTINE COMPUTES S CURVE PARAMETERS x1 & x2
!              GIVEN 2 (X,Y) POINTS
!**** CALLED IN:  CRPMD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      implicit NONE
      real(8) x1,x2,x3,x4,xx

      xx = dlog(x3/x1-x3)
      x2 = (xx-dlog(x4/x2-x4)) / (x4-x3)
      x1 = xx + x3 * x2
      return
end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine ascrv(x1,x2,x3,x4,x5,x6)
!**** PURPOSE: THIS SUBROUTINE COMPUTES S CURVE PARAMETERS x5 & x6
!              GIVEN 2 (X,Y) POINTS
!**** CALLED IN:   READCRP
!~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      implicit NONE
      real(8) xx, x1, x2, x3, x4, x5, x6

      xx = 0.0
      x5 = 0.0
      x6 = 0.0
      xx = Log(x3/x1 - x3)
      x6 = (xx - Log(x4/x2 - x4)) / (x4 - x3)
      x5 = xx + (x3 * x6)

      return
end




!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


subroutine adjustbe(j,je,beadj)
!**** PURPOSE: this subroutine adjusts biomass-energy ratio for crop
!              for higher atmosphere CO2 concentrations
!              F. Wechsung method (temperature-dependent)
!**** CALLED IN:  CRPMD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!
!      >>>>> COMMON PARAMETERS & VARIABLES
!      alfa       = alpha factor for vegetation
!      be(icr)    = biomass-energy ratio for crop
!      co2ref     = atm CO2 in the reference period, ppm
!      co2sce     = atm CO2 in the scenario period, ppm
!      ic3c4      = switch parameter: 3/4 - C3 or C4 crop?
!      nucr(j,je) = crop number (database)
!      tx(j)      = average daily temp., degree C
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      aa       = coef
!      bb       = coef
!      c1       = co2ref
!      c2       = co2sce
!      cc       = coef
!      ci1      = CO2 internal corr co2ref
!      ci2      = CO2 internal corr co2sce
!      deg      = local par
!      del      = local par
!      delq     = local par
!      xlnalfa  = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      use mod_snow
      implicit NONE
      integer j,je
      real(8) beadj,aa,bb,c1,c2,cc,ci1,ci2,deg,del,delq,xlnalfa
      !###########################
      !#### SNOW MODULE       ####
      !###########################
      if ( bSnowModule ) then
        tx_tmp   = tmit
      else
        tx_tmp   = tx(j)
      end if
      !###########################
!**** CALC xlnalfa = f(tx)
      aa = 0.3898E-2
      bb = 0.3769E-5
      cc = 0.3697E-4
      c1 = co2ref
      c2 = co2sce
      ci1 = c1 * 0.7
      ci2 = c2 * 0.7
      del = ci2 - ci1
      delq = ci2*ci2 - ci1*ci1
      xlnalfa = aa * del - bb * delq + cc * del * tx_tmp

!**** CALC degree - different for C3 and C4 crops
      if (ic3c4.eq.4) then
        deg = 0.36
      else
        deg = 0.6
      endif

!**** CALC alfa and beadj
      alfa = exp(xlnalfa) ** deg
      if (alfa.lt.1) alfa = 1.
      beadj = be(nucr(j,je)) * alfa

      return
end



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
