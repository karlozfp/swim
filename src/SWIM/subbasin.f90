!     FILE subbasin.f
!
!     SUBROUTINES IN THIS FILE          		CALLED FROM
!     subroutine subbasin(icode,ihout,inum1,inum2)	main
!     subroutine hrflowtt(j,je,k)       		subbasin

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

subroutine subbasin(icode,ihout,inum1,inum2)
!**** PURPOSE: THIS SUBROUTINE PERFORMS SUBBASIN OPERATIONS:
!              1. ESTABLISHES INITIAL CONDITIONS IN HYDROTOPES,
!              2. CALLS HYDROTOP TO CALCULATE HYDROTOP PROCESSES, 
!              3. AGGREGATES HYDROTOP OUTPUTS, 
!              4. CALCULATES EROSION IN SUBBASIN,
!              5. CALCULATES RETENTION OF NUTRIENTS,
!              6. CALCULATES VARIABLES FOR OUTPUT, and
!              7. SETS OUTPUTS FROM SUBBASIN FOR ROUTING
!**** CALLED IN:  MAIN 
!     icode = 1, ihout = inum1 = inum2 = j   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!
!      >>>>> INDICES
!      i, ida  = DAY 
!      j       = SUBBASIN NR 
!      jea,je  = HYDROTOPE NR 
!      k       = SOIL NR 
!      n       = LAND USE NR 
!      l       = SOIL LAYER NR
!      icr, iv = CROP or VEGETATION NR
!      ih      = HYDR. STORAGE LOCATON NR
!      io      = CROP OPERATION NR
!      if      = CROP FERTILISATION NR
!      >>>>>
!
!      >>>>> COMMON PARAMETERS & VARIABLES
!      af              = 1000. * da, km2
!      aff             = af * flu(j) = 1000 * da * flu(j), unit transf. coef.
!      alai(j,je)      = leaf area index 
!      alai0(n)        = initial leaf area index
!      ano3(j,je,l)    = wno3(l,k) N-NO3 in sub j, HRU je, layer l, kg/ha
!      anora(j,je,l)   = wmn(l,k) active organic N in sub j, HRU je, layer l, 
!                        kg/ha  
!      anors(j,je,l)   = wn(l,k) stable organic N in sub j, HRU je, layer l, 
!                        kg/ha  
!      ap(l,k)         = labile (soluble) phosphorus, kg/ha (initialisation)
!      bd(k)           = bulk density of the upper (!) soil layer, 
!                        calc from porosity, g/cm3 
!      canev           = canopy evaporation, mm
!      cklsp(j,je)     = combined c,k,ls,p factor
!      cn              = current CN in hydrotope
!      css(j)          = sediment conc in return flow, ppm
!      cv(j)           = initial land cover, kg/ha
!      cva(j,je)       = vegetation cover, kg/ha
!      da              = basin area, km2
!      dart(j)         = drainage area for subbasin, km2
!      degNgrw         = N degradation in groundwater, (1/day) 
!      degNsub         = N degradation in subsurface flow, (1/day)
!      degNsur         = N degradation in surface flow, (1/day)
!      degPsur         = P degradation in surface flow, (1/day)
!      dm(j,je)        = total biomass, kg/ha
!      dur             = flow duration, h
!      eo              = potential evapotranspiration, mm
!      eopot           = potential evapotranspiration, mm
!      ep              = plant transpirationm, mm
!      es              = soil evaporation, mm
!      evasum(j,je)    = SUM(ep + es + canev), mm   
!      fc(l,k)         = field capacity, mm    
!      flu(j)          = fraction of subbasin area in the basin
!      frar(j,je)      = fractional area of HYDROTOPE in SUBBASIN 
!      g(j,je)         = fraction of heat unit accumulated
!      gwchrg          = groundwater recharge, mm
!      gwq(j)          = groundwaterw contribution to stream, mm
!      gwseep          = groundwater seepage, mm
!      hsumfc(j,je)    = sum of field capacity in soil, calc in subbasin, mm
!      hsumul(j,je)    = sum of upper limit water content in soil, 
!                        calc in subbasin, mm 
!      hwss(2,j,je)    = fun(field cap), calc i subbasin from hsumfc(j,je)
!      ida             = current day
!      gis_m      = switch code to write monthly results (water & crops) for GIS output
!      gis_y      = switch code to write annual results (water & crops) for GIS output
!      gis_ave    = switch code to write average annual results (water & crops) for GIS output
!      ih1,ih2,ih3,ih4 = hydrotopes for HYDROTOPE PRINTOUT
!      isb1,isb2,isb3,isb4 = subbasins for HYDROTOPE PRINTOUT
!      isu1,isu2,isu3  = subbasins for SUBBASIN PRINTOUT
!      iv              = index for channel parameters chl(,), chw(,), chk(,)
!      iy              = current year as counter (1,...,nbyr)
!      iyr             = current year
!      mstruc(j,je,2)  = HRU structure vector to define k,n
!      ndpri           = day to write crop yield for GIS output
!      neap(j)         = number of HYDROTOPEs in subbasin, calc readbas
!      nn              = number of soil layers
!      ns(k)           = number of soil layers
!      nsa(k)          = number of layers for arable soils
!      nucr(j,je)      = crop number (database)
!      op(l,k)         = stable min. P, estimated from pmn(), kg/ha
!      parsz(5,j)      = particle size distribution, calc in subbasin
!      percn           = N leaching to g-w, kg/h 
!      plab(j,je,l)    = ap(l,k) labile (soluable) P in sub j, HRU je, 
!                        layer l, kg/ha
!      pma(j,je,l)     = pmn(l,k) active mineral P in sub j, HRU je, 
!                        layer l, kg/ha
!      pmn(l,k)        = act. min. P, estimated in readsol, kg/ha 
!      pms(j,je,l)     = op(l,k) stable mineral P in sub j, HRU je, 
!                        layer l, kg/ha
!      porg(j,je,l)    = wpo(l,k) organic P in sub j, HRU je, layer l, kg/ha
!      pr              = peak runoff rate, m3/sec.
!      gwrsum(j,je)    = annual sum of percolation for GIS output, SUM(sep),mm
!      precip          = precipitation in subbasin, mm
!      presum(j,je)    = annual sum of precip for GIS output, mm
!      psz(5,k)        = particle size distribution
!      q1              = daily surface runoff = xqd before call tran, mm 
!      qd              = daily surface runoff in hydrotope, mm
!      qi              = daily surface runoff in hydrotope, mm   
!      qtl             = transmission losses in subbasin, mm
!      ra(j)           = radiation in J/cm^2
!      rd(j,je)        = root depth, mm
!      retNgrw         = N retention in groundwater, days
!      retNsub         = N retention in subsurface flow, days
!      retNsur         = N retention in surface flow, days
!      retPsur         = P retention in surface flow, days
!      revap           = revaporation from groundwater, mm
!      revapst(j)      = transmission losses in the reach, calc in route, m3
!      rsd(j,je,2)     = crop residue, kg/ha
!      runsum(j,je)    = annual sum of surface+subsurface runoff for GIS output,
!                        SUM(qd+ssf), mm
!      sbp(j)          = precipitation in subbasin, mm
!      sda(1:10,j)     = subbasin outputs for routing, analogue to varoute()
!                        varoute(1:10,ihout) = sda(1:10,j)
!      sep             = percolation to g-w, mm
!      sml             = snow melt, calc in snom, mm
!      smq(j)          = SUM(xqd) - surface runoff for subbasin, mm
!      smsq(j)         = SUM(xssf) - subsurface runoff for subbasin, mm
!      sno(j)          = accumulated value of snow cover, mm
!      snoa(j,je)      = snow water content in sub j, HRU je, mm
!      snoev           = snow evaporation, mm
!      snow            = precipitation as snow, mm
!      ssf             = subsurface flow in HYDROTOPE, mm
!      ssfn            = N loss with subsurface flow, kg/ha
!      ste(j,je,l)     = water content in sub j, HRU je, layer l, mm
!      stin(l,k)       = soil water content in soil layer, mm
!      sub(1:28)       = basin outputs (weighted sums) daily, see desc. below
!      sub(1)  = SUM(precip)            precipitation, mm 
!      sub(2)  = SUM(precip), if tx()<0 snowfall, mm                   
!      sub(3)  = SUM(sml)               snowmelt(mm)
!      sub(4)  = SUM((tmx*flu)          average "weighted" max temp., degree C
!      sub(5)  = SUM(tmn*flu)           average "weighted" min temp., degree C
!      sub(6)  = SUM(ra*flu)            radiation, J/cm^2
!      sub(7)  = SUM(sumcn*flu)         curve number 
!      sub(8)  = SUM(xqi*flu)           surface runoff, mm         
!      sub(9)  = SUM(xssf*flu)          subsurface runoff, mm      
!      sub(10) = SUM(qtl*flu)           channel transm. losses, mm 
!      sub(11) = SUM(xsep*flu)          percolation, mm,            
!      sub(12) = SUM(xeo*flu)           pot evapotr., mm	    
!      sub(13) = SUM(xet*flu)           evapotranspiration, mm      
!      sub(14) = SUM(snoev*flu)         snow evaporation, mm
!      sub(15) = SUM(gwq*flu)           gr. water flow, mm	
!      sub(16) = SUM(revap*flu)         revap from g-w to soil prof., mm
!      sub(17) = SUM(gwseep*flu)        g-w percolation, mm
!      sub(18) = SUM(gwchrg*flu)        g-w recharge, mm	
!      sub(19) = SUM(xswimd*flu)        soil water index for basin	            
!      sub(20) = SUM(wysb*flu)          wysb=qi+ssf+gwq(j)-qtl, wat yld, mm   
!      sub(21) = SUM(yd/(100*da*flu)    sed yield, t/ha
!      sub(22) = SUM(yon*flu)           org. N loss with sed, kg/ha	
!      sub(23) = SUM(yph*flu)           org. P loss with sed, kg/ha
!      sub(24) = SUM(ysp*flu)           soluble P, kg/ha
!      sub(25) = SUM(xyno3*flu)         NO3-N in surface runoff, kg/ha
!      sub(26) = SUM(xssfn*flu)         NO3-N in subsurface runoff, kg/ha
!      sub(27) = SUM(xpercn*flu)        NO3-N leached to g-w, kg/ha
!      sub(28) = SUM(xuno3*flu)         NO3-N uptake by plants , kg/ha         
!      subp(j)         = daily precipitation in subbasin, mm (from readcli)
!      sumcn           = weighted aver. Cur.Num. in subbasin (SUM(cn))
!      susb(30,j)      = subbasin outputs daily, analogue to sub() above
!      swe(j,je)       = water content in sub j, HRU je
!      swin(k)         = soil water content in soil profile, mm
!      swind           = soil water index for hydrotope: 
!                        swind=swe(j,je)/sumfc(k), 1/1
!      sym(j)          = SUM(yd) = sum of sediment yield for subbasins, t
!      tmn(j)          = daily min temp in the subbasin, degree C
!      tmx(j)          = daily max temp in the subbasin, degree C
!      tsnfall         = threshold temperature for snow fall
!      tx(j)           = daily aver temp in the subbasin, degree C
!      ul(l,k)         = upper limit water content in layer, mm 
!      varoute(*,ih)   = subbasin outputs for routing:
!      Name             Units          Definition
!      varoute(2,ih)    |(m^3)         |surface flow  
!      varoute(3,ih)    |(tons)        |sediment
!      varoute(4,ih)    |(kg)          |organic N  
!      varoute(5,ih)    |(kg)          |organic P  
!      varoute(6,ih)    |(kg)          |nitrate N  
!      varoute(7,ih)    |(kg)          |soluble P
!      varoute(8,ih)    |(m^3)         |subsurface + g-w flow
!      vo              = surface runoff, m3
!      wmn(l,k)        = active org. N content, kg/ha
!      wn(l,k)         = stable organic N content, kg/ha 
!      wno3(l,k)       = NO3-N content, kg/ha
!      wpo(l,k)        = organic P content, kg/ha
!      wysb            = water yield in subbasin = xqi+xssf+gwq(j)-qtl, mm
!      xcklsp          = weighted aver. comb C*K*LS*P in subbasin, SUM(cklsp)
!      xeo             = weighted aver. pot. evapotr. in subbasin, SUM(eo), mm
!      xet             = weighted aver. act. evapotr. in subbasin, 
!                        SUM(ep+es), mm
!      xnorg           = weighted aver. st.org.N in lay 1 in subb., 
!                        SUM(anors), g/t
!      xnorgp          = weighted aver. st.org.N in lay 1 in subb., 
!                        SUM(anors), kg/ha 
!      xpercn          = weighted aver. N leaching to g-w in subb., kg/ha
!      xporg           = weighted aver. org.P in layer 1 in subb., 
!                        SUM(porg), g/t
!      xporgp          = weighted aver. org.P in layer 1 in subb., 
!                        SUM(porg), kg/ha
!      xpsed           = weighted aver. total P (porg+pms+pma) in subb., g/t
!      xpsedp          = weighted aver. total P (porg+pms+pma) in subb., kg/ha
!      xqd             = weighted aver. surface flow in subbasin, SUM(qd), mm
!      xqi             = weighted aver. surface flow in subbasin, SUM(qd), mm
!      xsep            = weighted aver. percolation in subbasin, SUM(sep), mm
!      xsnoev          = weighted aver. snow evap in subbasin, SUM(snoev), mm
!      xssf            = weighted aver. subsurface flow in subbasin, 
!                        SUM(ssf), mm
!      xssfn           = weighted aver. N loss with ssf in subbasin, 
!                        SUM(ssfn), mm
!      xswind          = weighted aver. soil wat. index in subbasin, SUM(swimd)
!      xwysb           = water yield for subbasin, m3
!      xxswind         = weighted aver. soil wat. index in the basin
!      xyno3           = weighted aver. N loss with qd in subbasin, 
!                        SUM(yno3), kg/ha
!      xysp            = weighted aver. soluble P leaching in subbasin, kg/ha
!      yd              = daily soil loss caused by water erosion, t
!      yno3            = N loss with surface flow, kg/ha
!      yon             = org N loss with erosion, kg/ha
!      yph             = org P loss with erosion, kg/ha
!      ysp             = soluble P leaching, kg/ha
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      ii      = local par   
!      im      = local par 
!      j       = subbasin nr 
!      jea     = hydrotope nr 
!      jj      = local par 
!      k       = soil nr 
!      l       = soil layer nr 
!      n       = land use nr 
!      wabad   = daily water balance
!      >>>>>
!
!      >>>>> UNITS TRANSFORMATION:
!      g/t   -->  kg/ha	*wt1=bden*dg/100., bden=bulk dens., dg=layer thickness
!      %     -->  g/t	*10000.
!      %     -->  kg/ha	*wt1*10000.
!      kg/ha -->  g/t	1/wt1 = 100./dg/bden()
!      mg/kg  =   g/t
!      mm    -->  m3    *(dart()*1000)
!      kg/ha -->  kg    *(dart()*100)
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
use mod_snow      !#### SNOW MODULE ####
implicit NONE
   integer, intent(in) :: icode,ihout,inum1,inum2
   integer             :: ii,im,j,jea,jj,k,l,n,wet
   real(8)                :: wabad
   real(8)                :: pcp_tmp  = 0. ! temporary precipitation values for output
   real(8)                :: snow_tmp = 0. ! temporary snow values for output
   real(8)                :: pcp_tot  = 0. ! total preciptiation (rain+snow) for GIS output
     
!#### INITIALIZATION: call initsub
   call initsub
   j = inum1
   aff = af * flu(j)

!**** COMPUTE TOTALS for precip:
   precip = subp(j)

   !###########################
   !#### SNOW MODULE       ####
   !###########################
   if ( bSnowModule ) then
      call snow_initsub
   else
     susb(1,j) = susb(1,j) + precip
     sub(1) = sub(1) + precip * flu(j)
     sbp(j) = sbp(j) + precip
   
     !#### COMPUTE snow, recalc precip
      !     ATTN: Snow accumulation, melt, and snow evaporation
      !        are calculated at hydrotope level!
      snow = 0.
      if (tx(j).le.tsnfall(j)) then
        snow = precip
        sub(2) = sub(2) + precip * flu(j)
        susb(2,j) = susb(2,j) + precip
        precip = 0.
      end if
   end if !( bSnowModule )
   !###########################

      
!*********************************************************** START OF CYCLE 100  
!     MAIN CYCLE IN SUBBASIN: 100 - HYDROTOPE caclculation: 
!     		1) initial conditions, 
!		2) call HYDROTOP, 
!		3) compute weighted averages for subbasin. 
   CYCLE_100: do jea=1,neap(j)
      n = mstruc(j,jea,1) ! land use type
      k = mstruc(j,jea,2) ! soil number
      wet=mstruc(j,jea,3) ! wetland 0/1
      
      !###########################
      !#### SNOW MODULE       ####
      !###########################
      !#### CALL SNOM: COMPUTE snow & snowmelt, recalc precip
      !     ATTN: in snom() precip and sno() are recalculated
      if ( bSnowModule ) then
         call snow_calc_snowprocesses(j,jea)
      else
         if ( snow > 0. ) snoa(j,jea) = snoa(j,jea) + snow
         if ( tmx(j) > tmelt(j) .AND. snoa(j,jea) > 1E-4 ) call snom(j,jea)
      end if
      !###########################


   !**** ESTABLISH number of soil layers: different for arable and non-arable soils
      nn = ns(k)
      if (n.eq.5) nn = nsa(k)

   !**** CALC hsumul(j,jea), hsumfc(j,jea), hwss(2,j,jea) and hrtc(j,jea,l)
      call assign_hsum

   !#### CALL HRFLOWTT
      call hrflowtt(j,jea,k)   
   
   !**** INITIAL CONDITIONS for water & nutrients
      if ( iy == 1.and.ida == 1 ) call init_water_nutrients(n,nn)


   !**** INITIALIZATION of forest - in the beginning of every year
   !     ATTN! Forest is init. every year, as no forest management is included
      if ( ida.eq.1 ) then  
         if ( n.eq.9.or.n.eq.10.or.n.eq.11.or.n.eq.13 ) then
            dm(j,jea) = 200000.
            rd(j,jea) = 2000.
         end if
      end if   

   !#### CALL HYDROTOPE - calculate processes in HYDROTOPE       
      call hydrotop(j,jea,k,n,wet)

   !     Subroutine wstress is called after ep was calculated in evap.f90
   !     This usually decreases ep but et is not updated thereafter.
   !     Important for hydrotope output!
      et = ep + es


   !**** COMPUTE WEIGHTED AVERAGES
   !     ATTN: xnorgp, xporgp - in kg/ha 
   !     ATTN: xnorg, xporg - coef. (10./bd(k)) to tranf. kg/ha to g/t (I layer!)
   !     ATTN: xet - not from et, et is not recalc after cropmd
      sumcn = sumcn + cn * frar(j,jea)

      xqd = xqd + qd * frar(j,jea)
      xqi = xqi + qi * frar(j,jea)
      xssf = xssf + ssf * frar(j,jea)   
      xsep = xsep + sep * frar(j,jea)
      xswind = xswind + swind * frar(j,jea)

      xeo = xeo + eopot * frar(j,jea)
      xet = xet + (ep + es + canev) * frar(j,jea) 

      xsnoev = xsnoev + snoev * frar(j,jea)
      sno(j) = sno(j) + snoa(j,jea) * frar(j,jea)
      
      ! summarise total area covered by snow
      if ( snoa(j,jea) > 1E-4 ) then
         area_tot_snow  = area_tot_snow  + frar(j,jea)*sbar(j)
         depth_ave_snow = depth_ave_snow + snoa(j,jea)*frar(j,jea)*sbar(j)/(da*10**6)
         if ( ida == nd ) snow_acc_mm  = snow_acc_mm + snoa(j,jea)*frar(j,jea)*sbar(j)/(da*10**6) - snow_acc_mm0
      end if
      if ( ida == nd ) soil_acc_mm  = soil_acc_mm + swe(j,jea) *frar(j,jea)*sbar(j)/(da*10**6)

      !###########################
      !#### SNOW MODULE       ####
      !###########################
      if ( bSnowModule ) then
        if ( gla(j,jea) > 1E-4 ) then
           area_tot_glacier  = area_tot_glacier        +              frar(j,jea)*sbar(j)
           depth_ave_glacier = depth_ave_glacier       + gla(j,jea) * frar(j,jea)*sbar(j)/(da*10**6)
           if ( ida == nd ) glac_acc_mm  = glac_acc_mm + gla(j,jea) * frar(j,jea)*sbar(j)/(da*10**6) - glac_acc_mm0
        end if
         xtmit   = xtmit   + tmit    * frar(j,jea)
         xtmax   = xtmax   + tmax    * frar(j,jea)
         xtmin   = xtmin   + tmin    * frar(j,jea)
         xprecip = xprecip + precipe * frar(j,jea) ! weighted subbasin rainfall or rainfall + snow melt (not total precipitation), mm
         xprecip_elev_cor = xprecip_elev_cor + precip_elev_cor * frar(j,jea) ! weighted subbasin elevation-based corrected precipitation, mm
         xsml    = xsml    + smle    * frar(j,jea)
         xsnow   = xsnow   + snow    * frar(j,jea) ! weighted subbasin snow from snowfall, mm
         xvsn    = xvsn    + vsn     * frar(j,jea) ! weighted subbasin from snow pack, mm
      end if
      !###########################

      xyno3 = xyno3 + yno3 * frar(j,jea)
      xssfn = xssfn + ssfn * frar(j,jea)       
      xpercn = xpercn + percn * frar(j,jea)
      xysp  = xysp + ysp * frar(j,jea)
      xcklsp = xcklsp + cklsp(j,jea) * frar(j,jea)
      xnorg = xnorg + anors(j,jea,1) * (10. / bd(k)) * frar(j,jea)
      xnorgp = xnorgp + anors(j,jea,1) * frar(j,jea)
      xporg = xporg + porg(j,jea,1) * (10. / bd(k)) * frar(j,jea)
      xporgp = xporgp + porg(j,jea,1) * frar(j,jea)
      xpsed = xpsed + (porg(j,jea,1) + pms(j,jea,1) + pma(j,jea,1)) * (10. / bd(k)) * frar(j,jea)
      xpsedp = xpsedp + (porg(j,jea,1) + pms(j,jea,1) + pma(j,jea,1)) * frar(j,jea)

      !###########################
      !#### SNOW MODULE       ####
      !###########################
      if ( bSnowModule ) then
         pcp_tmp = precip_elev_cor
         pcp_tot = precipe + snow
      else
         pcp_tmp = precip
         pcp_tot = subp(j)
      end if
      !###########################
      
      !**** WRITE: hydrotop daily water outputs in 4 selected hydrotopes: 
   !     FILES 51-57
      if ( j.eq.isb1.and.jea.eq.ih1 ) then
      if ( ida.eq.1.and.iy.eq.1 ) write(51,131)
      write(51,130) iyr,ida,j,jea,pcp_tmp,qd,ssf,sep,eopot,ep,es,et, &
         swind,alai(j,jea),dm(j,jea)/1000.,rsd(j,jea,1),rsd(j,jea,2),cva(j,jea),rd(j,jea),ws(j,jea),g(j,jea), &
         swe(j,jea),ffc(j)
      call htpmon_add(1,pcp_tmp,qd,ssf,sep,eopot,ep,es,ep+es,alai(j,jea))
      end if
      if ( j.eq.isb2.and.jea.eq.ih2 ) then
      if( ida.eq.1.and.iy.eq.1 ) write(52,131)
      write(52,130) iyr,ida,j,jea,pcp_tmp,qd,ssf,sep,eopot,ep,es,et, &
         swind,alai(j,jea),dm(j,jea)/1000.,rsd(j,jea,1),rsd(j,jea,2),cva(j,jea),rd(j,jea),ws(j,jea),g(j,jea), &
         swe(j,jea),ffc(j)
      call htpmon_add(2,pcp_tmp,qd,ssf,sep,eopot,ep,es,ep+es,alai(j,jea))
      end if
      if ( j.eq.isb3.and.jea.eq.ih3 ) then
      if(ida.eq.1.and.iy.eq.1) write(53,131)
      write(53,130) iyr,ida,j,jea,pcp_tmp,qd,ssf,sep,eopot,ep,es,et, &
         swind,alai(j,jea),dm(j,jea)/1000.,rsd(j,jea,1),rsd(j,jea,2),cva(j,jea),rd(j,jea),ws(j,jea),g(j,jea), &
         swe(j,jea),ffc(j)
      call htpmon_add(3,pcp_tmp,qd,ssf,sep,eopot,ep,es,ep+es,alai(j,jea))
      end if
      if ( j.eq.isb4.and.jea.eq.ih4 ) then
      if ( ida.eq.1.and.iy.eq.1 ) write(54,131)
      write(54,130) iyr,ida,j,jea,pcp_tmp,qd,ssf,sep,eopot,ep,es,et, &
         swind,alai(j,jea),dm(j,jea)/1000.,rsd(j,jea,1),rsd(j,jea,2),cva(j,jea),rd(j,jea),ws(j,jea),g(j,jea), &
         swe(j,jea),ffc(j)
      call htpmon_add(4,pcp_tmp,qd,ssf,sep,eopot,ep,es,ep+es,alai(j,jea))
      end if
      if ( j.eq.isb5.and.jea.eq.ih5 ) then
      if ( ida.eq.1.and.iy.eq.1 ) write(55,131)
      write(55,130) iyr,ida,j,jea,pcp_tmp,qd,ssf,sep,eopot,ep,es,et, &
         swind,alai(j,jea),dm(j,jea)/1000.,rsd(j,jea,1),rsd(j,jea,2),cva(j,jea),rd(j,jea),ws(j,jea),g(j,jea), &
         swe(j,jea),ffc(j)
      call htpmon_add(5,pcp_tmp,qd,ssf,sep,eopot,ep,es,ep+es,alai(j,jea))
      end if
      if ( j.eq.isb6.and.jea.eq.ih6 ) then
      if ( ida.eq.1.and.iy.eq.1 ) write(56,131)
      write(56,130) iyr,ida,j,jea,pcp_tmp,qd,ssf,sep,eopot,ep,es,et, &
         swind,alai(j,jea),dm(j,jea)/1000.,rsd(j,jea,1),rsd(j,jea,2),cva(j,jea),rd(j,jea),ws(j,jea),g(j,jea), &
         swe(j,jea),ffc(j)
      call htpmon_add(6,pcp_tmp,qd,ssf,sep,eopot,ep,es,ep+es,alai(j,jea))
      end if
      if ( j.eq.isb7.and.jea.eq.ih7 ) then
      if ( ida.eq.1.and.iy.eq.1 ) write(57,131)
      write(57,130) iyr,ida,j,jea,pcp_tmp,qd,ssf,sep,eopot,ep,es,et, &
         swind,alai(j,jea),dm(j,jea)/1000.,rsd(j,jea,1),rsd(j,jea,2),cva(j,jea),rd(j,jea),ws(j,jea),g(j,jea), &
         swe(j,jea),ffc(j)
      call htpmon_add(7,pcp_tmp,qd,ssf,sep,eopot,ep,es,ep+es,alai(j,jea))
      end if


      if ( gis_y > 0 .OR. gis_ave > 0 ) then ! annual output
      !**** CALC variables for GIS output
         presum(j,jea) = presum(j,jea) + pcp_tot ! precip
         runsum(j,jea) = runsum(j,jea) + qd + ssf
         gwrsum(j,jea) = gwrsum(j,jea) + sep   
         evasum(j,jea) = evasum(j,jea) + ep + es + canev   
         petsum(j,jea) = petsum(j,jea) + eo
      end if

      if ( gis_m > 0 ) then ! monthly output
         presummon(j,jea,mo) = presummon(j,jea,mo) + pcp_tot
         runsummon(j,jea,mo) = runsummon(j,jea,mo) + qd + ssf
         evasummon(j,jea,mo) = evasummon(j,jea,mo) + ep + es + canev
         petsummon(j,jea,mo) = petsummon(j,jea,mo) + eo
         gwssummon(j,jea,mo) = gwssummon(j,jea,mo) + sep
         ! divide by average number of days in month to get average soil water index instead of sum
         swisummon(j,jea,mo) = swisummon(j,jea,mo) + swind/30.4
         if (pcp_tot >= 1.)  npredays01(j,jea,mo) = npredays01(j,jea,mo) + 1
         if (pcp_tot >= 5.)  npredays05(j,jea,mo) = npredays05(j,jea,mo) + 1
         if (pcp_tot >= 10.) npredays10(j,jea,mo) = npredays10(j,jea,mo) + 1
         if (pcp_tot >= 20.) npredays20(j,jea,mo) = npredays20(j,jea,mo) + 1
      end if

      !#### CALL CROP_GIS
      if ( gis_y > 0 .AND. iy > 1 .AND. ida == ndpri ) call crop_gis(j,jea,n,k)
         
      !#### CALL HYDRO_GIS 
      if ( gis_y > 0 .and.ida.eq.365 ) then ! annual output
         call hydro_gis(j,jea)
            evamean(j,jea) = evamean(j,jea) + evasum(j,jea)
            pcpmean(j,jea) = pcpmean(j,jea) + presum(j,jea)
            petmean(j,jea) = petmean(j,jea) + petsum(j,jea)
            gwrmean(j,jea) = gwrmean(j,jea) + gwrsum(j,jea)
            if ( iy.eq.nbyr ) call gis_mean(j,jea,nbyr)
      end if

      !###########################
      !#### SNOW MODULE       ####
      !###########################
      if ( bSnowModule ) then
         !#### CALL GLACIER_GIS
         if( gis_y > 0 .AND. ida == gla_day_out) call gla_gis(j,jea)
      end if
      !###########################
      
   !**** CALC parsz() for subbasins
      do jj=1,5
         parsz(jj,j) = parsz(jj,j) + psz(jj,k) * frar(j,jea)
      end do
   
   end do CYCLE_100
!*********************************************************** END OF CYCLE 100  

   pet_day(j) = xeo ! for transmission losses

   !###########################
   !#### SNOW MODULE       ####
   !###########################
   if ( bSnowModule ) then
      !#### recalculate precipitation and air temperature for the whole basin 
      precip = xprecip
      tx(j) = xtmit
      tmx(j)= xtmax
      tmn(j)= xtmin
      sml = xsml
      psnow = xsnow
      
      xprecip = 0.
      xtmit   = 0.
      xtmax   = 0.
      xtmin   = 0.
      xsml    = 0.
      xsnow   = 0.
   end if
   !###########################

   !#### CALL alpha to calc r1 - alpha to be used in peakq
   if ( precip > 0. ) call alpha(j)


   !**** COMPUTE PEAK RUNOFF RATE, TRANSMISSION LOSSES & EROSION for subbasin
   !#### CALL PEAKQ, TRAN, YSED
   !     xssf (mm), css (ppm) aff=1000*da*flu(j), yd (t)
   if ( xqd.ne.0. ) then
      call peakq(j)
      if ( xqd.gt.0..and.pr.gt.0. ) then
         iv = 1
         vo = xqd * da * flu(j) * 1000.
         dur = vo / (pr*3600.)
         if ( dur.gt.24. ) dur = 24.
! SL BEGIN: transmission losses are calculated in the routing process using subroutine tran_river
! SL  only recalculate peak runoff rate (pr) in function tran(j)         
         q1 = xqd ! SL
         call tran(j) 
         !qtl = q1 - xqd
         xqd = q1
!          if ( qtl.lt.0. ) then
!             xqd = q1
!             qtl = 0.
!          end if
!          revapst(j) = revapst(j) + qtl
! SL END
      end if
      call ysed(j)
   end if
   yd = yd + xssf * aff * css(j)

   !**** COMPUTE ENRICHMENT RATIO, ORG N and P LOSS WITH EROSION
   !#### CALL ENRSB, ORGNSED, PSED
   if (qd > 0. .AND. pr > 0. .AND. precip > 0.) then
      call enrsb(j)
      call orgnsed(j)
      call psed(j)
   end if

!**** COMPUTE GROUND WATER CONTRIBUTION TO FLOW & WATER YIELD
!#### CALL GWMOD
   ! NOTE that revap from shallow aquifer is add to xet
!   call gwmod(j)
   call gwmod(j,xsep,xet,xeo)

   wysb = xqi + xssf + gwq(j) ! - qtl ! water yield, transmission losses are calculated in the routing process

      
!######## CaMa-Flood #######
         if ( bCamaFlood ) then
            runoff_mm(j) = wysb
         end if
!######## CaMa-Flood #######


!**** COMPUTE RETENTION of NUTRIENTS in SUBBASIN
!     Retention of N (xyno3, xssfn, xpercn) and soluble P (xysp)
!     Method: from Fred Hattermann 2003
!     Updated 11.03.2008 from Shaochun's file 
   xyno3  = (1.-exp(-1./retNsur - degNsur)) * xyno3 / (1.+ retNsur * degNsur) + tmpNsur(j) * exp(-1./retNsur - degNsur)
   xssfn  = (1.-exp(-1./retNsub - degNsub)) * xssfn / (1.+ retNsub * degNsub) + tmpNsub(j) * exp(-1. /retNsub - degNsub)
   xpercn = (1.-exp(-1./retNgrw - degNgrw)) * xpercn / (1. + retNgrw * degNgrw) + tmpNgrw(j) * exp(-1. / retNgrw - degNgrw)
   xysp   = (1.-exp(-1./retPsur - degPsur)) * xysp / (1. + retPsur * degPsur) + tmpPsur(j) * exp(-1. /retPsur - degPsur)
   tmpNsur(j) = xyno3      
   tmpNsub(j) = xssfn
   tmpNgrw(j) = xpercn
   tmpPsur(j) = xysp

!**** COMPUTE TOTALS for water: sub(1...13)
   !###########################
   !#### SNOW MODULE       ####
   !###########################
   if ( bSnowModule ) then
      susb(1,j) = susb(1,j) + xprecip_elev_cor ! precip
      sub(1) = sub(1) + xprecip_elev_cor * flu(j) ! precip * flu(j)
      sbp(j) = sbp(j) + xprecip_elev_cor ! precip !precipe
      sub(2) = sub(2) + psnow * flu(j)
      susb(2,j) = susb(2,j) + psnow      
   end if
   !###########################
   sub(3) = sub(3) + sml * flu(j)
   susb(3,j) = susb(3,j) + sml
   sub(4) = sub(4) + tmx(j) * flu(j)
   susb(4,j) = susb(4,j) + tmx(j)
   sub(5) = sub(5) + tmn(j) * flu(j)
   susb(5,j) = susb(5,j) + tmn(j)
   sub(29) = sub(29) + tx(j) * flu(j)!  / get_days_in_month(mo,iyr)
   susb(29,j) = susb(29,j) + tx(j)! / get_days_in_month(mo,iyr)
   sub(6) = sub(6) + ra(j) * flu(j)
   susb(6,j) = susb(6,j) + ra(j)
   sub(7) = sub(7) + sumcn * flu(j)
   susb(8,j) = susb(8,j) + xqd
   sub(8) = sub(8) + xqd * flu(j)
   smq(j) = smq(j) + xqd
   susb(9,j) = susb(9,j) + xssf
   sub(9) = sub(9) + xssf * flu(j)
   smsq(j) = smsq(j) + xssf
! SL Transmission losses are calculated during the routing process...   
!    susb(10,j) = susb(10,j) + qtl
!    sub(10) = sub(10) + qtl * flu(j)
!    revapst(j) = revapst(j) + qtl
   susb(11,j) = susb(11,j) + xsep
   sub(11) = sub(11) + xsep * flu(j)
   susb(12,j) = susb(12,j) + xeo
   sub(12) = sub(12) + xeo * flu(j)
   susb(13,j) = susb(13,j) + xet
   sub(13) = sub(13) + xet * flu(j)
   
!**** COMPUTE TOTALS for ground water: sub(15...19) 
   susb(15,j) = susb(15,j) + gwq(j)
   sub(15) = sub(15) + gwq(j) * flu(j)
   susb(16,j) = susb(16,j) + revap
   sub(16) = sub(16) + revap * flu(j)
   susb(17,j) = susb(17,j) + gwseep
   sub(17) = sub(17) + gwseep * flu(j)
   susb(18,j) = susb(18,j) + gwchrg
   sub(18) = sub(18) + gwchrg * flu(j)
   susb(19,j) = susb(19,j) + xswind
   sub(19) = sub(19) + xswind * flu(j)
   xxswind = xxswind + xswind * flu(j)

!**** Calc daily water balance 
   wabad = sub(1)-sub(8)-sub(9)-sub(11)-sub(13)
   
!**** COMPUTE TOTALS for water yield (wysb) & sediments: sub(20...25) 
   susb(20,j) = susb(20,j) + wysb
   sub(20) = sub(20) + wysb * flu(j)
   sub(21) = sub(21) + yd/(100.*da*flu(j))
   sym(j) = sym(j) + yd
   susb(21,j) = susb(21,j) + yd/(100.*da*flu(j))
   sub(22) = sub(22) + yon * flu(j)
   susb(22,j) = susb(22,j) + yon
   sub(23) = sub(23) + yph * flu(j)
   susb(23,j) = susb(23,j) + yph
   sub(24) = sub(24) + ysp * flu(j)
   susb(24,j) = susb(24,j) + ysp    
   
!**** COMPUTE SUBBASIN OUTPUTS FOR ROUTING: sda(), varoute()
!     ATTN: sda(6,j) = sum of 3 fluxes after retention, new version
!     ATTN: coef (dart()*1000) to transform from mm to m3 (42,48)
!     ATTN: coef (dart()*100)  to transform kg/ha to kg (44-47)
!     ATTN: wysb(mm)*dart(km2)*1000 = wysb*dart*1000 (m3)
!     ATTN: sda(2,j), sda(8,j) in m3
!     ATTN: xyno3 in kg/ha, sda(6) & varoute(6,.) in kg
   sda(1,j) = precip
   sda(2,j) = xqi * dart(ihout) * 1000.
   sda(3,j) = yd
   sda(4,j) = yon * dart(ihout) * 100
   sda(5,j) = yph * dart(ihout) * 100
   sda(6,j) = (xyno3 + xssfn + xpercn) * dart(ihout) * 100
   sda(7,j) = xysp * dart(ihout) * 100
   sda(8,j) = (wysb - xqi) * dart(ihout) * 1000. 
   xwysb = xwysb + wysb * dart(ihout) * 1000.

!cc riparian zone take water form GW flow as demanded
   gwqLastday(j)=sda(8,j)*0.9
   sda(8,j)=sda(8,j)-additionalGwUptake(j)

   do ii = 1, 8
     varoute(ii,ihout) = sda(ii,j)
   end do

   !###########################
   !#### SNOW MODULE       ####
   !###########################
   if ( bSnowModule ) then
      pcp_tmp  = xprecip_elev_cor
      snow_tmp = psnow
   else
      pcp_tmp  = precip
      snow_tmp = snow
   end if
   !###########################

      
   !######## CaMa-Flood #######
   if ( bCamaFlood ) then
      if ( j == mb ) then
         write(1001,150) iyr,ida,runoff_mm
      end if
   end if
   !######## CaMa-Flood #######


!**** WRITE daily water outputs for ALL subbasins: FILE 61 - sbd.out
   if ( bAllSubbasinsDaily ) then 
      if (ida.eq.1.and.j.eq.1.and.iy.eq.1) write(61,128)
      if (iy.eq.1) write(61,115) iyr-1900,ida,j,pcp_tmp,tx(j),xqd,xssf,xsep,xeo,xet,wysb
   end if

!**** WRITE daily water outputs for selected subbasins: FILES 63,64,65,66,67:
   if ( j.eq.isu1 ) then
      if ( ida == 1 .AND. iy==1 ) write(63,129)
      write(63,121) iyr,ida,j,pcp_tmp,snow_tmp,tx(j),xqd,xssf,xsep,gwq(j),gwseep,xeo,xet,wysb,xswind
   end if
   if ( j.eq.isu2 ) then
      if ( ida == 1 .AND. iy==1 ) write(64,129)
      write(64,121) iyr,ida,j,pcp_tmp,snow_tmp,tx(j),xqd,xssf,xsep,gwq(j),gwseep,xeo,xet,wysb,xswind
   end if
   if ( j.eq.isu3 ) then
      if ( ida == 1 .AND. iy==1 ) write(65,129)
      write(65,121) iyr,ida,j,pcp_tmp,snow_tmp,tx(j),xqd,xssf,xsep,gwq(j),gwseep,xeo,xet,wysb,xswind
   end if
   if ( j.eq.isu4 ) then
      if ( ida == 1 .AND. iy==1 ) write(66,129)
      write(66,121) iyr,ida,j,pcp_tmp,snow_tmp,tx(j),xqd,xssf,xsep,gwq(j),gwseep,xeo,xet,wysb,xswind
   end if
   if ( j.eq.isu5 ) then
      if ( ida == 1 .AND. iy==1 ) write(67,129)
      write(67,121) iyr,ida,j,pcp_tmp,snow_tmp,tx(j),xqd,xssf,xsep,gwq(j),gwseep,xeo,xet,wysb,xswind
   end if

  115 format(3i4,12f8.2)
  121 format(i5,i4,i6,20f8.3)
  128 format(/,'  YR DAY SUB  PRECIP    TEMP    SURQ    SUBQ    PERC',5X,'PET     AET    WYLD')
  129 format('YEAR  DAY   SUB   PRECIP  SNOW   Tmean    SURQ    SUBQ    PERC     GWQ   GWSEEP     PET     AET    WYLD   SWIND')
  130 format(i4,3i6,20f10.2)
  131 format('YR     DAY   SUB   HRU      PCP       SURQ      SUBQ      PERC      PET      PLANT_ET SOIL_ET     et       SWIND   '&
            &"   ALAI      dm        rsd1      rsd2      cva       rd        ws        g       swe         ffc")
  150 format(i5,i4,2000f10.2)
      return
   
      

!******************************************************************************

CONTAINS

!******************************************************************************
   subroutine htpmon_add(hru,precip,qd,ssf,sep,eo,ep,es,epes,alai)
      integer, intent(in) :: hru
      real(8), intent(in)    :: precip,qd,ssf,sep,eo,ep,es,epes,alai

      htpmon(hru,1) = htpmon(hru,1) + precip
      htpmon(hru,2) = htpmon(hru,2) + qd
      htpmon(hru,3) = htpmon(hru,3) + ssf
      htpmon(hru,4) = htpmon(hru,4) + sep
      htpmon(hru,5) = htpmon(hru,5) + eo
      htpmon(hru,6) = htpmon(hru,6) + ep
      htpmon(hru,7) = htpmon(hru,7) + es
      htpmon(hru,8) = htpmon(hru,8) + epes
      htpmon(hru,9) = htpmon(hru,9) + alai
   end subroutine htpmon_add

!******************************************************************************


!******************************************************************************

subroutine assign_hsum
!     Correction from Fred Hattermann
!     Reason: nn is defined for HRU, & sumul(k) and sumfc(k) are not appropriate
   hsumul(j,jea) = 0.
   hsumfc(j,jea) = 0.

   do l = 1, nn
      hsumul(j,jea) = hsumul(j,jea) + ul(l,k)
      hsumfc(j,jea) = hsumfc(j,jea) + fc(l,k)
   end do

   if (hsumfc(j,jea).lt.0.001) then
      write(6,*)' '
      write(6,*)'ERROR in subbasin:'
      write(6,fmt='(a12,i5,a10,i5,a10,i5,a7,i5,a10)')'In subbasin',j,', hydrotop',jea,', land use',n,', soil',snum(k),' FC = 0'
      write(6,fmt='(a30,i5,a4,i5,a22)')'To check: is soil at position', k,'id:',snum(k), 'included in soil.cio?'
      write(6,*)'SWIM STOPS'
      STOP
   end if

   hwss(1,j,jea) = -1.2677 + dlog(hsumfc(j,jea))
   hwss(2,j,jea) = 1.6234 / hsumfc(j,jea)

end subroutine assign_hsum

!******************************************************************************


!******************************************************************************

subroutine init_water_nutrients(n,nn)
! assign initial conditions for water & nutrients at the first
! day of simulation
   integer, intent(in) :: n  ! land use type
   integer, intent(in) :: nn ! number of soil arable layers
   integer :: l = 0

   do l=1,nn
      ste(j,jea,l) = stin(l,k)
   end do
   swe(j,jea) = swin(k)
   !###########################
   !#### SNOW MODULE       ####
   !###########################
   if ( .NOT. bSnowModule ) then
      snoa(j,jea) = sno(j)
   end if
   !###########################

   !****   Initial conditions for nutrients
   do l=1,nn
      ano3(j,jea,l) = wno3(l,k)
      anora(j,jea,l) = wmn(l,k)
      anors(j,jea,l) = wn(l,k)
      plab(j,jea,l) = ap(l,k)
      porg(j,jea,l) = wpo(l,k)
      pma(j,jea,l) = pmn(l,k)
      pms(j,jea,l) = op(l,k)
   end do

   !****   Correction of initial values ano3(), plab() for noncropland
   if (n.ne.5) then
      do l=1,nn
         ano3(j,jea,l) = ano3(j,jea,l) *.1
         plab(j,jea,l) = plab(j,jea,l) *.1
      end do
   end if

   !**** INITIALIZATION of crop parameters
   g(j,jea)=0.
   alai(j,jea)=alai0(n)
   !****   Correction of initial values ano3(), plab() for noncropland
   if (n == 5) then
      cva(j,jea)=cv(j)
   else
      cva(j,jea)=0.
      nucr(j,jea) = 0   ! crop number (crop.dat)
      dm(j,jea) = 0.    ! total biomass
      rd(j,jea) = 0.    ! root depth
   end if

   if ( n.eq.6.or.n.eq.7.or.n.eq.12 ) then
      dm(j,jea) = 3.
      rd(j,jea) = 300.
   end if

end subroutine init_water_nutrients

!******************************************************************************


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      
end subroutine subbasin

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine hrflowtt(j,je,k)
!**** PURPOSE: THIS SUBROUTINE CALCULATES RETURN FLOW TRAVEL TIMES 
!**** CALLED IN:   SUBBASIN 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!   
!      >>>>> COMMON PARAMETERS & VARIABLES 
!      bff          = baseflow factor for basin, from readbas
!      hrtc(j,je,l) = return flow travel time, calculated here, h
!      nn           = number of soil layers, calc in readsol
!      sc(l,k)      = sat. cond., input in readsol, mm/h
!      z(l,k)       = depth, input in readsol, mm
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      dg    = local par
!      i1    = local par
!      ii    = local par
!      la    = local par
!      sumk  = local par
!      xx    = local par
!      yy    = local par
!      zk    = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,k,i1,ii,la
      real(8) dg,sumk,xx,yy,zk

      sumk = 0.
      yy = 0.
      zk = 0.
      do 10 la = 1, nn
        ii = nn - la + 1
        i1 = ii - 1
        if (i1.ne.0) then
          xx = z(i1,k)
        else
          xx = 0.
        end if                    
        dg = z(ii,k) - xx
        yy = yy + dg
! SL begin
!        sumk = sumk + dg / sc(ii,k)    
!        zk = yy / sumk
!        zk = sc(ii,k) / zk
        if ( bSubcatch ) then
           sumk = sumk + dg / (sc(ii,k) * sccor(j))
           zk = yy / sumk
           zk = (sc(ii,k) * sccor(j)) / zk
        else
           sumk = sumk + dg / sc(ii,k)
           zk = yy / sumk
           zk = sc(ii,k) / zk
        end if
! SL end
        xx = -0.5447 + 0.01757 * zk
        if (xx.lt.-20.) xx = -20.
        if (bff(j).le.0.) bff(j) = .01
        hrtc(j,je,ii) = 10. * (1.-zk/(zk+exp(xx))) / bff(j)
   10 continue
      return



end subroutine hrflowtt

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
