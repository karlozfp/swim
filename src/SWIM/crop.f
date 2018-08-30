C     FILE crop.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine crpmd(j,je,k,n) 	hydrotop		 
C     subroutine operat(j,je,k,n)	crpmd	  
C     subroutine growth(j,je,k,n)	crpmd



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

	

      subroutine crpmd(j,je,k,n,wet)
C**** PURPOSE: THIS SUBROUTINE CALCULATES DAILY POTENTIAL & ACTUAL GROWTH 
C     OF TOTAL PLANT BIOMASS AND ROOTS AND CALCULATES LEAF AREA INDEX.
C     IT ADJUSTS DAILY BIOMASS TO WATER, TEMP. & NUTR. STRESS. 
C**** CALLED IN:  HYDROTOP  
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      actual     = actual evapotranspiration, mm
C      alai(j,je) = leaf area index 
C      cva(j,je)  = vegetation cover, kg/ha
C      dm(j,je)   = total biomass, kg/ha
C      ep         = plant transpiration, mm
C      es         = soil evaporation, mm
C      g(j,je)    = fraction of heat units to maturity accumulated
C      icrop      = switch code to print from crop()
C      icrsb      = number of subbasin to print from crop(), if icrop = 1
C      icrso      = number of soil to print from crop(), if icrop = 1 
C      ida        = current day
C      igro(j,je) = vegetation index, =1 if vegetation is growing
C      rd(j,je)   = root depth, mm
C      rsd(j,je,2)= crop residue in two upper soil layers, kg/ha
C      ts         = temperature stress factor
C      uap        = P uptake in hydrotope, kg/ha
C      ws(j,je)   = water stress factor
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,n,k,wet

      uap = 0.
      ts=0.

C**** CALC vegetation cover
      cva(j,je) = .8 * dm(j,je) + rsd(j,je,1) 
            
C#### CALL OPERAT
      call operat(j,je,k,n)

C#### CALL WSTRESS to COMPUTE WATER STRESS 
      if (igro(j,je).ge.1) call wstress(j,je,k,n,wet)
      actual = ep + es

C#### CALL GROWTH
      call growth(j,je,k,n)
            
      if(icrop.eq.1.and.j.eq.icrsb.and.k.eq.icrso)
     *    write (46,201)  ida,ws(j,je),ts,g(j,je)
     *                   ,dm(j,je),alai(j,je),rd(j,je)

  200 format('+++  ',3i5,9f10.3)
  201 format (i5,3f6.2,3f15.2)
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine operat(j,je,k,n)
C**** PURPOSE: TO DEFINE PLANT OPERATIONS 
C**** CALLED IN:  CRPMD   
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      alai(j,je)     = leaf area index
C      aryld(j,k,icr) = fraction of area by crop per sub,soil
C      arylda(iy,icr) = fraction of area by crop per year
C      aryldc(icr)    = fraction of area by crop
C      arylds(k,icr)  = fraction of area by crop per soil
C      avyld(j,k,icr) = av. yld per sub,soil,crop, kg/ha
C      avylda(iy,icr) = av yld per year, crop, kg/ha
C      avyldc(icr)    = av yld per crop, kg/ha
C      avylds(k,icr)  = av. yld per soil,crop, kg/ha
C      cva(j,je)      = vegetation cover, kg/ha
C      dm(j,je)       = total biomass, kg/ha
C      fon(j,je,l)    = fresh org N, kg/ha
C      fop(j,je,l)    = fresh org P, kg/ha
C      frar(j,je)     = fractional area of hydrotope in subbasin
C      g(j,je)        = fraction of heat units to maturity accumulated 
C      hi(icr)        = harvest index for crop (database), for maize & potat.
C      hia(j,je)      = harvest index
C      hiad(j,je)     = harvest index, adjusted
C      huharv(j,je)   = harvest index heat unit
C      icc            = index for cover crop corr. number in crop database
C      ida            = current day
C      idayx          = par = ida, to calc ndgro - number of growth days
C      idop(5,iop)    = day of operation
C      igro(j,je)     = vegetation index, =1 if yes
C      iopc(5,iop)    = opeartion code: 1 - planting, ...
C      ipo            = index for potatoes corr. number in crop database
C      isba           = index for s. barley corr. number in crop databas
C      istyr          = starting year
C      iwb            = index for w. barley corr. number in crop databas 
C      iwr            = index for w. rye corr. number in crop databas
C      iww            = index for w. wheat corr. number in crop databas
C      iy             = current year as counter (1,...,nbyr)
C      ncrp(5,iop)    = crop number
C      ndgro          = number of growth days
C      ndpri          = day to write crop yield for GIS output
C      nucr(j,je)     = crop number (database)
C      olai(j,je)     = alai(j,je) - leaf area index
C      rsd(j,je,2)    = residue, kg/ha
C      rwt(j,je)      = fraction of root weight
C      sbar(j)        = subbasin area, m2
C      snup(j,je)     = N uptake, kg/ha
C      spup(j,je)     = P uptake, kg/ha
C      swh(j,je)      = actual transp. by plants, mm
C      swp(j,je)      = potent. transp. by plants, mm
C      ws(j,je)       = water stress
C      yld(j,je)      = crop yield, kg/ha
C      ylda(j,k)      = crop yield for subbasin and soil, kg/ha
C      >>>>>

C      >>>>> STATIC PARAMETERS 
C      icr            = local par
C      ii             = local par
C      ioper          = local par
C      xx             = local par
C      yield          = current yield
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,n,k
      integer icr, ii, ioper
      real(8) xx, yield
      
C*********************************************************** START IF (IGRO=0)  
C**** CHECK if day of planting, then goto 10 - planting
      if(igro(j,je).eq.0) then
      do 21 ii=1,mop
        if(ida.eq.idop(n,ii)) then
          nucr(j,je) = ncrp(n,ii)
          ioper = iopc(n,ii)
          if(ioper.eq.1) go to 10
        endif
   21   continue       
        go to 11  

   10   igro(j,je)=1
        g(j,je) = 0.
        dm(j,je) = 0.01
        snup(j,je) = 0.
        spup(j,je) = 0.
        swh(j,je) = 0.
        swp(j,je) = 0.
        huharv(j,je) = 0.
        hia(j,je) = 0.
        olai(j,je) = 0.
        rwt(j,je) = 0.
        idayx = -99
        ndgro = 0         
      endif
C*********************************************************** END IF (IGRO=0)  
 
C*********************************************************** START IF(IGRO=1)  
C**** CHECK if day of harvest and kill       
   11 if(igro(j,je).eq.1) then 
        do 22 ii=1,mop
        if(ida.eq.idop(n,ii)) then
          nucr(j,je) = ncrp(n,ii)
          ioper = iopc(n,ii)
          if(ioper.eq.2) go to 12
          if(ioper.eq.3) go to 13
          if(ioper.eq.4) go to 14
        endif
   22   continue
        go to 16    

C****   CALC HARVEST AND KILL
   12   igro(j,je)=0
        if(hiad(j,je).gt.hi(nucr(j,je))) hiad(j,je) = hi(nucr(j,je))
        if(hiad(j,je).gt.1.) hiad(j,je) = 1.

C****   CALC residue & fresh org N and P (no residue, fon, fop for cover crop)
        if (nucr(j,je).ne.icc) then        
        rsd(j,je,1) = 0.25*(1.-rwt(j,je))*(1.-hiad(j,je))*dm(j,je)
     *          +rsd(j,je,1)
        rsd(j,je,2) = 0.75*(1.-rwt(j,je))*(1.-hiad(j,je))*dm(j,je)
     *          +rsd(j,je,2)
        if(rsd(j,je,1).le.0.) rsd(j,je,1)=1.e-6 
        if(rsd(j,je,2).le.0.) rsd(j,je,2)=1.e-6 

        fon(j,je,1) = rsd(j,je,1) * .008
        fon(j,je,2) = rsd(j,je,2) * .008
        
        fop(j,je,1) = rsd(j,je,1) * .0011
        fop(j,je,2) = rsd(j,je,2) * .0011
        endif
        
C****   CALC yield
C       ATTN! Harvest index used for grains: hia(), for maize, potatoes: hi()
C       No yield for cover crop (icc)
        if(nucr(j,je).eq.iww.or.nucr(j,je).eq.iwb.or.
     *       nucr(j,je).eq.iwr.or.nucr(j,je).eq.isba) then
          yield = 0.85 * dm(j,je) * hia(j,je)
        else if (nucr(j,je).eq.ipo) then
          yield = 1.00 * dm(j,je) * hi(nucr(j,je))
        else
          yield = 0.85 * dm(j,je) * hi(nucr(j,je))
        endif
        if(nucr(j,je).eq.icc) yield = 0.            

        ylda(j,k) = yield
        yld(j,je) = yld(j,je) + yield
        dm(j,je) = 0.
        ws(j,je) = 1.
        alai(j,je) = 0.
        hia(j,je) = 0.
        cva(j,je) = rsd(j,je,1)+rsd(j,je,2)
        g(j,je) = 0.

        idayx = 0
        icr = nucr(j,je)

C****   CALC Day to write crop yield for GIS output (except cover crop)
        if (icr.ne.icc) ndpri = ida + 3
        
C****   CALC average yield
C       avyld(j,k,icr) av yld per sub,soil,crop & aryld(j,k,icr): frac. area
C       avylds(k,icr)  av yld per soil,crop     & arylds(k,icr): frac. area
C       avyldc(icr)    av yld per crop          & aryldc(icr): frac. area
C       avylda(iy,icr) av yld per year, crop    & arylda(iy,icr): frac. area 

        if ( icrop == 1 ) then
            avyld(j,k,icr) = avyld(j,k,icr) + ylda(j,k)*frar(j,je)/100.
            aryld(j,k,icr) = aryld(j,k,icr) + frar(j,je)
        endif
        
        avylds(k,icr) = avylds(k,icr) + ylda(j,k)*frar(j,je)/100.            
        arylds(k,icr) = arylds(k,icr) + frar(j,je)            

        avyldc(icr) = avyldc(icr) + ylda(j,k)*frar(j,je)/100.            
        aryldc(icr) = aryldc(icr) + frar(j,je)            

        avylda(iy,icr) = avylda(iy,icr) + ylda(j,k)*frar(j,je)/100.            
        arylda(iy,icr) = arylda(iy,icr) + frar(j,je)            

        if (icrop == 1 .AND. ylda(j,k).gt.0.) then
        write(58,101)  nucr(j,je),
     *   istyr+iy-1,j,k,ylda(j,k)/100., frar(j,je)*sbar(j)/10000.
        endif

        go to 16

C****   CALC HARVEST ONLY - CUTTING, NO KILL
   13   if(hiad(j,je).gt.hi(nucr(j,je))) 
     *         hiad(j,je) = hi(nucr(j,je))
        yield = (1.-rwt(j,je)) * dm(j,je) * hiad(j,je)
        yld(j,je) = yld(j,je) + yield        
        xx = dm(j,je)
        dm(j,je) = dm(j,je) - yield
        alai(j,je) = alai(j,je)*dm(j,je)/xx
        g(j,je)=g(j,je)*dm(j,je)/xx
        go to 16

C****   CALC KILL
   14   igro(j,je)=0
        dm(j,je) = 0.
        ws(j,je) = 1.
        alai(j,je) = 0.
        cva(j,je)=0.
        g(j,je) = 0.
      endif
C*********************************************************** END IF (IGRO=1)  

   16 continue

  100 format(a5,' Crp=',i3,'  Yr=',i5,'  Sub=',i4,'  Sol=',i4,
     *  '  Yld=',  f7.1,' dt/ha','  Area=',f10.1,' ha')      
  101 format(' Crp=',i3,'  Yr=',i5,'  Sub=',i4,'  Sol=',i4,
     *  '  Yld=',  f7.1,' dt/ha','  Area=',f10.1,' ha')      
      return 
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine growth(j,je,k,n)
C**** PURPOSE: TO SIMULATE PLANT GROWTH 
C**** CALLED IN:  CRPMD   
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      actual       = actual evapotranspiration, mm
C      alai(j,je)   = leaf area index
C      ald1(icr)    = shape parameter for the LAI developement equation
C      ald2(icr)    = shape parameter for the LAI developement equation
C      be(icr)      = biomass-energy ratio for crop, kg m2 MJ-1 ha-1 d-1
C      blai(icr)    = max LAI for crop
C      dlai(icr)    = fraction of season, when LAI declines
C      dm(j,je)     = total biomass, kg/ha
C      flu(j)       = fraction of subbasin area in the basin
C      frar(j,je)   = fractional areas of hydrotope in subbasin
C      g(j,je)      = fraction of heat units to maturity accumulated, 1/1
C      hi(icr)      = harvest index for crop (from database)
C      hia(j,je)    = harvest index
C      hiad(j,je)   = harvest index, adjusted
C      huharv(j,je) = harvest index heat unit
C      hun(icr)     = potential heat units required for maturity of crop
C      ialpha       = switch parameter for CO2 effect on net photosynth.
C      ida          = current day
C      idayx        = ida, to calc ndgro - number of growth days
C      idlef        = code for the day length effect in crop 0/1
C      igro(j,je)   = vegetation index, =1 if yes 
C      ilcc(icr)    = land cover category --> readcrp
C      ndgro        = number of growth days
C      nucr(j,je)   = crop number (database)
C      olai(j,je)   = alai(j,je) - leaf area index
Cblock pit          = 58.13
C      potentl      = potential evapotranspiration, mm
C      ra(j)        = solar radiation in subbasin j, J/cm^2
C      rd(j,je)     = root depth, mm
C      rdmx(icr)    = maximum root depth, mm 
C      rwt(j,je)    = fraction of root weight
C      sdn          = sum N stress days
C      sdp          = sum P stress days
C      sdt          = sum temp stress days
C      sdw          = sum water stress days
C      strsn        = N stress factor
C      strsp        = P stress factor
C      swh(j,je)    = actual transp. by plants, mm
C      swp(j,je)    = potent. transp. by plants, mm
C      tb(icr)      = base temp. for plant growth, degree C
C      ts           = temp. stress
C      tsav(j,je)   = temp. stress, accumulated
C      tx(j)        = average daily temp., degree C
C      ws(j,je)     = water stress
C      wsav(j,je)   = water stress, accumulated
C      ylc(j)       = cos(lat()/clt), lat() - lat, clt=57.296
C      yls(j)       = sin(lat()/clt), lat() - lat, clt=57.296
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      beadj      = adjusted on CO2 biomass-energy ratio
C      ch         = local par
C      dayl       = day length
C      ddm        = delta dm
C      delg       = delta g
C      deltalai   = delta LAI
C      dlef       = local par
C      duma       = local par
C      f          = fraction of plant's maximum leaf area index corresponding to 
C                   to a given fraction of potential heat units for plant
C      ff         = delta f for the day
C      h          = local par
C      heat       = local par
C      par        = local par
C      reg        = local par
C      sd         = local par
C      tgx        = local par
C      x1         = local par
C      x2         = local par
C      x3         = local par
C      x4         = local par
C      xi         = local par
C      xinc       = local par
C      xy         = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      use mod_snow !#### SNOW MODULE       ####
      implicit NONE
      integer j, je, n, k
      real(8) beadj, ch, dayl, ddm, delg, deltalai, dlef, duma, f, ff 
      real(8) h, heat, par, reg, sd, tgx, x1, x2, x3, x4, xi, xinc, xy 

      x1 = 0.05
      x2 = 0.9
      x3 = 10.05
      x4 = 50.90
      
      !ToDo: dlef = 0. ! ???
      ! dlef is not initialized if  g(j,je) >=0.5 .AND. idlef.NOT.0

C*********************************************************** START IF (IGRO>=1)  
C**** CALC DAILY INCREASE IN HEAT UNITS delg & ACCUMULATED HEAT UNITS g()
      if(igro(j,je).ge.1) then

         if (idayx.ne.ida.and.g(j,je).gt.0.) then
            ndgro = ndgro +1
            idayx = ida
         end if    

         !###########################
         !#### SNOW MODULE       ####
         !###########################
         if ( bSnowModule ) then
            delg = (tmit-tb(nucr(j,je)))/hun(nucr(j,je))
         else
            delg = (tx(j)-tb(nucr(j,je)))/hun(nucr(j,je))
         end if
       !###########################

        
C****   CALC day length (from clgen)
        xi = ida
        sd = .4102 * sin((xi-80.25)/pit)
        ch = -yls(j) * tan(sd) / ylc(j)
        if (ch.lt.1.) then
          if (ch.le.-1.) go to 110
          h = acos(ch)
          go to 120
        end if
        h = 0.
        go to 120
  110   h = 3.1416
  120   dayl = 7.72 * h

C****   Correction of delg on day length (Jan Gräfe)
        if(g(j,je).lt.0.5) then
          dlef = 0.25 +0.75*(dayl - 8.)/8.
          if(dlef.gt.1.) dlef=1.
          if(dlef.lt.0.25) dlef=0.25
        endif

        if (idlef.eq.0) then
          delg = delg*1.
        else
          delg = delg*dlef
        endif 
                
        if (delg.lt.0.) delg = 0.
        g(j,je) = g(j,je) + delg
        if (g(j,je).gt.1.) g(j,je) = 1.

C*********************************************************** START IF (G<=1)  
C****   GROWTH SEASON
        if (g(j,je).le.1.) then
        
C####     CALL TSTRESS - CALC TEMPERATURE STRESS
          !###########################
          !#### SNOW MODULE       ####
          !###########################
          if ( bSnowModule ) then
            tgx = tmit - tb(nucr(j,je))
          else
            tgx = tx(j) - tb(nucr(j,je))
          end if
          !###########################

          if (tgx.le.0.) then
            ts = 0.
          else
            call tstress(tgx,j,je,n)
          end if

C****     COMPUTE ROOT DEPTH
          if(ilcc(nucr(j,je)).le.2.or.ilcc(nucr(j,je)).eq.4.
     *        or.ilcc(nucr(j,je)).eq.5) then
            rd(j,je) = 2.5 * g(j,je) * rdmx(nucr(j,je))
            if(rd(j,je).gt.rdmx(nucr(j,je))) rd(j,je)=rdmx(nucr(j,je))         
          else
            rd(j,je)=rdmx(nucr(j,je))
          endif      
              
C****     COMPUTE potential increase in biomass - ddm
C         STANDARD VERSION: WITHOUT CO2 FERTILIZATION 
          par = .005 * ra(j) * (1.-exp(-.65*(alai(j,je)+.05)))
          ddm = be(nucr(j,je)) * par
          if (ddm.lt.0.) ddm = 0.

C****     COMPUTE potential increase in biomass - ddm
C         VERSION for CLIMATE IMPACT ASSESSMENT: WITH CO2 FERTILIZATION
C####     CALL ADJUSTBE to adjust be if ialpha = 1
          if (ialpha.eq.1) then
            call adjustbe(j,je,beadj)
            ddm = beadj * par
            if (ddm.lt.0.) ddm = 0.
          endif

C####     CALL NUPTAKE & PUPTAKE - CALC N AND P UPTAKE
          call nuptake(j,je,nucr(j,je))
          call puptake(j,je,nucr(j,je))

C****     CALC crop growth regulating factor: reg 
          strsn = 1.
          strsp = 1.
          reg = amin1(ws(j,je),ts,strsn,strsp)
          if(reg.lt.0.) reg = 0.
          if(reg.gt.1.) reg = 1.

          tsav(j,je) = tsav(j,je) + ts
          wsav(j,je) = wsav(j,je) + ws(j,je)

C****     CALC biomass dm() & root weight fraction rwt()
          dm(j,je) = dm(j,je) + ddm * reg
          rwt(j,je) = (.4-.2*g(j,je))
                    
C****     CALC harvest index under favourable conditions - hia() 
C              & real(8) harvest index - hiad()
          f = g(j,je) / (g(j,je) + 
     *        exp(ald1(nucr(j,je))-ald2(nucr(j,je))*g(j,je)))
          ff = f - huharv(j,je)
          huharv(j,je) = f

	  if (igro(j,je) .gt. 0) then
	    if (g(j,je) .gt. 0.5) then 
  	     swh(j,je) = swh(j,je) + actual
	     swp(j,je) = swp(j,je) + potentl
	    endif	  
            heat = 100. * (1.-(dlai(nucr(j,je))-g(j,je)))
C####       CALL SCURVE
	    call scurve(x1,x2,x3,x4)
	    hia(j,je) = hi(nucr(j,je)) * 100 * g(j,je) / 
     *            (100 * g(j,je)+exp(11.1-10. * g(j,je)))
	    xinc = 100. * swh(j,je) / (swp(j,je) + 1.e-6)
            if (xinc.lt.0.) xinc = 0.
	    duma = xinc / (xinc + exp(x1 - x2 * xinc))
	    hiad(j,je) = hia(j,je) * duma
 	  else
	  endif  

C****     CALC SUM STRESS DAYS 
          sdw = sdw + (1.-ws(j,je)) * flu(j) * frar(j,je)
          sdt = sdt + (1.-ts) * flu(j) * frar(j,je)
          sdn = sdn + (1.-strsn) * flu(j) * frar(j,je)
          sdp = sdp + (1.-strsp) * flu(j) * frar(j,je)

C****     COMPUTE LEAF AREA INDEX - alai()
          if (g(j,je).le.dlai(nucr(j,je))) then
          if (alai(j,je).gt.blai(nucr(j,je)))
     *         alai(j,je)= blai(nucr(j,je)) 
           xy=sqrt(reg)
           deltalai = ff * blai(nucr(j,je)) * 
     *        (1.-exp(5.*(alai(j,je)-blai(nucr(j,je)))))*xy
           alai(j,je) = alai(j,je) + deltalai
           if (alai(j,je).gt.blai(nucr(j,je))) 
     *        alai(j,je)= blai(nucr(j,je))
            olai(j,je) = alai(j,je)
          else
            alai(j,je) = 16. * olai(j,je) * (1.-g(j,je)) ** 2
            if (alai(j,je).gt.olai(j,je)) alai(j,je) = olai(j,je)
          end if
          
        end if
C*********************************************************** END IF (G<=1)  
      end if
C*********************************************************** END IF (IGRO>=1)  

      return
      end


     
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



