C     FILE evap.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine  evap(j,je,k,n)        hydrotop



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine evap(j,je,k,n)
C**** PURPOSE: THIS SUBROUTINE COMPUTES THE AMOUNT OF SOIL EVAPORATION  
C             & THE POTENTIAL PLANT TRANSPIRATION USING RITCHIE'S METHOD  
C**** CALLED IN:  HYDROTOP  
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      alai(j,je)    = leaf area index 
C      canev         = canopy evaporation, mm
C      canstor(j,je) = canopy water storage, mm
C      cva(j,je)     = vegetation cover, kg/h
C      ecal          = general potential evap calibration factor
C      eo            = potential evapotranspiration, mm
C      eopot         = potential evapotranspiration, mm
C      ep            = plant transpiration, mm
C      es            = soil evaporation, mm
C      et            = es + ep, mm
C      humi(j)       = air humidity in the subbasin, %
C      ida           = current day
C      ievap         = switch code to print from evap()
C      ievhd         = number of hydrotope to print from evap(), if ievap = 1 
C      ievsb         = number of subbasin to print from evap(), if ievap = 1 
C      nn            = number of soil layers
C      omega         = month factor for Turc(Ivanov) evap (Glugla 1989)
Cblock pit           = parameter for estimation of the day length
C      preinf(j,je)  = precipitation adjusted for canopy storage, mm
C      qd            = daily surface runoff, mm
C      ra(j)         = solar radiation, J/cm^2
C      s1(j,je)      = internal func. for Richie's method to estimate es
C      s2(j,je)      = internal func. for Richie's method to estimate es
C      salb(j)       = soil albedo 
C      snoa(j,je)    = water content in snow, mm
C      snoev         = snow evaporation, mm
C      ste(j,je,l)   = water storage in a layer, , mm, calc in hydrotop & purk
C      thc           = correction factor for potential evapotranspiration 
C                      range for thc: (0.8-1.0), value 1. - from R. Muttiah
C      tv(j,je)      = internal func. for Richie's method to estimate es
C      tx(j)         = average daily temperatue, degree C
C      ylc(j)        = cos(lat()/clt), lat() - lat, clt=57.296, to calc rmx 
C      yls(j)        = sin(lat()/clt), lat() - lat, clt=57.296, to calc rmx 
C                      (convert degrees to radians (2pi/360=1/57.296) )
C      z(l,k)        = soil depth, mm
C      >>>>>

C      >>>>> STATIC PARAMETERS 
C      alb   = soil albedo
C      aph   = local par
C      cej   = local par
C      ch    = local par
C      corn  = correction on land use for Turc-Ivanov evaporation
C      d     = inclination of saturation vapor pressure curve
C      dayl  = day length
C      dd    = local par
C      eaj   = local par
C      eos   = local par
C      esd   = local par
C      esx   = local par
C      ff    = local par
C      gma   = local par
C      h     = local par
C      hh    = local par
C      ho    = local par
C      jk1   = local par
C      jk2   = local par
C      p     = local par
C      rmx   = max solar radiation
C      rto   = local par
C      sb    = local par
C      sd    = local par
C      skyem = sky emissivity
C      sp    = local par
C      suu   = local par
C      thrad = add. radiation due to sky emissivity
C      tk    = local par
C      tkk   = local par
C      turc  = potential evapotranspiration according to Turc
C      u     = local par
C      vp    = vapor pressure
C      vps   = saturated vapor pressure
C      xi    = current day
C      xx    = local par
C      yc    = local par
C      ys    = local par
C      yy    = local par
C      zz    = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      use mod_snow !#### SNOW MODULE       ####
      implicit NONE
      integer j,je,n,k,jk1,jk2
      real(8) alb,aph,cej,ch,corn,d,dayl,dd,eaj,eos,esd,esx,ff,gma
      real(8) h,hh,ho,p,rmx,rto,sb,sd,skyem,sp,suu,thrad,tk,tkk
      real(8) u,vp,vps,xi,xx,yc,ys,yy,zz
      real(8) turc

      esd = 300.
      cej = -5.e-5
      u   = 6.
      vps = 0.0
      corn = 1.0

      !###########################
      !#### SNOW MODULE       ####
      !###########################
      if ( bSnowModule ) then
         tx_tmp = tmit
      else
        tx_tmp = tx(j)
      end if
      !###########################
C sl begin
C     If radiation = 0.0 there will be caused an error while computing evap.
C     This was my experience in a catchment in northern Sweden, hence I include here following correction:
      if ( ra(j) <= 0. ) ra(j) = 1.e-6
C sl end

C**** CALC albedo 
      p = preinf(j,je) - qd
      eaj = exp(cej*(cva(j,je)+.1))
      tk = tx_tmp + 273.
      tkk = tk * tk
      if (idvwk.eq.1) then
        if (tx_tmp.ge.0.) then
          vps = 6.11 * exp((17.62*tx_tmp)/(243.12+tx_tmp))
          d = vps * 4284. / (243.12+tx_tmp)**2.
        else
          vps = 6.11 * exp((22.46*tx_tmp)/(272.62+tx_tmp))
          d = vps * 6123. / (272.62+tx_tmp)**2.
        end if
        gma = d / (d+.655) 
      else
        d = exp(21.255-5304./tk) * 5304. / tkk
        gma = d / (d+.68)
      end if
      if (snoa(j,je).le.5.) then
        alb = salb(j)
        if (alai(j,je).gt.0.) alb = .23 * (1.-eaj) + salb(j) * eaj
      else
        alb = .6
        !###########################
        !#### SNOW MODULE       ####
        !###########################
        if ( bSnowModule ) then
           if ( rsn(j,je).eq.rnew ) alb = .8
        end if
        !###########################
      end if

C**** CALC max solar radiation rmx, coef 711 changed to 916
C                               916 (ly) ==> 3847.2 (J/cm^2)
      xi = ida
      sd = .4102 * sin((xi-80.25)/pit)
      ch = -yls(j) * tan(sd) / ylc(j)
      if (ch.lt.1.) then
        if (ch.le.-1.) go to 200
        h = acos(ch)
        go to 201
      end if
      h = 0.
      go to 201
  200 h = 3.1416
  201 dayl = 7.72 * h
      dd = 1. + .0335 * sin((xi+88.2)/pit)
      ys = yls(j) * sin(sd)
      yc = ylc(j) * cos(sd)
ccc      rmx = 711. * (h*ys+yc*sin(h))
      rmx = 3847.2 * (h*ys+yc*sin(h))

C**** CORRECTION of radiation on sky emissivity (Ranjan)
C     ho1 - old, ho - from Ranjan, divisor 58.3 for cal ==> 244.86 for J
C     thrad correction, thc - calib. coef.(0.8-1.)
C       ho1 = ra(j) * (1.-alb) / 58.3
C       ho = (ra(j) * (1.-alb)+thrad) / 58.3
      if (idvwk.ne.1) then
C     Idso & Jackson (1969) J. Geophys. Res. 74(23):5397--5403
        skyem = 1. - 0.261*exp(-7.77e-4*(tk-273.15)**2)
        thrad = (skyem-0.96)*4.914e-7*tk**4*(.2+.8*(ra(j)/rmx))
        ho = (ra(j) * (1.-alb)+thc(j)*thrad) / 244.86
      else
C     DVWK (1995) Merkblatt Nr. 238 / Brunt (1932)
        if ( humi(j) < 0. ) then
           vp = vps * .65
        else
           vp = vps * (humi(j)/100.)
        end if
        !vp = vps * (humi(j)/100.)
        skyem = 0.34 - 0.044 * sqrt(vp)
        thrad = -5.67 * 8.64e-8 * tk**4. * skyem * (.1+.9*(ra(j)/rmx))
        ho = (ra(j) * (1.-alb) + thrad) / (249.8 - .242*tx_tmp)
      end if
      if (ho.lt.0.) ho = 0.001
      
C**** CORRECTION of aph=1.28 on humidity: 
C     aph = f(humidity):  aph(60) = 1.74, aph(90)=1.28
      if ( humi(j) < 0. ) then
         hh = .65
      else
         hh = humi(j)/100.
      end if
      !hh = humi(j)/100.
      zz = 40. * hh - 29.
      if (zz.gt.10.) zz = 10.
      if (zz.lt.-10.) zz = -10.      
      ff = hh / (hh + exp(zz))
      aph = 1.28 + 0.46 * ff      

C**** CALC POTENTIAL ET
      if (iemeth.eq.0) then
        if (idvwk.eq.1) then
          eo = ecal(j) * 1.26 * ho * gma
        else
          eo = ecal(j) * aph * ho * gma
        end if
      else
C     TURC-IVANOV POTENTIAL EVAP
        if (tx_tmp.ge.5.) then
C       DEFINE MONTH FACTOR FOR TURC-IVANOV EVAP (assigned in common.f90)
          if ( iemeth == 2 ) omega(mo) = 1. ! No monthly correction
          turc = .0031*omega(mo)*(ra(j)+209.4) * (tx_tmp/(tx_tmp+15.))
        else
          if ( humi(j) < 0. ) then
            turc = 0.000036 * (25.+tx_tmp)**2. * 35.
          else
            turc = 0.000036 * (25.+tx_tmp)**2. * (100.-humi(j))
          end if
          !turc = 0.000036 * (25.+tx(j))**2 * (100.-humi(j))
        end if
C       CORRECTION ON LAND USE 
        if(n.eq.1) corn  = 1.3
        if(n.eq.2) corn  = 1.        
        if(n.eq.3) corn  = 1.
        if(n.eq.4) corn  = 1.
        if(n.eq.5) corn  = 0.9
        if(n.eq.6) corn  = 0.9
        if(n.eq.7) corn  = 1. 
        if(n.eq.8) corn  = 1.
        if(n.eq.9) corn  = 1.14
        if(n.eq.10) corn = 1.19
        if(n.eq.11) corn = 1.1
        if(n.eq.12) corn = 1.3
        if(n.eq.13) corn = 1.3
        if(n.eq.14) corn = 1.
        if(n.eq.15) corn = 0.9

        eo = ecal(j) * corn * turc
      end if

C**** CORRECTION of pot. evap. on interception
C     eopot is used later as Pot Evap for write()
C     eo is corrected for interception and used to calc es, ep
      eopot = eo
      canev = 0.
      if (canstor(j,je).gt.0.) then 
      eo = eo - canstor(j,je)
      if (eo.lt.0.) then
        canstor(j,je) = -eo
        canev = eopot
        eo = 0.
      else
        canev = canstor(j,je)
        canstor(j,je) = 0.
      endif
      endif
      
C**** CALC soil evaporation es
      eos = eo * eaj
      if (s1(j,je).ge.u) go to 30
      sp = s1(j,je) - p

      if (sp.gt.0.) then
        s1(j,je) = sp
        go to 20
      end if

   10 s1(j,je) = 0.
   20 s1(j,je) = s1(j,je) + eos
      suu = s1(j,je) - u

      if (suu.le.0.) then
        es = eos
        go to 40
      end if

      es = eos - .4 * suu
      s2(j,je) = .6 * suu
      tv(j,je) = (s2(j,je)/3.5) ** 2.
      go to 40

   30 sb = p - s2(j,je)
      if (sb.ge.0.) then
        p = sb
        s1(j,je) = u - p
        tv(j,je) = 0.
        if (s1(j,je).lt.0.) go to 10
        go to 20
      end if

      tv(j,je) = tv(j,je) + 1.
      es = 3.5 * sqrt(tv(j,je)) - s2(j,je)

      if (p.le.0.) then
        if (es.gt.eos) es = eos
      else
        esx = 0.8 * p
        if (esx.le.es) esx = es + p
        if (esx.gt.eos) esx = eos
        es = esx
      end if

      s2(j,je) = s2(j,je) + es - p
      tv(j,je) = (s2(j,je)/3.5) ** 2.

   40 if (es.le.0.) es = 0.

C**** CALC plant transpiration ep: preliminary estimation, 
C          later recalc in wstress (vegfun.f)
      if (alai(j,je).le.3.0) then
        ep = alai(j,je) * eo / 3.
      else
        ep = eo
      end if

      et = es + ep + canev
      if (eopot.lt.et) then
        et = eopot
        es = et - ep -canev
      end if

      xx = es

C**** RECALC ste(), sno()
      if (snoa(j,je).lt.es) then
        xx = xx - snoa(j,je)
        snoa(j,je) = 0.
        do 50 jk2 = 1, nn
          if (z(jk2,k).gt.esd) go to 70
          if (ste(j,je,jk2).gt.xx) go to 60
          xx = xx - ste(j,je,jk2)
          ste(j,je,jk2) = 0.
   50   continue
        go to 80
   60   ste(j,je,jk2) = -xx + ste(j,je,jk2)
        go to 100
   70   jk1 = jk2 - 1
        yy = 0.
        if (jk1.gt.0) yy = z(jk1,k)
        rto = ste(j,je,jk2) * (esd-yy) / (z(jk2,k)-yy)
        if (rto.gt.xx) go to 90
        xx = xx - rto
        ste(j,je,jk2) = ste(j,je,jk2) - rto
   80   es = es - xx
        et = et - xx
        go to 100
   90   ste(j,je,jk2) = ste(j,je,jk2) - xx
      else
        snoa(j,je) = snoa(j,je) - es        
         snoev = es 
      end if
  100 continue
      
      if(ievap.eq.1.and.j.eq.ievsb.and.je.eq.ievhd)
     *  write (45,101) ida,eo,es,ep, 
     *        cva(j,je),tx_tmp,ra(j),ho,thrad,alai(j,je)

  101 format (i4,3f8.3,2x,7f8.2)        
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
