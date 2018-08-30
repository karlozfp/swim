C     FILE solt.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine  solt(zz,j,je,k,n)	hydrotop
C     subroutine  snom(j)		subbasin



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine solt(zz,j,je,k,n)
C**** PURPOSE: THIS SUBROUTINE ESTIMATES DAILY AVERAGE TEMPERATURE 
C              AT THE BOTTOM OF EACH SOIL LAYER 
C              =f(air temp, precip, residue, snow cover) 
C**** CALLED IN:  HYDROTOP 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      abd(k)     = sum of porosity for soil type, mm
C      amp(j)     = annual 1/2 amplitude in daily average temp, calc readwet, 
C                   degree C
C      avt(j)     = average annual air temperature, calc readwet, degree C
C      bcv(j,je)  = lag factor for residue and snow effects on surf.temp. 
C      ida        = current day 
C      isolt      = switch code to print from solt()
C      isosb      = number of subbasin to print from solt(), if isolt = 1
C      iy         = current year
C      mo         = current month
C      nn         = number of soil layers, calc in subbasin, cycle 100
Cblock pit        = 58.13
C      precip     = precipitation, mm, calc in readcli
C      swe(j,je)  = soil water, mm, calc in hydrotop (previous day)
C      te(j,je,l) = daily ave temp at the bottom of each layer, degree C
C      te0(j,je)  = bare soil surface temp, degree C
C      tmn(j)     = daily min temp. for subbasin, readcli, degree C 
C      tmx(j)     = daily max temp. for subbasin, readcli, degree C
C      tx(j)      = daily aver. temperature, readcli, degree C
C      wft(m,j)   = monthly prob. of rainy day, calc readwet
C      z(l,k)     = soil depth, read in readsol, mm
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      alx      = local par
C      b        = local par
C      dd       = local par
C      dp       = max damping depth for the soil
C      dt       = local par
C      f        = local par
C      l        = local par
C      st0      = bare soil surfac temp
C      ta       = local par
C      wc       = local par
C      ww       = local par
C      xi       = local par
C      xx       = local par
C      zd       = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      use mod_snow !#### SNOW MODULE       ####
      implicit NONE
      integer j,je,k,n,l
      real(8) zz, alx,b,dd,dp,dt,f,st0,ta,wc,ww,xi,xx,zd 

      !###########################
      !#### SNOW MODULE       ####
      !###########################
      if ( bSnowModule ) then
        tx_tmp   = tmit
        tmax_tmp = tmax
        tmin_tmp = tmin
      else
        tx_tmp = tx(j)
        tmax_tmp = tmx(j)
        tmin_tmp = tmn(j)
      end if
      !###########################

      xi = ida
      alx = (xi-200.) / pit
      f = abd(k) / (abd(k)+686.*exp(-5.63*abd(k)))
      dp = 1000. + 2500. * f
      ww = .356 - .144 * abd(k)
      b = dlog(500./dp)
      wc = swe(j,je) / (ww*z(nn,k))
      f = exp(b*((1.-wc)/(1.+wc))**2.)
      dd = f * dp

C**** CALC st0 - bare soil surfac temp
      if (preinf(j,je).le.0.) then
        st0 = wft(mo,j) * (tmax_tmp-tx_tmp) + tx_tmp
      else
        st0 = wft(mo,j) * (tx_tmp-tmin_tmp) + tmin_tmp
      end if

      ta = avt(j) + amp(j) * cos(alx)
      xx = bcv(j,je) * te0(j,je) + (1.-bcv(j,je)) * st0
      dt = xx - ta
      te0(j,je) = st0
      zz = 2. * dd
      xx = 0.  

C**** CALC te()
      do 10 l = 1, nn
        zd = -z(l,k) / dd
        te(j,je,l) = avt(j) + (amp(j)*cos(alx+zd)+dt) * exp(zd)            
   10 continue

      if(isolt.eq.1.and.j.eq.isosb.and.n.eq.5.and.k.eq.33) 
     *  write (42,100) iy, ida, tx_tmp,te(j,je,1),
     *         te(j,je,2),te(j,je,3),te(j,je,4) 
  100 format (2i5,20f10.2,8f10.1)    
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




      subroutine snom(j,jea)
C**** PURPOSE: THIS SUBROUTINE COMPUTES DAILY SNOW MELT  
C              WHEN AVERAGE AIR TEMP EXCEEDS 0 DEGREES C 
C**** CALLED IN:   HYDROTOPE 
C     snow melt rate in SWAT = 4.57: sml = 4.57 * tmx(j)
C     snow melt rate in HBV = 3.2
C     current version in SWIM: sml = 3.2 * tx(j
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      sml       = snow melt, mm
C      smrate    = snow melt rate for the degree-day method    
C      sno(j)    = water content of snow, mm
C      tmelt     = threshold temperature for snow melt
C      tmx(j)    = daily max temp, read in readcli, degree C
C      tx(j)     = daily average temp, read in readcli, degree C
C      >>>>>

C      >>>>> STATIC PARAMETERS 
C      smr       = snow melt rate, coef for the degree-day method, mm/degree
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
C     T.Vetter modification: smrate from .bsn file; tmelt instead of 0;
C     T.Vetter modification: sml = smr * (tmx(j)-tmelt)
      use common_par
      implicit NONE
      integer j, jea
      real(8) smr

      smr = smrate(j)
      sml = 0.
      !if (tmx(j).gt.tmelt) then
        sml = smr * (tmx(j)-tmelt(j))
        if (sml.gt.snoa(j,jea)) sml = snoa(j,jea)
        snoa(j,jea) = snoa(j,jea) - sml
      !end if
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
