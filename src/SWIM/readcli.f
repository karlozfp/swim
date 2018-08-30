C     FILE readcli.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine readcli		main



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine readcli
C**** PURPOSE: this subroutine reads climate data from clim1.dat and clim2.dat
C              & hydrological data from runoff.dat
C**** CALLED IN:  MAIN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C      PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      daylen(j) = day length in subbasin
C      humi(j)   = air humidity in the subbasin, %
C      ida       = current day in the year (Julian day)
C      mb        = number of subbasins
Cblock pit       = parameter for estimation of the day length
C      precip    = precipitation in the current subbasin, mm 
C      ra(j)     = radiation, J/cm^2
C      runo(ida) = water discharge in the basin outlet, m3/sec
C      subp(j)   = daily precipitation in the subbasin, mm 
C      tmn(j)    = daily min temp in the subbasin, degree C
C      tmx(j)    = daily max temp in the subbasin, degree C
C      tx(j)     = daily aver temp in the subbasin, degree C
C      ylc(j)    = cos(lat()/clt), lat() - latitude, clt=57.296, 
C                  used to calc rmx in evap
C      yls(j)    = sin(lat()/clt), lat() - latitude, clt=57.296, 
C                  used to calc rmx in evap
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      ch       = parameter for estim. day length
C      h        = parameter for estim. day length
C      id       = day
C      iiyr     = year
C      imn      = month
C      k        = count parameter
C      sd       = parameter for estim. day length
C      xi       = day (real(8) number)
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters 
      use common_par
      implicit NONE
      integer id,iiyr,imn,k
      real(8) ch,h,sd,xi
      real(8),dimension(mb) :: mx,mn

      precip = 0.

C**** READ CLIMATE DATA & MEASURED DISCHARGE (current day)
      read(21,*) (ra(k),humi(k),subp(k),k=1,mb)
      read(22,*) (tmn(k),tmx(k),tx(k),k=1,mb)
      if ( bRunoffdat ) then
         read(25,*) iiyr,imn,id,(runo(ida,k), k=1,nqobs)
      else
         runo(ida,1) = -999.
      end if
      
      ! If relative humidity is not provided
      if ( isNaN(humi(1)) ) humi = -999.9
      if ( isNaN(ra(1)) ) ra = 1.

      ! In order to avoid errors if Tmax < Tmin the following conversion is done
      do k=1,mb
         mx(k) = max(tmx(k),tmn(k))
         mn(k) = min(tmx(k),tmn(k))
         tmx(k) = mx(k)
         tmn(k) = mn(k)
      end do

C**** CALC day length (from clgen)
      do 10 k = 1,mb
      xi = ida
      sd = .4102 * sin((xi-80.25)/pit)
      ch = -yls(k) * tan(sd) / ylc(k)
      if (ch.lt.1.) then
          if (ch.le.-1.) go to 110
          h = acos(ch)
          go to 120
      end if
      h = 0.
      go to 120
  110 h = 3.1416
  120 daylen(k) = 7.72 * h
   10 continue

      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



C-------------------------------------------------------------------------------
C Author  : aich@pik-potsdam.de
C Date    : 2013-08-04
C modified: 2013-08-04
C
C PURPOSE : generating radiation with latitude, tmin and tmax
C
C CALLED  : from program readcli
C-------------------------------------------------------------------------------

      subroutine hargreaves_rad
      use common_par
      implicit none
      integer :: id,iiyr,imn,k
      real(8) :: ch,h,sd,xi,gb,dr,ds,ws2,ram_1,radi,nstra,ek2

      gb  = 0.
      ! ek1 = empirical constant
      !ek1=0.16 ! used in West Africa (Niger Basin)
      !ek1 = 0.115 ! used in East Africa (Blue Nile Basin), old value
      !ec1 = 0.145 ! used in East Africa (Blue Nile Basin), for new SWIM trunk version
      ek2 = 0.  !empirische Konstante 1

      dr = 1 + (.033*cos(((2.*3.141592)/365.)*ida)) !Inverse relative Distanz Erde-Sonne
      ds = .409*sin(((2.*3.141592)/365.)*ida-1.39)! Deklination der Sonne
 
      do k=1,mb
         ! Note, if latitude is larger 67 degrees North, this function does not work!
         if ( lat(k) > 66.5 )  then
            lat(k) = 66.5
            if ( bErrorFile ) then
               write(1000,*) "Latitude is > 66.5, subbasin:",k,
     *         "set to 66.5"
            end if
         end if
         if ( lat(k) < -66.5 ) then
            lat(k) = -66.5
            if ( bErrorFile ) then
               write(1000,*) "Latitude is < -66.5, subbasin:",k,
     *         "set to <66.5"
            end if
         end if               
         gb    = (3.1415/180.)*lat(k)  !Breite in Bogenwinkel
         ws2   = acos((-tan(gb))*tan(ds)) !Stundenwinkel beim Sonnenuntergang
         ram_1 = (ws2*sin(gb)*sin(ds))+(cos(gb)*cos(ds)*sin(ws2))
         radi  = ((24.*60.)/3.141592)*.0820*dr*ram_1
         radi  = (radi*100.) ! Formel in MJ/m2/d   /*1000000 weil MJ, dann /10000 weil m2 in cm2 bedeutet -> /100
         ra(k) = ec1*radi*((tmx(k)-tmn(k))**.5)+ek2   ! Formel von Hargreaves(1985)
      end do

      end subroutine hargreaves_rad



