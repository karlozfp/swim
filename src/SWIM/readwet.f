C     File readwet.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine  readwet(i)		readsub



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine readwet(i)
C**** PURPOSE: THIS SUBROUTINE READS MONTHLY STATISTICAL WEATHER PARAMETERS
C              for the basin from wgen.dat 
C**** CALLED IN:  readsub 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      ATTN: Input parameters read from wgen.dat are BASIN parameters;
C            Dimension (j) is optional, if subbasin statistics will be availbale
C      amp(j)     = annual amplitude in daily aver. temperature, degree C 
C      avt(j)     = average annual air temp, degree C, used in solt()
Cblock clt        = parameter = 57.296
C      daylmn(j)  = min day length, h 
C      ffc(j)     = field capacity, m/m (not used)
Cblock nc(m)      = number of days passed in the beginning of month
C      obmn(m,j)  = average monthly min temp, degree C
C      obmx(m,j)  = average monthly max temp, degree C
C      prw(1,m,j) = monthly probability of wet day after dry day
C      prw(2,m,j) = monthly probability of wet day after wet day
C      r(8)       = vector for output in readwet
C      rsm(m)     = monthly max .5h rain for period of record, mm, 
C                   smoothed wim()
C      rsmm(m)    = monthly number of rainy days
C      rsmy(m)    = monthly rainfall, mm 
C      rst(m,j)   = monthly mean event of daily rainfall, mm 
C      tpnyr(j)   = number of years of record max .5h rainfall
C      tp5(j)     = 10 year frequency of .5h rainfall (mm)
C      tp6(j)     = 10 year frequency of .6h rainfall (mm)
C      wft(m,j)   = monthly prob. of rainy day
C      wi(m,j)    = f(wim), used in alpha() for estim of precip. alpha factor 
C      wim(m)     = monthly max .5h rain for period of record, mm
C      ylc(j)     = cos(lat()/clt), lat() - lat, clt=57.296, for rmx in evap
C      yls(j)     = sin(lat()/clt), lat() - lat, clt=57.296, for rmx in evap
C                   (convert degrees to radians (2pi/360=1/57.296) )
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      i        = subbasin number (IN TITLE)       
C      ch       = interm. parameter
C      f        = interm. parameter
C      h        = interm. parameter
C      ii       = cycle parameter
C      j        = cycle parameter
C      mon      = cycle parameter
C      r25      = interm. parameter
C      tas      = interm. parameter to calc amp()
C      tav      = interm. parameter, monthly mean temp
C      tbb      = interm. parameter to calc amp()
C      titldum  = text
C      xm       = interm. parameter
C      xx       = interm. parameter
C      xy2      = interm. parameter
C      ytn      = interm. parameter
C      >>>>>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer i,ii,j,mon
      real(8) ch,f,h,r25,tas,tav,tbb,xm,xx,xy2,ytn
      character*10 titldum 
      real(8) clt
      data clt /57.296/
      real(8) :: r5,r6,tpn

C**** READ unit 11 - weather generator data wgen.dat
      if (i == 1) then
         read(11,99)   titldum
         read(11,*)  r5, r6, tpn ! these parameters are actually not subbasin individual
         tp5   = r5
         tp6   = r6
         tpnyr = tpn
         read(11,*) (obmx(mon), mon=1,12) 
         read(11,*) (obmn(mon), mon=1,12)
         read(11,*)  wim
         read(11,*) (prw(1,mon),mon = 1,12)
         read(11,*) (prw(2,mon),mon = 1,12)
         read(11,*) (rst(mon),mon = 1,12)
         close(11)
      end if

C**** CALCULATION of WEATHER GENERATOR PARAMETERS, step 1
C     yls(), ylc() - used in evap
C     daylmn - used in veget
      xx = lat(i) / clt
      yls(i) = sin(xx)
      ylc(i) = cos(xx)
      ytn = tan(xx)
      ch = .439 * abs(ytn)
      if (ch.ge.1.) then
        h = 0.
      else
        h = acos(ch)
      endif
      daylmn(i) = 7.72 * h


! SL
!! calculate day length threshold for dormancy
! used in vegmd in subroutine veget.f90
      if ( bDormancy ) then
         dormhr(i) = 0.
         if (Abs(lat(i)) > 40.) then
            dormhr(i) = 1.
         else if (Abs(lat(i)) < 20.) then
            dormhr(i) = -.1
         else
            dormhr(i) = (Abs(lat(i)) - 20.) / 20.
         end if
      else
         dormhr(i) = -1.
      end if
      
      
C**** CALCULATION of WEATHER GENERATOR PARAMETERS, step 2
C     wft() - used in solt, wi() - used in alpha & peakq
      do 18 ii = 1, 12
        r(ii) = 0.
   18 continue       

      rsm(1) = (wim(12)+wim(1)+wim(2)) / 3.

      do 19 mon = 2, 11
        rsm(mon) = (wim(mon-1)+wim(mon)+wim(mon+1)) / 3.
   19 continue 

      rsm(12) = (wim(11)+wim(12)+wim(1)) / 3.
      if(i.eq.1) write (31,101)
      tbb = 0.
      tas = 100.

      do 20 mon = 1, 12
        xm = nc(mon+1) - nc(mon)
        rsmm(mon) = xm * prw(1,mon) / 
     *             (1.-prw(2,mon)+prw(1,mon))
        if(rsmm(mon).le.0.) rsmm(mon) = .001
        wft(mon,i) = rsmm(mon) / xm
        r25 = rst(mon)
        rsmy(mon) = rsmm(mon) * r25
        xy2 = .5 / tpnyr(i)
        f = xy2 / rsmm(mon)
        wi(mon,i) = -rsm(mon) / dlog(f)
        wi(mon,i) = 1. - exp(-wi(mon,i)/r25)
        if (wi(mon,i).lt..1) wi(mon,i) = .1
        if (wi(mon,i).gt..95) wi(mon,i) = .95
        r(1) = r(1) + obmx(mon)
        r(2) = r(2) + obmn(mon)
        r(8) = r(8) + rsmy(mon)

C****   WRITE monthly WGEN parameters
        if(i.eq.1) then     
        write (31,102) mon,wim(mon),obmx(mon),obmn(mon), 
     *    rsmy(mon), rsmm(mon), wi(mon,i)
        write (31,*) 'wft(,) = ', wft(mon,i)
        endif

        tav = (obmx(mon)+obmn(mon)) / 2.
        if (tav.gt.tbb) tbb = tav
        if (tav.lt.tas) tas = tav
   20 continue

C**** CALC average annual WGEN parameters
      do 21 j = 1, 2
        r(j) = r(j) / 12.
   21 continue      

C**** CALCULATION of WEATHER GENERATOR PARAMETERS, step 3
C     avt(), amp() - used in solt
      avt(i) = (r(1)+r(2)) / 2.
      amp(i) = (tbb-tas) / 2.
      xx = r(8)
      ffc(i) = xx / (xx+exp(9.043-.002135*xx))

   99 format (a) 
  101 format (//,12x,'R5MX',5x,' TMX',5x,' TMN',5x,
     *    'RAIN',5x,'DAYP',5x,'ALPH')
  102 format (5x,i3,8f9.2)
  103 format (5f8.3,i4)
  104 format (12f6.3)
      return

      end      


 
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
