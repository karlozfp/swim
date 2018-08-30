C     FILE curn.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine  curno(cnn,j,je,k,n)	hydrotop
C     subroutine  volq(j,je,k,n)	hydrotop
C     subroutine  peakq(j)		subbasin
C     subroutine  tran(j)		subbasin		 
C     subroutine  alpha(j)		subbasin



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine curno(cnn,j,je,k,n)
C**** PURPOSE: TO SET CURVE NUMBER PARAMETERS 
C**** CALLED IN:   HYDROTOP 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      cnn          = Curve Number = cn2(k,n), from readsol, with title
C      cnum1        = init. CN for cropland, cond 1
C      cnum3        = init. CN for cropland, cond 3
C      hsumfc(j,je) = sum of field capacity in soil, calc in subbasin, mm
C      hsumul(j,je) = sum of upper limit water content in soil, 
C                     calc in subbasin, mm 
C      icn          = switch code for CN: 0 - CN dif for soils, 
C                                         1 - CN=const from cnum1, cnum3
C      icurn        = switch code to print from curn() 
C      icursb       = number of subbasin to print from curn(), if icurn = 1
C      ida          = current day
C      smx(j,je)    = retention factor, corresponding cn1
C      wf(2,j,je)   = shape parameters for calc. of retention
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      c2    = local par
C      cn1   = CN, conditions 1
C      cn3   = CN, conditions 3
C      s3    = local par
C      yy    = local par
C      zz    = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer, intent(in) :: j,je,n,k
      real(8), intent(in) :: cnn ! cnn = cn2(k,n)
      real(8)             :: c2, cn1, cn3, s3, yy, zz

C**** CALC cn1, cn3
      if (icn.eq.0) then
        c2 = cnn * cnn
        cn1 = -16.911 +1.3481 *cnn -.013793 *c2 +.00011772* c2 * cnn
        cn3 = cnn * exp(.006729*(100.-cnn))         
      else
        cn1 = cnum1         
        cn3 = cnum3
      endif
            
C**** CALC retention factor smx()
      smx(j,je) = 254. * (100./cn1-1.)
      s3 = 254. * (100./cn3-1.)

C**** CALC shape parameters for CN method wf(1,) and wf(2,)
      yy=hsumfc(j,je)/(1.-s3/smx(j,je))-hsumfc(j,je)
      if (yy.ne.0.) then
        zz=dlog(yy)
        wf(2,j,je) = (zz-dlog(hsumul(j,je)/(1.-2.54/smx(j,je)) 
     *    -hsumul(j,je))) / (hsumul(j,je)-hsumfc(j,je))
        wf(1,j,je) = zz + wf(2,j,je) * hsumfc(j,je)
      else
        wf(2,j,je) = 0.
        wf(1,j,je) = 0.
      endif

      if (icurn.eq.1.and.j.eq.icursb) then
      write (41,100) ida,j,je,cnn,cn1,cn3,wf(1,j,je),wf(2,j,je)
     *     ,smx(j,je)
      endif
  100 format(3i5,3f5.0,3f10.3)
  101 format('CN = ',3i5,12f10.3)
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine volq(j,je,k,n)
C**** PURPOSE: THIS SUBROUTINE COMPUTES DAILY RUNOFF GIVEN DAILY PRECIPITATION
C     AND SNOW MELT USING A MODIFIED SCS CURVE NUMBER APPROACH 
C**** CALLED IN:  HYDROTOP 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      alai(j,je)    = Leaf Area Index (LAI) 
C      blai(icr)     = max LAI for crop
C      canmax(n)     = canopy maximum storage for interception, mm, calc in init
C      canstor(j,je) = canopy water storage, mm
C      cn            = Curve Number, current
C      icurn         = switch code to print from curn() 
C      icursb        = number of subbasin to print from curn(), if icurn = 1
C      ida           = current day
C      igro(j,je)    = vegetation index, =1 if vegetation is growing
C      nn            = number of soil layers, calc in subbasin, cycle 100
C      nucr(j,je)    = crop number (database)
C      precip        = precipitation, mm, read in readcli
C      preinf(j,je)  = precipitation adjusted for canopy storage, mm
C      qd            = daily surface runoff, mm
C      smx(j,je)     = retention coef,  calc in curno
C      ste(j,je,l)   = water storage in a layer, mm, calc in hydrotop & purk
C      te(j,je,l)    = soil temperature, degree C, calc in solt
C      wf(2,j,je)    = shape parameters eq.6, calc in curno
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      bb      = local par
C      canmxl  = local par
C      l       = local par
C      pb      = local par
C      r2      = local par
C      sum     = soil water content in all layers
C      xx      = local par
C      xx1     = local par
C      xx3     = local par
C      xx4     = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,n,k,l
      real(8) bb,canmxl, pb,r2,sum,xx,xx1,xx3,xx4

      sum = 0.
      do 10 l = 1, nn
        sum = sum + ste(j,je,l)
   10 continue

C**** Canopy interception
      xx1 = 0.
      canmxl = 0.
      xx1 = preinf(j,je)

C**** CALC canopy storage
      if (igro(j,je).ge.1) then
        canmxl = canmax(n) * alai(j,je) / blai(nucr(j,je))
      else
        canmxl = 0.
      endif
      xx3 = preinf(j,je) - canmxl
      if (xx3 < 0.) then
        canstor(j,je) = xx1
      else
        canstor(j,je) = canmxl
      endif

      preinf(j,je) = preinf(j,je) - canstor(j,je)
      xx4 = preinf(j,je) - canstor(j,je)

      xx = wf(1,j,je) - wf(2,j,je) * sum
      if (xx.lt.-20.) xx = -20.
      if (xx.gt.20.)  xx = 20.

      r2 = smx(j,je) * (1.-sum/(sum+exp(xx)))
      if (te(j,je,2).le.0.) r2 = smx(j,je) * (1.-exp(-.000862*r2))
      cn = 25400. / (r2+254.)
      r2 = 25400. / cn - 254.
      bb = .2 * r2
      pb = xx4 - bb

C**** CALC daily surface runoff qd
      if (pb.gt.0.) then
        qd = pb * pb / (xx4+.8*r2)
      else
        qd=0.  
      end if

      if (icurn.eq.1.and.j.eq.icursb) then
        write (41,101) ida,j,je,nn,cn,sum,smx(j,je),r2,qd
      endif
            
  100 format(3i5,f5.0,4f10.3)        
  101 format('volq =',4i5,10f10.3)        
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine peakq(j)
C**** PURPOSE: THIS SUBROUTINE COMPUTES THE PEAK RUNOFF RATE 
C     USING A MODIFICATION OF THE RATIONAL FORMULA 
C**** CALLED IN:   SUBBASIN 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      al(j)  = fun(tc,tp5,tp6,flu,da), calc in readsub
C      pr     = peak runoff rate, m3/sec.
C      r1     = alpha for rainfall, the fraction of total rainfall 
C               occuring during 0.5h
C      tc(j)  = time of concentration, hours, calc in readsub
C      xqd    = surface runoff, mm, calc in volq
C      >>>>> 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j

      pr = r1 * al(j) * xqd / tc(j)        
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine tran(j)
C**** PURPOSE: THIS SUBROUTINE COMPUTES CHANNEL TRANSMISSION LOSSES
C              and recalculates surface runoff xqd and peak runoff rate pr 
C**** CALLED IN:  SUBBASIN  
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      chk(2,j) = effective hydraulic conductivity of main channel, mm/h
C      chl(2,j) = main channel length, km
C      chw(2,j) = average width of main channel, m
C      dur      = flow duration, h
C      ida      = current day
C      itran    = switch code to print from tran()
C      iv       = 1 in subbasin (reach 1 as outlet)
C      pr       = peak runoff rate, m3/sec.
C      q1       = remember old xqd, mm, to compare with new one in subbasin
C      vo       = runoff volume = xqd * da * flu(j) * 1000, m3, 
C                 calc in subbasin
C      xqd      = surface runoff, mm, recalculation
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      a    = local par
C      ak   = local par
C      axw  = local par
C      b    = local par
C      bxw  = local par
C      pr1  = local par
C      pxw  = local par
C      vol  = local par
C      xx   = local par
C      yy   = local par
C      zz   = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j
      real(8) a,ak,axw,b,bxw,pr1,pxw,vol,xx,yy,zz
      
C      q1 = xqd
      yy = vo / (xqd*1000.)
      a = -.0001831 * chk(iv,j) * dur
      xx = .2649 * chk(iv,j) * dur / vo

C**** RECALC xqd, pr
C     iv = 1 (from subbasin)
      if (xx.lt.1.) then
        ak = -1.09 * dlog(1.-xx)
        b = exp(-ak)
        if ((1.-b).le.1.e-20) go to 10
        zz = -2.04 * ak * chw(iv,j) * chl(iv,j)
        if (zz.ge.-30.) then
          bxw = exp(zz)
          axw = (a/(1.-b)) * (1.-bxw)
          pxw = (-axw/bxw)
          vol = vo / 1233.5
          xqd = 0.
          if (vol.gt.pxw) xqd = axw + bxw * vol
          xqd = xqd * 1.234 / yy
          pr1 = 35.3 * pr
          pr = 12.1 * axw / dur - (1.-bxw) * vol + bxw * pr1
          pr = pr / 35.3
          if (pr.lt.0.) pr = 0.
          go to 10
        end if
      end if

      xqd = 0.
   10 continue
      if(itran.eq.1) write (43,102) ida,j,pr,q1,xqd

  102 format('TRAN  ', 2i5,3f10.3)       
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine alpha(j)
C**** PURPOSE: THIS SUBROUTINE COMPUTES PRECIP ALPHA FACTOR:
C              ALPHA is A DIMENSIONLESS PARAMETER THAT EXPRESSES  
C              THE FRACTION OF TOTAL RAINFALL THAT OCCURS DURING 0.5 H 
C readwet: read wim() = monthly max .5h rain for period of record (mm)
C readwet: calc wi()  = f(wim) = to be used in alpha() 
C                       for estim of precip. alpha factor    
C alpha:   calc r1    = alpha for rainfall, the fraction of total rainfall 
C                       occuring in 0.5h
C peakq:   calc pr    = peak runoff rate using r1
C**** CALLED IN:   SUBBASIN 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
Cblock ab      = .02083
Cblock k7      = parameters for random number generator
C      mo      = current month
C      precip  = precipitation in subbasin, mm
C      r1      = alpha for rainfall, the fraction of total rainfall 
C                occuring during 0.5h
C      rn1     = random value, calc in readsub
C      rp      = alpha for rainfall, the fraction of total rainfall 
C                occuring during 0.5h
C      sml     = snow melt, calc in snom(), mm
C      wi(m,j) = f(wim),wim = monthly max .5h rain for period of record, mm
C      >>>>>

C      >>>>> STATIC PARAMETERS 
C      aii    = local parameter
C      ajp    = local parameter
C      ei     = local parameter
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j
      real(8) aii,ajp,ei
      real(8) gammad

      ei = precip - sml
      ! sl begin
      ei=MAX(ei,0.)
      ! sl end
      aii = (1. - ab) / (wi(mo,j)-ab)
      if (ei.ge.25.) then
        ajp = 1. - exp(-125./ei)
      else
        ajp = 1.
      end if
      r1 = gammad(rn1,aii,k7)
      r1 = (ei*(ab+r1*(ajp-ab))+sml*ab) / precip
      ! sl begin
      if ( r1 >= 1. ) r1 = 0.99999
      ! sl end
      rp = r1
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

