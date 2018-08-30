C     FILE eros.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine ecklsp(j,je,k,n)	hydrotop
C     subroutine ysed(j)		subbasin
C     subroutine enrsb(j)		subbasin
C     subroutine orgnsed(j)		subbasin
C     subroutine psed(j)		subbasin



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine ecklsp(j,je,k,n)
C**** PURPOSE: THIS SUBROUTINE ESTIMATES COMBINED CKLSP factor 
C     FOR WATER EROSION  
C**** CALLED IN:  HYDROTOP 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      cklsp(j,je) = combined c,k,ls,p factor
C      cva(j,je)   = land cover, kg/ha, calc in crpmd
C      cvm(icr)    = minimum value of C factor for water erosion, readcrp
C      dm(j,je)    = total biomass, kg/ha
C      ecp(j)      = P factor, readsub
C      ek(k)       = USLE soil K factor, read in readsol
C      ida         = current day
C      ieros       = switch code to print from eros()
C      iersb       = number of subbasin to print from eros(), if ieros = 1
C      igro(j,je)  = vegetation index for cropland, = 1 if planted
C      nucr(j,je)  = crop number
C      sl(j)       = USLE slope length/slope steepness factor  
C      >>>>>

C      >>>>> STATIC PARAMETER
C      c        = C (land cover) factor
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,n,k
      real(8) c

      c = 0.
      if(n.eq.6.or.n.eq.7) 		c = 0.10
      if(n.eq.9) 			c = 0.002
      if(n.eq.8.or.n.eq.10.or.n.eq.11) 	c = 0.45
      if(n.eq.5) then
        if (igro(j,je).eq.1) then
         c = exp((-.2231-cvm(nucr(j,je)))*
     *       exp(-.00115*cva(j,je))+cvm(nucr(j,je)))
        else
         c = 0.8
        endif
      endif           

C**** CALC combined c,k,ls,p factor
      cklsp(j,je) = c * ek(k) * ecp(j) * sl(j)

      if (ieros.eq.1.and.j.eq.iersb) then
        write (47,101) ida,je,n,igro(j,je),
     *      dm(j,je),cva(j,je),c,ek(k),sl(j),cklsp(j,je)
      endif 

  101 format(4i5,6f8.3)
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine ysed(j)
C**** PURPOSE: THIS SUBROUTINE ESTIMATES DAILY SOIL LOSS CAUSED BY WATER  
C              EROSION USING THE MODIFIED UNIVERSAL SOIL LOSS EQUATION 
C**** CALLED IN:  SUBBASIN  
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      aff    = 1000. * da * flu(j), calc in subbasin
C      ida    = current day
C      ieros  = switch code to print from eros()
C      iersb  = number of subbasin to print from eros(), if ieros = 1
C      pr     = peak runoff rate, m3/sec., calc in peakq
C      precip = precipitation, mm
C      sl(j)  = USLE slope length/slope steepness factor  
C      xcklsp = combined c,k,ls,p factor for subbasin, calc in subbasin
C      xqd    = surface runoff for subbasin, mm, calc in subbasin
C      yd     = daily soil loss from subbasin caused by water erosion, t
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j
      
      yd = 11.8 * (xqd*pr*aff) ** .56 * xcklsp

      if (ieros.eq.1.and.j.eq.iersb) then
        write (47,101) ida,j,precip,xqd,pr,aff,sl(j),xcklsp,yd
      endif  

  101 format(2i5,f10.1,2f8.3,f15.0,2f10.3,f10.0)    
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine enrsb(j)
C**** PURPOSE: THIS SUBROUTINE CALCULATES ENRICHMENT RATIO FOR SUBBASIN 
C**** CALLED IN:  SUBBASIN 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      da         = area of the basin, km2
C      da9        = 100. * da = basin area in ha, from readbas
C      er         = enrichment ratio for subbasin
C      flu(j)     = fraction of subbasin area in the basin
C      parsz(5,j) = particle size distribution, calc in subbasin
C      pct(5,j)   = delivery ratios (part. size distr. for sediments)
C      pr         = peak runoff rate, m3/sec., in peakq
C      precip     = precipitation, mm
C      rp         = alpha for rainfall, the fraction of total rainfall 
C                   occuring during 0.5h
C      xqd        = daily surface runoff, mm, calc in subbasin
C      yd         = daily soil loss, t, calc in ysed
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      bet  = local par
C      cy   = local par
C      dia  = local par
C      dr   = local par
C      durf = local par
C      jj   = local par
C      nsz  = local par
C      rep  = local par
C      rinf = local par
C      rp1  = local par
C      tot  = local par
C      x1   = local par
C      x2   = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j, jj, nsz
      real(8) bet,cy,dia,dr,durf,rep,rinf,rp1,tot,x1,x2

      dimension dia(5)
      data nsz /5/, dia /14.14, 3.16, 1.41, 5.48, 22.36/            
      rp1 = -2. * precip * dlog(1.-rp)
      durf = 4.605 * precip / rp1
      rinf = (precip-xqd) / durf
      rep = (rp1-rinf) * da * flu(j) / 3.6
      dr = (pr/(rep+1.e-10)) ** .56
      bet = dlog(dr) / 4.47
      tot = 0.

      do 10 jj = 1, nsz
        pct(jj,j) = parsz(jj,j) * exp(bet*dia(jj))
        tot = tot + pct(jj,j)
   10 continue

      do 20 jj = 1, nsz
        pct(jj,j) = pct(jj,j) / tot
   20 continue

      cy = 100000. * yd / (da9*xqd+1.e-6)
      x2 = -log10(dr) / 2.699
      x1 = 1. / .25 ** x2

C**** CALC ENRICHMENT RATIO FOR SUBBASIN
      if (cy > 1.e-6) then
        er = .78 * cy ** (-.2468)
      else
        er = 0.
      endif
      if (er.lt.1.) er = 1.
      if (er.gt.3.5) er = 3.5

      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine orgnsed(j)
C**** PURPOSE:  COMPUTES organic N loss with erosion
C**** CALLED IN:   SUBBASIN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      conn    = xnorg * er, g/t
C      da9     = 100. * da = basin area in ha, from readbas
C      er      = enrichment ratio, from enrsb
C      xnorg   = N org. in I layer for subbasin, g/t
C      xnorgp  = N org. in I layer for subbasin, kg/ha
C      yd      = daily soil loss (erosion), in t, calc in ysed
C      yon     = org N loss with erosion, kg/ha
C      yone(j) = org N loss with erosion, kg/ha
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j

C**** CALC org N loss with erosion
      conn = xnorg * er
      yon = .001 * conn * yd / da9

C**** Correction: AnjaH
ccc      if (yon.lt.xnorgp) yon = 0.
      if (yon.gt.xnorgp) yon = xnorgp
      if (yon.le.0) yon = 0.

      yone(j) = yon 
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine psed(j)
C**** PURPOSE: COMPUTES P loss with erosion 
C**** CALLED IN:  SUBBASIN 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      cpp     = xporg * er, g/t
C      da9     = 100. * da = basin area in ha, from readbas
C      er      = enrichment ration, from enrsb
C      xporg   = P org. in I layer in subbasin, g/t
C      xpsedp  = SUM(porg+pms+pma) in subbasin, kg/ha
C      yd      = daily soil loss, in t, calc in ysed
C      yph     = P org. loss with erosion, kg/ha
C      yphe(j) = P org. loss with erosion, kg/ha
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j

C**** CALC P org. loss with erosion
      cpp = xporg * er
      yph = .001 * cpp * yd / da9

C**** Correction: AnjaH
ccc      if (yph.lt.xporgp) yph = 0.
      if (yph.gt.xpsedp) yph = xpsedp
      if (yph.le.0) yph = 0.
      yphe(j) = yph
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




