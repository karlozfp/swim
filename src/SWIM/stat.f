C     FILE stat.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine xnash(qo,qs,inn,icd)	main
C     function   gammad(rn1,ai,k7)	alpha(j)
C     function   randn(k)		gammad,init,readsub
C     subroutine xmonth			main



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


 
      subroutine xnash(qo,qs,inn,icd)
C**** PURPOSE: THIS SUBROUTINE COMPUTES CRITERIA OF FIT: 
C              difference in calulated water balance, relative difference**2, 
C              Nash&Sutcliffe Efficiency & LOG-Nash&Sutcliffe Efficiency
C**** CALLED IN:  MAIN 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> PARAMETERS & VARIABLES in TITLE
C      qo(1:inn) = observed water discharge, m3/sec.
C      qs(1:inn) = simulated water discharge, m3/sec.
C      inn       = number of days
C      icd       = code: 1 - one year, 2 - whole period

C      >>>>> COMMON PARAMETERS
C      istyr     = starting year
C      iy        = current year
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      akk(20) = accumulated values, internal
C      crdif   = difference in calulated water balance
C      crdifp  = difference in calulated water balance, %
C      dif     = difference between qs and qo
C      eff     = Nash & Sutcliffe Efficiency for variables qo and qs
C      efflog  = Nash & Sutcliffe Efficiency for log(qo) and log(qs)
C      f00     = local par
C      f11     = local par
C      ik      = local par
C      im      = local par
C      qmid    = local par
C      qmid2   = local par
C      reldif2 = relative difference **2
C      xdif    = xqo - xqs
C      xqo     = log(qo)
C      xqs     = log(qs)
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters & descriptions
      use common_par
      implicit NONE
      integer inn,icd,ik,im
      real(8) qo(inn), qs(inn), akk(20),crdif,crdifp,dif,eff
      real(8) efflog,f00,f11,qmid,qmid2,reldif2,xdif,xqo,xqs
      
      do 10 im=1,20
        akk(im) = 0.
   10 continue

      do 20 ik=1,inn
        dif = qs(ik) - qo (ik)
        qmid = 0.5 * (abs(qo(ik)) + qs(ik))
        qmid2 = qmid * qmid
        akk(1) = akk(1) + 1.
        akk(2) = akk(2) + qo(ik)
        akk(3) = akk(3) + qo(ik)*qo(ik) 
        akk(4) = akk(4) + dif
        akk(5) = akk(5) + dif*dif
        if(qmid2.gt.0.001) then
          akk(6) = akk(6) + dif*dif/qmid2
        else
        endif 
        
        if (qo(ik).gt.0..and.qs(ik).gt.0.) then
          xqo  = log(qo(ik))
          xqs  = log(qs(ik))
          xdif = xqo - xqs 
          akk(7) = akk(7) + 1.
          akk(8) = akk(8) + xqo 
          akk(9) = akk(9) + xqo*xqo 
          akk(10) = akk(10) + xdif*xdif
        endif
   20 continue
      
      crdif = akk(4)

      if (akk(2).ne.0) then
        crdifp = akk(4) * 100/akk(2)
      else
        crdifp = 0.
      endif
      
      reldif2 = akk(6)
      
      if (akk(1).gt.0.) then
        f00 = akk(3) - akk(2)*akk(2)/akk(1)
        if(f00.gt.0.0001) eff = (f00-akk(5))/f00
      else
        eff = 0.
      endif      
     
      if (akk(7).gt.0.) then
        f11 = akk(9) - akk(8)*akk(8)/akk(7)
        if(f11.gt.0.0001) efflog = (f11-akk(10))/f11
      else
        efflog = 0.
      endif
      
      if (icd.eq.1) then
      if ( bRunoffDat ) then
         write(6,201) istyr+iy-1, crdif,crdifp,reldif2,efflog,eff
         write(80,201) istyr+iy-1, crdif,crdifp,reldif2,efflog,eff
      end if
      else
      if ( bRunoffDat ) then
         write(6,202) crdif,crdifp,reldif2,efflog,eff
         write(80,202) crdif,crdifp,reldif2,efflog,eff
      end if
      endif            
      
  201 format(10x,'CRITERIA of fit,      yr =',i5,/,
     *10x,'Difference',f10.2,f10.0,'%',/,
     *10x,'Rel.dif**2',f10.2,/,10x,'L-Efficiency',f8.2,/,
     *10x,'Efficiency',f10.2,/)         

  202 format(10x,'CRITERIA of fit,          TOTAL',/,
     *10x,'Difference',f10.2,f10.0,'%',/,
     *10x,'Rel.dif**2',f10.2,/,
     *10x,'L-Efficiency',f8.2,/,10x,'Efficiency',f10.2,/)                 
        
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      function gammad(rn1,ai,k7)
C**** PURPOSE: THIS FUNCTION PROVIDES NUMBERS rn1 FROM A GAMMA DISTRIBUTION, 
C              GIVEN TWO RANDOM NUMBERS ai & k7 
C**** CALLED IN:  ALPHA 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> STATIC PARAMETERS
C      fu  = local par
C      rn  = local par
C      xn1 = local par
C      xx  = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      implicit NONE
      integer k7
      real(8) rn1,ai, fu,rn,xn1,xx,randn,gammad

      dimension k7(4)
      data xn1 /10./

   10 gammad = rn1
      xx = rn1 * ai
      rn = randn(k7)
      fu = xx ** xn1 * exp(xn1*(1.-xx))
      rn1 = rn
      if (fu.lt.rn) go to 10
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      function randn(k)
C**** PURPOSE: THIS FUNCTION PROVIDES RANDOM NUMBERS RANGING FROM 0. TO 1. 
C**** CALLED IN:  GAMMAD, INIT, READSUB 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> STATIC
C      i = local par
C      >>>>>
Ccalc randn
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      implicit NONE
      integer i,k
      real(8) randn
      
      dimension k(4)
      k(4) = 3 * k(4) + k(2)
      k(3) = 3 * k(3) + k(1)
      k(2) = 3 * k(2)
      k(1) = 3 * k(1)
      i = k(1) / 1000
      k(1) = k(1) - i * 1000
      k(2) = k(2) + i
      i = k(2) / 100
      k(2) = k(2) - 100 * i
      k(3) = k(3) + i
      i = k(3) / 1000
      k(3) = k(3) - i * 1000
      k(4) = k(4) + i
      i = k(4) / 100
      k(4) = k(4) - 100 * i
      randn = (((float(k(1))*.001+float(k(2)))*.01+float(k(3)))*.001+
     *    float(k(4))) * .01
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine xmonth
C**** PURPOSE: THIS SUBROUTINE DETERMINES THE MONTH, GIVEN THE DAY OF THE YEAR 
C**** CALLED IN:   MAIN  
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS
C      ida   = current day
C      mo    = current month
Cblock nc(m) = number of julian days passed to the beginning of month
C      nt    = 0/1 if (mod(iyr,4).eq.0)/not
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      m1  = local par
C      nda = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer m1,nda

      if (ida.gt.nc(2)) then
        do 10 mo = 2, 12
          m1 = mo + 1
          nda = nc(m1) - nt
          if (ida.le.nda) go to 20
   10   continue
      end if
      mo = 1
   20 return
      end


C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
