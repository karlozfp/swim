C     FILE pcycle.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine pcycle(j,je,k,n) 	hydrotop
C     subroutine psollch(j,je,k,n) 	hydrotop
C     subroutine puptake(j,je,k,n) 	crpmd(),veget()



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine pcycle(j,je,k,n)
C**** PURPOSE: THIS SUBROUTINE COMPUTES P FLUX BETWEEN  
C              THE LABILE, ACTIVE MINERAL AND STABLE MINERAL P POOLS 
C**** CALLED IN:   HYDROTOP  
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      ano3(j,je,l)   = nitrate (NO3-N) content in a layer, kg/ha 
C      anora(j,je,l)  = active org. N content in a layer, kg/ha
C      anors(j,je,l)  = stable org. N content in a layer, kg/ha
C      csf            = combined water/temperature factor
C      flu(j)         = fraction of subbasin area in the basin
C      fon(j,je,l)    = fresh organic N from residue in a layer, kg/ha
C      fop(j,je,l)    = fresh organic P from residue in a layer, kg/ha
C      frar(j,je)     = fractional areas of hydrotope in subbasin
C      humn           = N mineralization from humus, kg/ha
C      nn             = number of soil layers, from subbasin
C      plab(j,je,l)   = labile P content in a layer, kg/ha
C      pma(j,je,l)    = active mineral P content, kg/ha
C      pms(j,je,l)    = stable mineral P content, kg/ha
C      porg(j,je,l)   = organic P content, kg/ha
C      psp            = phosphorus availability index
C      rsd(j,je,2)    = crop residue, kg/ha
C      salpf          = SUM(alpflow): flow between active and labile P pool 
C                       for basin, kg/ha
C      saspf          = SUM(aspflow): flow between active and stable P pool 
C                       for basin, kg/ha
C      sfomp          = SUM(fomp): fresh organic P mineralisation for basin, 
C                       kg/ha
C      shump          = SUM(hump): humus P-mineralization for basin, kg/ha
C      yphe(j)	      = org. P loss with erosion, kg/ha
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      alpflow  = flow between active and labile P pool
C      aspflow  = flow between active and stable P pool
C      bk       = rate constant set to 0.0006
C      ca	= minimum of CN and CP-ratio factor
C      cnr	= CN-ratio
C      cnrf	= CN-ratio factor
C      cpr	= CP-ratio
C      cprf	= CP-ratio factor
C      decr	= decompostion rate for residue
C      fomp     = mineralization from fresh org. P pool
C      hump	= humus P-mineralization
C      l        = layer
C      r4	= CN- or CP-ratio
C      resdc	= decompostion rate for fresh org. material
C      rto	= equilibrium constant (normally set to 1) 
C      xx	= locale variable
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,n,k,l
      real(8) alpflow,aspflow,bk,ca,cnr,cnrf,cpr,cprf,decr
      real(8) fomp,hump,r4,resdc,rto,xx

      data bk /.0006/
      
C**** SUBTRACT org P lost with erosion
      porg(j,je,1) = porg(j,je,1) - yphe(j)
      if (porg(j,je,1).le.0.) porg(j,je,1) = 0.
      yphe(j) = 0.      

C*********************************************************** START OF CYCLE 10  
      do 10 l = 1, nn

C****   CALC humus mineralization for P: hump
        xx = anora(j,je,l) + anors(j,je,l)
        if(xx.gt.0.) then
          hump = 1.4 * humn * porg(j,je,l) / xx
        else
          hump = 0.
        endif

C****   RECALC pools 
        porg(j,je,l) = porg(j,je,l) - hump
        plab(j,je,l) = plab(j,je,l) + hump

C****   CALC fresh organic matter mineralization: fomp & RECALC pools
        if (l.le.2) then
          r4 = .58 * rsd(j,je,l)
          cnr = r4 / (fon(j,je,l)+ano3(j,je,l)+1.e-6)
          cpr = r4 / (fop(j,je,l)+plab(j,je,l)+1.e-6)
          if (cnr.gt.25.) then
            cnrf = exp(-.693*(cnr-25.)/25.)
          else
            cnrf = 1.
          end if
          if (cpr.gt.200.) then
            cprf = exp(-.693*(cpr-200.)/200.)
          else
            cprf = 1.
          end if
          ca = amin1(cnrf,cprf)
          decr = .05 * ca * csf
          fomp = decr * fop(j,je,l)
          resdc = decr * rsd(j,je,l)
          fop(j,je,l) = fop(j,je,l) - fomp
        end if
        
        if (l.ge.2) then
          fomp = 0.
        end if
                
        porg(j,je,l) = porg(j,je,l) - hump + .2 * fomp
        plab(j,je,l) = plab(j,je,l) + hump + .8 * fomp

   10 continue
C*********************************************************** END OF CYCLE 10  

      rto = psp / (1.-psp)
      do 30 l = 1, nn
        alpflow = (plab(j,je,l)-pma(j,je,l)*rto)
        if (alpflow.lt.0.) alpflow = alpflow * .1
        aspflow = bk * (4.*pma(j,je,l)-pms(j,je,l))
        if (aspflow.lt.0.) aspflow = aspflow * .1
        pms(j,je,l) = pms(j,je,l) + aspflow
        pma(j,je,l) = pma(j,je,l) - aspflow + alpflow
        plab(j,je,l) = plab(j,je,l) - alpflow

	shump = shump + hump * flu(j) * frar(j,je)
	sfomp = sfomp + fomp * flu(j) * frar(j,je)
	saspf = saspf + aspflow * flu(j) * frar(j,je)
	salpf = salpf + alpflow * flu(j) * frar(j,je)
   30 continue
      return
      end
      


C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




      subroutine psollch(j,je,k,n)
C**** PURPOSE: COMPUTES soluble P loss with surface runoff 
C**** CALLED IN:   HYDROTOP 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      bd(k)        = bulk density of the upper soil layer, g/cm3
C      plab(j,je,1) = P labile in the I soil layer, kg/ha
C                     plab(j,je,1) * (10. / bd(k)), g/t
C      qd           = surface runoff, mm
C      ysp          = soluble P leaching, kg/ha
C      >>>>>

C      >>>>> STATIC PARAMETERS 
C      dkd          = P conc in sediment divided by that of water in m3/t
C      xx           = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,n,k
      real(8) dkd,xx

      data dkd /175./
      ysp = .01 * plab(j,je,1) * (10. / bd(k)) * qd / dkd
      xx = plab(j,je,1) - ysp
      if (xx.le.0.) then        
        ysp = 0.
      endif
      plab(j,je,1) =  plab(j,je,1) - ysp 
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




      subroutine puptake(j,je,nv)
C**** PURPOSE: CALCULATES P UPTAKE by PLANTS, calls npstress
C**** CALLED IN:   GROWTH  
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      bp1(nv)      = normal fraction of P in plant biomass at emergence
C      bp3(nv)      = normal fraction of P in plant biomass at maturity
C      bpu1(nv)     = used to calculate sp1 - S-curve parameter
C      bpu2(nv)     = used to calculate sp2 - S-curve parameter
C      dm(j,je)     = total biomass, kg/ha
C      flu(j)       = fraction of subbasin area in the basin
C      frar(j,je)   = fractional areas of hydrotope in subbasin
C      g(j,je)      = fraction of heat units to maturity accumulated
C      ida          = current day
C      nn           = number of soil layers, from subbasin
C      plab(j,je,l) = P content in soil layer,  recalc here, kg/ha 
C      sbpup        = weighted average P uptake in the basin, kg/ha
C      spup(j,je)   = P uptake in hydrotop, kg/ha
C      strsp        = P stress factor for plants
C      uap          = P uptake by plants for a given day (SUPPLY), kg/ha
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      cpt    = optimal conc N in biomass
C      l      = local par
C      sp1    = local par
C      sp2    = local par
C      uapot  = optimal N uptake by the crop until given day (DEMAND) 
C      uu     = P stress
C      xx     = local par
C      yy     = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,l,nv
      real(8) cpt,sp1,sp2,uapot,uu,xx,yy

C**** INITIALIZATION
      xx = g(j,je)
      sp1 = bpu1(nv)
      sp2 = bpu2(nv)      

C**** CALC P uptake by plants & RECALC pools          
C#### CALL nutrient stress
      cpt = bp3(nv) + 
     * (bp1(nv)-bp3(nv))*(1-xx/(xx+exp(sp1-sp2*xx)))
      uap = cpt * dm(j,je) - spup(j,je)
      if (uap.ge.0) then
        if (ida.eq.1) uap = 0.
        uapot = uap
        xx = uap
        do 10 l = 1, nn
          yy = plab(j,je,l) - xx
          if (yy.gt.0.) then
            plab(j,je,l) = yy
            go to 20
          else
            xx = xx - plab(j,je,l)
            plab(j,je,l) = 0.
          end if
   10   continue
        uap = uap - xx
   20   if (uap.lt.0.) uap = 0.
        call npstress(uap,uapot,uu)
        strsp = uu
        spup(j,je) = spup(j,je) + uap
        sbpup = sbpup + uap * flu(j) * frar(j,je)
      else
        uap = 0.
        strsp = 1.
      end if

      return
      end

 
 
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

