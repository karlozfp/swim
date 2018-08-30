C     FILE ncycle.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine  ncycle(j,je,k,n)	hydrotop
C     subroutine  nlch(j,je,k,n)	hydrotop
C     subroutine  nuptake(j,je,k,n)	crpmd(), veget()
C     subroutine  fert(j,je,n,k,ii)     hydrotop



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine ncycle(j,je,k,n)
C**** PURPOSE: THIS SUBROUTINE ESTIMATES DAILY N AND P MINERALIZATION &
C     IMMOBILIZATION CONSIDERING FRESH ORGANIC MATERIAL (CROP RESIDUE)
C     AND ACTIVE AND STABLE HUMUS MATERIAL. 
C**** CALLED IN:   HYDROTOP 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      ano3(j,je,l)   = nitrate (NO3-N) content in a layer, kg/ha 
C      anora(j,je,l)  = active org. N content in a layer, kg/ha
C      anors(j,je,l)  = stable org. N content in a layer, kg/ha
C      cbn(l,k)       = organic carbon content in a layer, %
C      csf            = combined water/temperature factor
C      dflow(j,je,20) = monthly flows for water and N (see writhru.f)
C      flu(j)         = fraction of subbasin area in the basin
C      fon(j,je,l)    = fresh organic N from residue in a layer, kg/ha
C      fop(j,je,l)    = fresh organic P from residue in a layer, kg/ha
C      frar(j,je)     = fractional areas of hydrotope in subbasin
C      humn           = N mineralization from humus mineralization, kg/ha
C      ida            = current day
C      inuhd          = number of hydrotope to print from ncycle(), if inutr=1
C      inusb          = number of subbasin to print from ncycle(), if inutr=1
C      inutr          = switch code to print from ncycle()
C      nn             = number of soil layers, from subbasin
C      plab(j,je,l)   = labile P content in a layer, kg/ha
C      qd             = daily surface runoff, mm
C      rsd(j,je,2)    = crop residue, kg/ha
C      rtn            = active N pool fraction, = 0.15
C      sasnf          = SUM(asnflow): flow between active and stable org. N 
C                       for basin, kg/ha
C      sdnit          = SUM(denit): daily N-NO3 loss by denitrification 
C                       for basin, kg/ha
C      sfomn          = SUM(fomn): mineralisation from fresh org. N for basin, 
C                       kg/ha
C      shumn          = SUM(humn): humus N minerlisation for basin, kg/ha
C      ste(j,je,l)    = water storage in a layer, mm, recalc here
C      te(j,je,l)     = daily ave temp at the bottom of each layer, degree C
C      ul(l,k)        = upper limit water content in layer, mm 
C      xnflow(1:17)   = N flows for a choosen hydrotope to write in nutr.prn
C      xnflow(1)      = N loss with surface flow	calc nlch, kg/ha
C      xnflow(2)      = N loss with subsurface flow 	calc nlch, kg/ha
C      xnflow(3)      = N loss with percolation 	calc nlch, kg/ha
C      xnflow(4)      = N concentration in layer 2	calc nlch, kg/ha
C      xnflow(5)      = N input with precip		calc nlch, kg/ha
C      xnflow(6)      = N loss from layer 2		calc nlch, kg/ha
C      xnflow(7)      = N fertilization		        calc fert, kg/ha
C      xnflow(8)      = N uptake by plants		calc nuptake, kg/ha
C      xnflow(9)      = N denit				calc ncycle, kg/ha
C      xnflow(10)     = N miner from fresh org N	calc ncycle, kg/ha
C      xnflow(11)     = N miner from humus		calc ncycle, kg/ha
C      xnflow(12)     = xhumcdg/xhumn			calc ncycle 
C      xnflow(13)     = xhumsut/xhumn			calc ncycle 
C      xnflow(14)     = xhumcsf/xhumn			calc ncycle
C      xnflow(15)     = xfomcdg 			calc ncycle
C      xnflow(16)     = xfomsut 			calc ncycle
C      xnflow(17)     = xfomcsf 			calc ncycle
C      yone(j)        = org. N lost with erosion (calc in orgnsed), kg/ha
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      asnflow   = flow between active and stable org. N Pools, kg/ha
C      ca        = minimum of CN and CP-ratio factor
C      cdg       = temperature factor for humus mineralization
C      cdn       = shape coefficient for combined temp-carbon factor
C      cmn       = humus rate constant for N (normally set to 0.0003)
C      cnr       = CN-ratio
C      cnrf      = CN-ratio facto
C      cpr       = CP-ratio
C      cprf      = CP-ratio factor
C      decr      = decompostion rate for residue
C      denit     = daily N-NO3 loss by denitrification
C      deth      = threshold of soil water content for denitrification
C      fomn      = mineralisation from fresh org. N, kg/ha
C      ik        = local par
C      l         = local par
C      ll        = local par
C      nraz      = local par
C      r4        = CN- or CP-ratio 
C      resdc     = decompostion rate for fresh org. material
C      sut       = soil water factor for humus mineralization
C      sut4      = local par
C      xden      = accumulated denit
C      xfomcdg   = cdg (temperature factor)
C      xfomcsf   = csf (combined water/temperature factor)
C      xfomn     = accumulated fomn
C      xfomsut   = sut (water factor)
C      xhumcdg   = cdg*humn (temperature factor*humn)
C      xhumcsf   = csf*humn (combined water/temperature factor*humn)
C      xhumn     = accumulated humn
C      xhumsut   = sut*humn (water factor*humn)
C      xx        = local par
C      xx1       = local par
C      xx2       = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,n,k,ik,l,ll,nraz
      real(8) asnflow,ca,cdg,cdn,cmn,cnr,cnrf,cpr,cprf,decr,denit
      real(8) deth,fomn,r4,resdc,sut,sut4,xden,xx,xx1,xx2,xfomcdg
      real(8) xfomcsf,xfomn,xfomsut,xhumcdg,xhumcsf,xhumn,xhumsut

      data cdn / -1.4/, cmn /.0003/
      
C**** INITIALIZATION
      xx1 = 0.
      xx2 = 0.
      xden = 0.
      xfomn = 0.
      xhumn = 0.
      xhumcdg = 0.
      xhumsut = 0.
      xhumcsf = 0.
      xfomcdg = 0.
      xfomsut = 0.
      xfomcsf = 0.
      nraz = 0
             
C**** SUBTRACT org N lost with erosion
      anors(j,je,1) = anors(j,je,1) - yone(j)
      if (anors(j,je,1).le.0.) anors(j,je,1) = 0.
      yone(j) = 0. 
           
C*********************************************************** START OF CYCLE 10  
      do 10 l = 1, nn

C****   CALC soil water factor for humus mineralization: sut
        ll = l
        if (l.eq.1) ll = 2
        xx = te(j,je,ll)
        sut = ste(j,je,ll) / (ul(ll,k)+1.e-10)
        if (sut.gt.1.) sut = 1.
        if (sut.lt.0.) sut=1.e-10
        sut4 = .06 * exp(3.*sut)
        
C****   CALC temperature factor for humus mineralization: cdg 
        if (xx.gt.0.) then
          cdg = xx / (xx+exp(6.82-.232*xx)+1.e-6)
        else
          cdg = 0.
        endif

C****   CALC combined water & temperature factor: csf    
        csf = sqrt(cdg*sut)      
         
C****   CALC daily NO3-N loss by denitrification: denit, xden
        deth = 0.9
        if (sut.ge.deth) then
          denit = sut4*ano3(j,je,l) * (1.-exp(cdn*cdg*cbn(l,k)))
        else
          denit = 0.
        end if
        ano3(j,je,l) = ano3(j,je,l) - denit
        xden = xden + denit
        
C****   CALC asnflow - N flow between act. & stab. org N; RECALC pools
        asnflow = .1e-4 * (anora(j,je,l)*(1./rtn-1.)-anors(j,je,l))          
        anors(j,je,l) = anors(j,je,l) + asnflow
        anora(j,je,l) = anora(j,je,l) - asnflow

C****   CALC humus mineralization: humn; RECALC pools
        humn = cmn * csf * anora(j,je,l)
        xx = anora(j,je,l) + anors(j,je,l)        
        xhumcdg = xhumcdg + cdg*humn
        xhumsut = xhumsut + sut*humn
        xhumcsf = xhumcsf + csf*humn
        if (humn.gt.0.) nraz=nraz + 1
        anora(j,je,l) = anora(j,je,l) - humn 
        ano3(j,je,l) = ano3(j,je,l) + humn
        xhumn = xhumn + humn
        
C****   CALC mineralization of fresh organic matter: fomn; RECALC pools
C       CALC residue decomposition (only here, not in pcycle!)
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

          fomn = decr * fon(j,je,l)
          resdc = decr * rsd(j,je,l)

          rsd(j,je,l) = rsd(j,je,l) - resdc
          fon(j,je,l) = fon(j,je,l) - fomn
          xfomcdg = cdg
          xfomsut = sut
          xfomcsf = csf
        else
          fomn = 0.
        end if
                
        anora(j,je,l) = anora(j,je,l) + .2*fomn   
        ano3(j,je,l) = ano3(j,je,l) + .8*fomn  
        xfomn = xfomn + .8*fomn
 
C****   CALC SUMS for basin
        shumn = shumn + humn * flu(j) * frar(j,je)
        sasnf = sasnf + asnflow * flu(j) * frar(j,je)
        sfomn = sfomn + fomn * flu(j) * frar(j,je)
        sdnit = sdnit + denit * flu(j) * frar(j,je)      

   10 continue
C*********************************************************** END OF CYCLE 10  
   
C**** CALC N flows for a choosen hydrotope (output in ncycle)
      if(inutr.eq.1.and.j.eq.inusb.and.je.eq.inuhd) then
      xnflow(9) = xden
      xnflow(10) = xfomn
      xnflow(11) = xhumn
c      if (xhumn.gt.0) then
c        xnflow(12) = xhumcdg/xhumn
c        xnflow(13) = xhumsut/xhumn
c        xnflow(14) = xhumcsf/xhumn
c      else
c        xnflow(12) = 0.
c        xnflow(13) = 0.
c        xnflow(14) = 0.
c      endif        
c      xnflow(15) = xfomcdg 
c      xnflow(16) = xfomsut 
c      xnflow(17) = xfomcsf                   
      write (48,100) ida,(xnflow(ik),ik=1,11),(ano3(j,je,l),l=1,5),
     * qd        
      endif

C**** CALC monthly flows for selected HRUs (output in writhru.f)      
      dflow(j,je,16) = dflow(j,je,16)+ xfomn 
      dflow(j,je,17) = dflow(j,je,17)+ xhumn
      dflow(j,je,9) = dflow(j,je,9) + xden 
            
  100 format(i4,25f9.3,4f12.3,f5.0)
  101 format (3i5,10f8.3)
  102 format (5i5,f10.3)     
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine nlch(j,je,k,n)
C**** PURPOSE: THIS SUBROUTINE CALCULATES NITRATE LEACHING 
C**** CALLED IN:   HYDROTOP 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      ano3(j,je,l)   = nitrate (NO3-N) content in a layer, kg/ha 
C      anora(j,je,l)  = active org. N content in a layer, kg/ha
C      anors(j,je,l)  = stable org. N content in a layer, kg/ha
C      dflow(j,je,20) = monthly flows for water and N (see writhru.f)
C      flate(j,je,l)  = subsurface flow, mm, from purk
C      inuhd          = number of hydrotope to print from ncycle(), if inutr=1
C      inusb          = number of subbasin to print from ncycle(), if inutr=1
C      nn             = number of soil layers, from subbasin
C      percn          = N leaching to g-w, kg/ha 
C      poe(j,je,l)    = percolation, mm, from purk
C      precip         = precipitation, mm
C      qd             = daily surface runoff, mm
C      ssfn           = N loss with subsurface flow, kg/ha 
C      ul(l,k)        = upper limit water content in layer, mm 
C      xnflow(1:17)   = N flow for a choosen hydrotope to write in nutr.prn 
C                      (see ncycle)
C      yno3           = N loss with surface flow, kg/ha
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      co      = average daily concentration of N-NO3 in the layer, kg/ha
C      l       = local par
C      qip     = input with precip, then - input in a layer from the layer above
C      rcn     = local par
C      sro     = local variable: surface runoff
C      vno3    = amount of N-NO3 lost from the layer
C      vv      = total amount of water lost from the soil layer
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,n,k,l
      real(8) co,qip,rcn,sro,vno3,vv

      data rcn /1./
       
C**** INITIALIZATION
      qip = .01 * rcn * preinf(j,je)
      if(j.eq.inusb.and.je.eq.inuhd) xnflow(5) = qip      
      sro = qd
      ssfn = 0.
      xnflow(6) = 0.
      
C**** CALC nitrate loss in surface and subsurface runoff: yno3, ssfn
C     RECALC ano3() 
      do 10 l = 1, nn
        vv = poe(j,je,l) + sro + flate(j,je,l) + 1.e-10
        ano3(j,je,l) = ano3(j,je,l) + qip
        vno3 = ano3(j,je,l) * (1.-exp(-vv/ul(l,k)))
        co = vno3 / vv        
        if (l.eq.1.) yno3 = qd * co        
        ano3(j,je,l) = ano3(j,je,l) - vno3
        qip = co * poe(j,je,l)
        sro = 0.        
        ssfn = ssfn + co * flate(j,je,l)
   10 continue

C**** CALC nitrate leaching into ground water: percn
      percn = qip

C**** CALC N flows for a choosen hydrotope (output in ncycle)
      if(j.eq.inusb.and.je.eq.inuhd) then
        xnflow(1) = yno3
        xnflow(2) = ssfn
        xnflow(3) = percn
      endif
        if(j.eq.inusb.and.je.eq.inuhd.and.l.eq.2) 
     *      xnflow(4) = co 
        if(j.eq.inusb.and.je.eq.inuhd.and.l.eq.2) 
     *      xnflow(6) = xnflow(6) + vno3 - qip
      
C**** CALC monthly flows for selected HRUs (output in writhru.f)      
      dflow(j,je,5) = dflow(j,je,5)+yno3 
      dflow(j,je,6) = dflow(j,je,6)+ssfn  
      dflow(j,je,7) = dflow(j,je,7)+percn  
      dflow(j,je,10) = dflow(j,je,10)+ano3(j,je,1)+ 
     *    ano3(j,je,2)+ano3(j,je,3)+ano3(j,je,4)+ano3(j,je,5)  
      dflow(j,je,11) = dflow(j,je,11)+ anora(j,je,1) + anora(j,je,2)
     *  + anora(j,je,3)+ anora(j,je,4)+ anora(j,je,5)
      dflow(j,je,12) = dflow(j,je,12)+ anors(j,je,1) + anors(j,je,2)
     *  + anors(j,je,3)+ anors(j,je,4)+ anors(j,je,5)  

c      dflow(j,je,11) = dflow(j,je,11)+ ano3(j,je,1) 
c      dflow(j,je,12) = dflow(j,je,12)+ ano3(j,je,2) 
      dflow(j,je,13) = dflow(j,je,13)+ ano3(j,je,3) 
      dflow(j,je,14) = dflow(j,je,14)+ ano3(j,je,4) 
      dflow(j,je,15) = dflow(j,je,15)+ ano3(j,je,5) 
       
      return
      end
     


C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine nuptake(j,je,nv)
C**** PURPOSE: CALCULATES N UPTAKE by PLANTS, calls npstress()
C**** CALLED IN:  GROWTH 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      ano3(j,je,l)   = nitrate (NO3-N) content in a layer , kg/ha
C      bn1(nv)        = normal fraction of N in plant biomass at emergence
C      bn3(nv)        = normal fraction of N in plant biomass at maturity
C      bnu1(nv)       = coef used to calculate sp1 - S-curve parameter
C      bnu2(nv)       = coef used to calculate sp2 - S-curve parameter 
C      cnb            = optimal conc N in biomass, kg/kg
C      dflow(j,je,20) = monthly flows for water and N (see writhru.f)
C      dm(j,je)       = total biomass, kg/ha
C      flu(j)         = fraction of subbasin area in the basin
C      frar(j,je)     = fractional areas of hydrotope in subbasin
C      g(j,je)        = fraction of heat units to maturity accumulated
C      ida            = current day
C      inuhd          = number of hydrotope to print from ncycle(), if inutr=1
C      inusb          = number of subbasin to print from ncycle(), if inutr=1
C      nn             = number of soil layers, from subbasin
C      sbnup          = sum of N uptake for basin
C      snup(j,je)     = N uptake accumulated, kg/ha
C      strsn          = N stress factor
C      uno3           = N uptake by the crop for a given day, kg/ha (SUPPLY)
C      xnflow(1:17)   = N flows for a choosen hydrotope to write in nutr.prn 
C                      (see ncycle)
C      >>>>>

C      >>>>> STATIC PARAMETERS 
C      l       = local par
C      sp1     = local par
C      sp2     = local par
C      uno3pot = optimal N uptake by the crop until given day. kg/ha (DEMAND) 
C      uu      = nutrient stress
C      xx      = local par
C      yy      = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,nv,l
      real(8) sp1,sp2,uno3pot,uu,xx,yy

C**** INITIALIZATION
      xx = g(j,je)
      sp1 = bnu1(nv)
      sp2 = bnu2(nv)      

C**** CALC N uptake by plants & RECALC pools          
C#### CALL nutrient stress
      cnb = bn3(nv) + 
     * (bn1(nv)-bn3(nv))*(1-xx/(xx+exp(sp1-sp2*xx)))
      uno3 = cnb * dm(j,je) - snup(j,je)
      if (uno3.ge.0) then
        if (ida.eq.1) uno3 = 0.
        uno3pot = uno3
        xx = uno3
        do 10 l = 1, nn
          yy = ano3(j,je,l) - xx
          if (yy.gt.0.) then
            ano3(j,je,l) = yy
            go to 20
          else
            xx = xx - ano3(j,je,l)
            ano3(j,je,l) = 0.
          end if
   10   continue
        uno3 = uno3 - xx        
   20   if (uno3.lt.0.) uno3 = 0.
        snup(j,je) = snup(j,je) + uno3
        sbnup = sbnup + uno3 * flu(j) * frar(j,je)
        call npstress(uno3,uno3pot,uu)
        strsn = uu
      else
        uno3 = 0.
        strsn = 1.
      end if

C**** CALC N flows for a choosen hydrotope (output in ncycle)
      if(j.eq.inusb.and.je.eq.inuhd) xnflow(8) = uno3

C**** CALC monthly flows for selected HRUs (output in writhru.f)      
      dflow(j,je,8) = dflow(j,je,8)+uno3 
 
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine fert(j,je,n,k,ii)
C**** PURPOSE: this subroutine applies N and P fertilizers as
C              specified by date and amount in initcrop() 
C**** CALLED IN:   HYDROTOP 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      ano3(j,je,l)   = nitrate (NO3-N) content in a layer 
C      anora(j,je,l)  = active org. N content in a layer
C      dflow(j,je,20) = monthly flows for water and N (see writhru.f)
C      fen(n,if)      = amount of min N fertilizers applied, kg N/ha
C      feno(n,if)     = amount of org N fertilizers applied, kg N/ha
C      fep(n,if)      = amount of P fertilizers applied, kg P/ha
C      ida            = current day
C      inuhd          = number of hydrotope to print from ncycle(), if inutr=1
C      inusb          = number of subbasin to print from ncycle(), if inutr=1
C      plab(j,je,l)   = labile P content in a layer, kg/ha
C      xnflow(7)      = NO3-N fertilization in a choosen hydrotope, kg N/ha
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,n,k,ii

C**** ADD FERTILIZERS      
      ano3(j,je,2) = ano3(j,je,2) + fen(n,ii)
      anora(j,je,2) = anora(j,je,2) + feno(n,ii)
      plab(j,je,1) = plab(j,je,1) + fep(n,ii)
      
C**** CALC N flows for a choosen hydrotope (output in ncycle)
      if(j.eq.inusb.and.je.eq.inuhd) xnflow(7) = fen(n,ii)

C**** CALC monthly flows for selected HRUs (output in writhru.f)      
      dflow(j,je,20) = dflow(j,je,20) + fen(n,ii) + feno(n,ii) 
      
      if(j.eq.inusb.and.je.eq.inuhd.and.(fen(n,ii)+feno(n,ii))>1.)
     *     write (6,100)  ida, fen(n,ii),feno(n,ii)
      
  100 format('Fertilization day', i4,'  amount = ',f5.0,' + ',f5.0)   
      return
      end

 
 
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

