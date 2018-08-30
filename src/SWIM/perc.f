C     FILE perc.f
C
C     SUBROUTINES IN THIS FILE             CALLED FROM
C     subroutine purk(j,je,k)		   hydrotop
C     subroutine perc(j,je,k,j1,j2)	   purk



      subroutine purk(j,je,k)
C**** PURPOSE: THIS SUBROUTINE DIVIDES EACH LAYER'S FLOW INTO 4 MM SLUGS
C              and CALLS PERC to CALC PERCOLATION and LATERAL SUBSURFACE FLOW
C**** CALLED IN:  HYDROTOP 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      fc(l,k)       = field capacity, mm, calc readsol
C      flate(j,je,l) = lateral subsurface flow, sum of prk for layer, mm
C      ida           = current day
C      ipehd         = number of hydrotope to print from perc(), if iperc = 1
C      iperc         = switch code to print from perc()
C      ipesb         = number of subbasin to print from perc(), if iperc = 1
C      nn            = number of soil layers, calc in subbasin
C      poe(j,je,l)   = percolation, sum of sep for layer, mm
C      prk           = lateral subsurface flow, calc in perc for 4mm slugs, mm
C      rain          = preinf(j,je)-qd, mm
C      sep           = percolation, calc in perc for 4mm slugs, recalc here, mm
C      ste(j,je,l)   = water storage in a layer, recalc here, mm
C      su            = water excess above FC, mm
C      >>>>>

C      >>>>> STATIC PARAMETERS 
C      add   = local par
C      amt   = local par
C      j1    = local par
C      j2    = local par
C      l     = local par
C      n1    = local par
C      n2    = local par
C      n3    = local par
C      sum   = local par
C      tot   = local par
C      vvv   = local par
C      vvvs  = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,k,j1,j2,l,n1,n2,n3
      real(8) add,amt,sum,tot,vvv
      dimension vvv(11)
      data amt /4./

      n1 = 1
      n3 = 1

C**** Calc vvv()
      n2 = 0
      prk = 0.
      do 10 l = 1, nn
        flate(j,je,l) = 0.
        poe(j,je,l) = 0.
   10 continue
      vvv(1) = ste(j,je,1) - fc(1,k)
      if (vvv(1).ge.0.) then
        ste(j,je,1) = fc(1,k)
      else
        vvv(1) = 0.
      end if
      vvv(1) = vvv(1) + rain
      if (vvv(1).gt.0.) then
        n1 = 1
        n2 = 1
      end if
      do 20 l = 2, nn
        vvv(l) = ste(j,je,l) - fc(l,k)
        if (vvv(l).gt.0.) then
          ste(j,je,l) = fc(l,k)
          if (n2.gt.0) go to 20
          n2 = 1
          n1 = l
        else
          vvv(l) = 0.
        end if
   20 continue
      vvv(nn+1) = 0.
      tot = 0.
      add = 0.
     
C#### CALL PERC to calculate percolation and subsurface flow
   30 if (n2.ne.0) then
        n2 = 0
        do 50 j1 = n1, nn
          sum = 0.
          j2 = j1 + 1
          if (j1.eq.nn) j2 = nn
          if (vvv(j1).gt.0.) then
            if (n2.le.0) then
                n2 = 1
                n3 = j1
            end if
            su = vvv(j1)
            if (su.gt.amt) su = amt
            ste(j,je,j1) = ste(j,je,j1) + su
            sep = 0.
            ste(j,je,j1) = ste(j,je,j1) - sep       
            sum = sep
            if (su.ne.sep) then
              if (ste(j,je,j1).gt.fc(j1,k)) then
                call perc(j,je,k,j1,j2)
                ste(j,je,j1) = ste(j,je,j1) - sep - prk
                sum = sum + sep
                add = add + prk
                flate(j,je,j1) = flate(j,je,j1) + prk
                poe(j,je,j1) = poe(j,je,j1) + sep
              endif
            end if
            vvv(j1) = vvv(j1) - su
            j2 = j1 + 1
            vvv(j2) = vvv(j2) + sum
          end if
   50   continue
        tot = tot + sum
        n1 = n3
        go to 30
      end if

      sep = tot
      prk = add
             
      if(iperc.eq.1.and.j.eq.ipesb.and.je.eq.ipehd) then
         write (44,101) ida,flate(j,je,1),flate(j,je,2),
     *       flate(j,je,3),flate(j,je,4),flate(j,je,5),prk
         write (44,103) flate(j,je,1)+flate(j,je,2)+
     *       flate(j,je,3)+flate(j,je,4)+flate(j,je,5)
         write (44,102) ida,poe(j,je,1),poe(j,je,2),
     *       poe(j,je,3),poe(j,je,4),poe(j,je,5),sep
      endif
  101 format(i4,' ssf  =   ',(5f7.3),f12.3)
  102 format(i4,'      =   ',(5f7.3),f18.3)
  103 format(44x,f12.3)                    
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




      subroutine perc(j,je,k,j1,j2)      
C**** PURPOSE: THIS SUBROUTINE COMPUTES PERCOLATION AND LATERAL SUBSURFACE
C              FLOW FROM A SOIL LAYER WHEN FIELD CAPACITY IS EXCEEDED.
C**** METHOD:  SWRRB
C**** CALLED IN:  PURK
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
Cinp   j1 - layer, from purk
Cinp   j2 - layer, from purk
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      hk(l,k)       = beta coef. to estimate hydr. cond., used in perc
C      hrtc(j,je,l)  = return flow travel time, calc. hrflowtt, h
C      hwss(2,j,je)  = fun(field cap), calc in subbasin from hsumfc(j,jea)
C      ida           = current day
C      ipehd         = number of hydrotope to print from perc(), if iperc = 1
C      iperc         = switch code to print from perc()
C      ipesb         = number of subbasin to print from perc(), if iperc = 1
C      prk           = lateral subsurface flow, calc in perc for 4mm slugs, mm
C      sc(l,k)       = saturated conductivity, mm/h, calc, if isc = 1
C      sep           = percolation, calc in perc for 4mm slugs, recalc here, mm
C      ste(j,je,l)   = water storage in a layer, recalc here, mm
C      su            = water excess above FC
C      swe(j,je)     = soil water content, mm
C      te(j,je,l)    = daily average temp at the bottom of each layer, degree C
C      ul(l,k)       = upper limit water content in layer, mm 
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      adjf    = local par
C      cr      = local par
C      fx      = local par
C      iconfig = local par
C      rtf     = local par
C      rtw     = local par
C      stu     = local par
C      stu1    = local par
C      stu2    = local par
C      stz     = local par
C      sup     = local par
C      sup1    = local par
C      xx      = local par
C      zz      = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer j,je,k,j1,j2
      real(8) cr,fx,rtf,rtw,stu,stu1,stu2,stz,sup,sup1,xx,zz

C**** IF TEMP OF LAYER IS BELOW 0 C - NO WATER FLOW
      if (te(j,je,j1).le.0.) then
        sep = 0.
        prk = 0.
        go to 30
      end if

      sup = su - sep
      sup1 = sup

C**** CALC SEEPAGE TO NEXT LAYER
      xx = hwss(1,j,je) - hwss(2,j,je) * swe(j,je)
      if (xx.lt.-20.) xx = -20.
      rtw = 10. * (1.-swe(j,je)/(swe(j,je)+exp(xx)))
      rtf = hrtc(j,je,j1) * rtw
      if (rtf.gt.0.) rtf = 1. - exp(-1./rtf)
      stz = ste(j,je,j1) / ul(j1,k)
      if (stz.ge.1.) then
        fx = 1.
      else
        fx = stz ** hk(j1,k)
      end if
      stu = ste(j,je,j2) / ul(j2,k)
      stu1 =  ul(j1,k) / ste(j,je,j1)
      stu2 = ste(j,je,j1) / ul(j1,k)
      
      if (stu.ge.1.) then
        sep = 0.
        prk = rtf * sup
      else
        cr = sqrt(1.-stu)
C SL ***
        if ( bSubcatch ) then
           zz =  24. * fx * cr * sc(j1,k) * sccor(j) / sup
        else
           zz =  24. * fx * cr * sc(j1,k) / sup
        end if
C SL ***
        if (zz.gt.10.) then
          sep = sup
          prk = 0.
        else
          sep = sup * (1.-exp(-zz))
          sup = sup - sep
          prk = rtf * sup
        endif
      end if      
   30 continue

C**** Write      
      if(iperc.eq.1.and.j.eq.ipesb.and.je.eq.ipehd) then
         if ( sup1 <= 0. ) sup1 = .000001
         if ( sep  <= 0. ) sep  = .000001       
         write (44,100) ida,j1,sup1,prk,sep,(prk+sep)/sup1,prk/sep,rtf,
     *   hrtc(j,je,j1), rtw
      end if

  100 format(2i4,3f8.3,10x,5f8.3)
      return
      end

