!     FILE readsol.f
!
!     SUBROUTINES IN THIS FILE          CALLED FROM
!     subroutine readsol		main 		 



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


subroutine readsol
!**** PURPOSE: THIS SUBROUTINE READS SOIL DATA 
!**** CALLED IN:   MAIN 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!   
!      >>>>> COMMON PARAMETERS & VARIABLES
!      abd(k)     = sum of porosity for soil type, mm, used in solt 
!      alai0(n)   = initial Leaf Area Index for land use type
!      ap(l,k)    = labile (soluble) phosphorus, g/t, then trans. to kg/ha
!      asc(1:17)  = empirical coef. (estimated here) to calc sc(l,k)
!      awc(l,k)   = available water capacity, % , /100. to transfer to m/m
!      bd(k)      = bulk density of the upper (!) soil layer, g/cm3, 
!                   calc from porosity 
!      bden(l,k)  = bulk density, input, g/cm3 
!      bsc(1:17)  = empirical coef to calc sc(l,k)
!      cbn(l,k)   = organic carbon content, %
!      cla(l,k)   = clay content, %
!      clay(1,k)  = clay content, %
!      cn2(k,n,j) = Curve Numbers for soil k, land use n, and subbasin
!      cn2a(n)    = Curve Numbers for soil group A
!      cn2b(n)    = Curve Numbers for soil group B
!      cn2c(n)    = Curve Numbers for soil group C
!      cn2d(n)    = Curve Numbers for soil group D
!      cv(j)      = initial land cover, kg/ha
!      ek(k)      = soil erodibility factor
!      fc(l,k)    = field capacity, %, then trans. to mm    
!      hk(l,k)    = beta coef. to estimate hydr. cond., used in perc
!      hum(l,k)   = humus content, kg/ha
!      isc        = code for saturated cond.: 0 - read, 1 - calc
!      nn         = number of soil layers
!      ns(k)      = number of soil layers
!      nsa(k)     = number of layers for arable soils
!block nsolgr(k)  = number of soils group for soil type, 1=A,2=B,3=C,4=D
!      op(l,k)    = stable min. P, estimated from pmn(l,k), coef = 4., kg/ha
!      pmn(l,k)   = act. min. P, estimated from ap(l,k), psp=0.5, kg/ha
!      por(l,k)   = porosity, m/m (calc)
!      poros(l,k) = porosity, % (input)
!block psp        = parameter = 0.5
!      psz(5,k)   = particle size distribution
!block rtn        = parameter = .15
!      san(l,k)   = sand content, %
!      sand(1,k)  = sand content, %
!      sc(l,k)    = saturated conductivity, mm/h, calc, if isc = 1
!      sccor      = correction factor for saturated conductivity (all soils)
!      sil(l,k)   = silt content, %
!      silt(1,k)  = silt content, %
!      snam(k)    = soil name
!      stin(l,k)  = water content in soil layer, mm
!      stinco     = init. water content coef, from readbas
!      sumfc(k)   = sum of field capacity in soil, mm
!      sumul(k)   = sum of upper limit water content in soil, mm
!      swin(k)    = soil water content in soil profile, mm
!      ul(l,k)    = upper limit water content in layer, mm 
!      up(l,k)    = upper limit water content, m/m (used only here to calc fc)
!      wmn(l,k)   = active org. N, estim. from wn(l,k), rtn=.15, kg/ha
!      wn(l,k)    = stable organic N content, %, 
!                   recalc from % to g/t OR 
!                   estim. from C using C:N=10. and 10000. coef from % to g/t, 
!                   then trans. to kg/ha
!      wno3(l,k)  = NO3-N content (kg/ha), estimated in g/t, trans to kg/ha
!      wp(l,k)    = wilting point, in m/m (used here and in perc)
!      wpo(l,k)   = org. P, estimated from wn(), coef=.125, kg/ha
!      z(l,k)     = depth to bottom of layer, mm
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      ccf      = correction coef for up and por by silt content
!      dg       = layer thickness
!      i        = cycle par
!      j        = par     
!      jj       = par       
!      k        = soil type number
!      ka       = par      
!      l        = par      
!      n        = par
!      numsb    = actual number of soils in database
!      sfile    = soil file name      
!      tex      = sum of clay, silt and sand      
!      titldum  = text
!      wt1      = coef. to transform from g/t to kg/ha
!      xx       = par
!      xz       = par
!      zz       = par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
implicit NONE
   integer            :: k ! soil counter
   integer            :: l ! layer counter
   integer            :: jj,n
   integer            :: j
   integer            :: opensucc = 0
   character(len=100) :: titldum,snam
   character(len= 13) :: soildat
   real(8)               :: ccf,dg,tex,wt1,xx,xz,zz
   real(8), dimension(17):: asc

   write(6,*) 'SOIL PARAMETERS:'
   write(6,*) 'Number of soils in soil.cio = ',msdb

!*********************************************************** START OF SOIL CYCLE
! SL
   rewind(15)
!**** Reading Soil parameters from soilXX.dat files
   do k = 1,msdb

      if (k.gt.msdb.or.k.eq.0) then   
         write(6,*)'ERROR in readsol:'
         write(6,*)'   soil type ',k,' > msdb = max soil number, or 0'
         write(6,*)'   soil data name under nr ', k,' in soil.cio'
         write(6,*)'SWIM STOPS'
         stop  
      end if

! SL  call opensoil (subroutine deleted)
      read(15,fmt="(1a)") soildat
      call caps(soildat)
      
      ! open soil parameter files
      open(16,file=trim(inputPath)//'Soil/'//soildat, status='old', IOSTAT=opensucc)
      if ( opensucc /= 0 ) then
         write(6,*)'ERROR! Cannot open file: ',soildat, 'Does it exist?'
         stop
      end if

!**** READ SOIL HYDRAULIC PROPERTIES 
!     THE SOIL IS DIVIDED VERTICALLY INTO UP TO 10 LAYERS
!     LAYER THICKNESS CAN BE VARIABLE
!     ns - number of layers, nsa - number of layers for arable soils
      read(16,*, ERR=55) snum(k),ns(k),nsa(k),nsolgr(k)

      nn = ns(k)
      if ( ns(k).gt.ml.or.ns(k).eq.0 ) then
         write(6,*)'ERROR in readsol:'
         write(6,*)'   number of layers for soil ',k,' > 10 = max number, or 0'
         write(6,*)'SWIM STOPS'
         stop
      end if
      if ( nsa(k).gt.ns(k).or.nsa(k).eq.0 ) then
         write(6,*)'ERROR in readsol:'
         write(6,*)'   number-of-ar-layers, soil ',k,' > number-of-layers, or 0'
         write(6,*)'SWIM STOPS'
         stop 
      end if

      read(16,99) snam
      read(16,99) titldum
      read(16,99) titldum 
      read(16,*, ERR=55) (z(l,k),l = 1,nn) 
      read(16,*, ERR=55) (cla(l,k),l = 1,nn) 
      read(16,*, ERR=55) (sil(l,k),l = 1,nn) 
      read(16,*, ERR=55) (san(l,k),l = 1,nn) 
      read(16,*, ERR=55) (bden(l,k),l = 1,nn) 
      read(16,*, ERR=55) (poros(l,k),l = 1,nn) 
      read(16,*, ERR=55) (awc(l,k),l = 1,nn) 
      read(16,*, ERR=55) (fc(l,k),l = 1,nn) 
      read(16,*, ERR=55) (cbn(l,k),l = 1,nn) 
      read(16,*, ERR=55) (wn(l,k),l = 1,nn) 
      read(16,*, ERR=55) (sc(l,k),l = 1,nn) 
      read(16,*, ERR=55)  ek(k)        

      if (k.gt.ms.or.k.eq.0) goto 61
      do l=1,nn
         tex = cla(l,k)+sil(l,k)+san(l,k)
         if (z(l,k).lt.0.001) goto 64
         if (tex<99.0.or.tex>101.) goto 65
         if (bden(l,k).lt.0.001) goto 66
         if (poros(l,k).lt.0.01) goto 67
         if (awc(l,k).lt.0.01) goto 68
         if (fc(l,k).lt.0.01) goto 69        
         if (sc(l,k).lt.0.01) goto 78
      end do
      if (ek(k).lt.0.01) goto 79        

!**** ESTIMATE CARBON FOR LOWER LAYERS IF AVAILABLE ONLY FOR UPPER LAYER
      if (cbn(3,k).le.0) then
         xx = z(2,k)
         do l = 3, nn
            dg = (z(l,k)-xx)
            if (cbn(l,k).eq.0.) cbn(l,k) = cbn(l-1,k) * exp(-.001*dg)
            xx = z(l,k)
         end do
      end if

!**** CALCULATE INITIAL NUTRIENT CONTENT: wno3, wn, wmn, wpo, ap, pmn, op, hum
!     INITIALIZE sc() if isc = 0 
!     Units transformation:
!	    g/t   -->  kg/ha	*wt1=bden()*dg/100., bden() - bulk density
!	    %     -->  g/t	*10000.
!	    %     -->  kg/ha	*wt1*10000.
!	    kg/ha -->  g/t	1/wt1=100./dg/bden()
!	    mg/kg  =   g/t
      xx = 0.
      do l = 1, nn
        if (isc.eq.1) sc(l,k) = 0.
        dg = (z(l,k)-xx)
        wt1 = bden(l,k) * dg / 100.          
        if (wno3(l,k).le.0.) then
          wno3(l,k) = 10. * exp(-z(l,k)/1000.)     
          wno3(l,k) = wno3(l,k) * wt1              
        endif            
        wn(l,k) = 10000. * wn(l,k)                    
        if (wn(l,k).le.0.) wn(l,k) = 10000. * cbn(l,k) /10.
        wn(l,k) = wn(l,k) * wt1               
        wmn(l,k) = wn(l,k) * rtn
        wn(l,k) = wn(l,k) * (1.-rtn)          
        wpo(l,k) = .125 * wn(l,k)            
        if (ap(1,k).le.0.) ap(1,k) = 5.                 
        ap(l,k) = ap(1,k) * wt1          
        pmn(l,k) = ap(l,k) * (1.-psp) / psp          
        op(l,k) = 4. * pmn(l,k)                   
        hum(l,k) = cbn(l,k) * wt1 * 172.
        xx = z(l,k)                                       
      end do
   
!**** CALCULATE PARTICLE SIZE DISTRIBUTION psz
      silt(1,k) = sil(1,k) / 100.
      clay(1,k) = cla(1,k) / 100.
      sand(1,k) = san(1,k) / 100.
      psz(1,k) = (1.-clay(1,k)) ** 2.49 * sand(1,k)
      psz(2,k) = .13 * silt(1,k)
      psz(3,k) = .20 * clay(1,k)
      if (clay(1,k).le..5) then
        if (clay(1,k).lt..25) go to 73
        psz(4,k) = .28 * (clay(1,k)-.25) + .5
        go to 74
      end if
      psz(4,k) = .57
      go to 74
   73 psz(4,k) = 2. * clay(1,k)  
   74 psz(5,k) = 1. - psz(1,k) - psz(2,k) - psz(3,k) - psz(4,k)
        
!**** CALCULATE/RECALCULATE HYDRAULIC PARAMETERS wp, up, por, awc &
!     CALCULATE SATURATED CONDUCTIVITY sc IN CASE isc=1
!     CORRECTION of up & por by silt content: Fred Hattermann 2002, ccf=.05  
!     Removed 2011 from Tobias Vetter
      do l = 1, nn
!        wp(l,k) = 0.4 * cla(l,k) * bden(l,k) / 100.
         wp(l,k)=  fc(l,k)/100-awc(l,k)/100
!        ccf = 0.05
!        up(l,k) = wp(l,k) + awc(l,k)/100. + ccf *  sil(1,k) / 100.     
         up(l,k)= fc(l,k)/100
!        por(l,k) = 1. - bden(l,k) / 2.65 + ccf *  sil(1,k) / 100.
         por(l,k)=poros(l,k)/100
         if (up(l,k).ge.por(l,k)) then 
            up(l,k) = por(l,k) - .05 
            wp(l,k) = up(l,k) - awc(l,k)/100.
            if (wp(l,k).le.0.) then
               up(l,k) = por(l,k) * .75 
               wp(l,k) = por(l,k) * .25 
            end if
         end if
         awc(l,k) = up(l,k) - wp(l,k)

         if (isc.eq.1) then       
            asc(1)=1.
            asc(2)=san(l,k)
            asc(3)=cla(l,k)
            asc(4)=poros(l,k)/100.
            asc(5)=san(l,k)*san(l,k)
            asc(6)=cla(l,k)*cla(l,k)
            asc(7)=poros(l,k)*poros(l,k)/10000. 
            asc(8)=san(l,k)*cla(l,k)
            asc(9)=san(l,k)*poros(l,k)/100.
            asc(10)=cla(l,k)*poros(l,k)/100.
            asc(11)=asc(5)*cla(l,k)
            asc(12)=asc(6)*poros(l,k)/100.
            asc(13)=asc(5)*poros(l,k)/100.
            asc(14)=asc(6)*san(l,k)
            asc(15)=asc(7)*cla(l,k)
            asc(16)=asc(5)*asc(7)
            asc(17)=asc(6)*asc(7)
            do jj=1,17
               sc(l,k)=sc(l,k)+asc(jj)*bsc(jj)
            end do
            sc(l,k)=exp(sc(l,k))*10.
         end if
      end do

!**** GLOBAL CORRECTION of sc() using calibration parameter sccor
!**** CORRECTION of ek() using calibration parameter ekc0
      if ( .NOT.bSubcatch ) then
         do l = 1, nn
           sc(l,k) = sc(l,k) * sccor_
         end do
      end if
      ek(k) = ek(k) * ekc0

   end do ! loop over number of soil files
!*********************************************************** END OF SOIL CYCLE
         
!**** DETERMINE DRAINAGE COEFS ul, sumul, fc, sumfc, hk, wss & 
!     WRITE SELECTED SOIL PARAMETERS
      write (32,121)
      do k = 1, ms
         if (z(1,k).gt.0.) then             
            nn=ns(k)
            bd(k) = 2.65 * (1.-por(1,k))
            abd(k) = 0.
            zz = 0.
            xx = 0.
            swin(k)=0.     

            do l = 1, nn
               dg = z(l,k) - xx
               xz = por(l,k) * dg * 10.
               abd(k) = abd(k) + xz
               ul(l,k) = (por(l,k)-wp(l,k)) * dg
               sumul(k) = sumul(k) + ul(l,k)
               fc(l,k) = dg * (up(l,k)-wp(l,k))
               sumfc(k) = sumfc(k) + fc(l,k)
               stin(l,k) = fc(l,k) * stinco
               hk(l,k) = -2.655 / log10(fc(l,k)/ul(l,k))
               xx = z(l,k)
               swin(k) = swin(k) + stin(l,k)
            end do     

            abd(k) = abd(k) / (10.*z(nn,k))
                                      
            write(32,120) k !, snam
            write(32,*) 'Soil layers = ',nn
            write(32,141) (z(l,k),l=1,nn)
            write(32,147) (sc(l,k), l=1,nn)     
!            write(32,149) (cbn(l,k), l=1,nn)             
!            write(32,151) (wn(l,k), l=1,nn)
!            write(32,148) (wno3(l,k), l=1,nn)
!            write(32,152) (wmn(l,k), l=1,nn)
!            write(32,153) (wpo(l,k), l=1,nn)
!            write(32,154) (ap(l,k), l=1,nn)
!            write(32,155) (pmn(l,k), l=1,nn)
!            write(32,156) (op(l,k), l=1,nn)
!            write(32,190)                 
!          endif                  
         end if
      end do
 
!**** DEFINE CURVE NUMBERS 
!     cn2(k,n)- CN, conditions 2 
      do k = 1,ms
         do n = 1,mc
            do j = 1, mb
                select case(nsolgr(k))
                case(1)
                    cn2(k,n,j)=cn2a(n)
                    if (n.eq.1) write(32,*) 'Soil group A', k
                case(2)
                    cn2(k,n,j)=cn2b(n)
                    if (n.eq.1) write(32,*) 'Soil group B', k
                case(3)
                    cn2(k,n,j)=cn2c(n)
                    if (n.eq.1) write(32,*) 'Soil group C', k
                case(4)
                    cn2(k,n,j)=cn2d(n)
                    if (n.eq.1) write(32,*) 'Soil group D', k
                end select
            end do
         end do
         !write(32,fmt='(<nbrlut>f6.2)') (cn2(k,lu),lu = 1,nbrlut)
      end do

!       write (32,130)
!       write (32,133) (cn2(55,n),n = 1,13)
!       write (32,133) (cn2(56,n),n = 1,13)
!       write (32,133) (cn2(57,n),n = 1,13)
!       write (32,133) (cn2(59,n),n = 1,13)

!**** INITIALIZE LAND COVER and LAI
!     cv()    - land cover initial 
!     alai0() - LAI initial
      cv = 0.01
      alai0(9)=2.5
      alai0(6)=0.2
      alai0(7)=0.1
      alai0(12)=0.3      

      write (6,*) '===> Soil parameters for',k, 'soils - READ!'
      write (6,*) ' '
      write (6,*) 'NB! Riparian zone (vers. 3) implemented in wstress:'
      write (6,*) '    Increased plant transpiration ep for wetlands '
      write (6,*) ' '
       			    
   99 format (a) 
  300 format (10f8.3,a)
  120 format (//,10x,'SOIL TYPE ',i4,5x,a)
  121 format (///t20,'SOILS DATA'/)
  130 format (/t1,'CONDITION 2 CN'/)
  133 format ((13f6.0))
  141 format (/t1,'LAYER DEPTH'/(8f10.3)) 
  147 format (/t1,'SATURATED CONDUCTIVITY'/(8f10.3)) 
  148 format (/t1,'N-NO3 CONTENT, wno3, kg/ha ='/(8f10.3))
  149 format (/t1,'ORGANIC CARBON, %'/(8f10.3))         
  151 format (/t1,'STABLE ORG. N, wn, kg/ha ='/(8f10.3))
  152 format (/t1,'ACTIVE ORG. N, wmn, kg/ha ='/(8f10.3))
  153 format (/t1,'ORGANIC P, wpo, kg/ha ='/(8f10.3))
  154 format (/t1,'LABILE P, ap, kg/ha ='/(8f10.3))
  155 format (/t1,'ACTIVE MINERAL P, pmn, kg/ha ='/(8f10.3)) 
  156 format (/t1,'STABLE MINERAL P, op, kg/ha ='/(8f10.3))
  190 format ('**********************************************'/)
      return

   55 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   READ ERROR for soil ',k
      write (6,*)'   Please check: some parameters are missing?'
      write (6,*)'SWIM STOPS'
      pause
      stop     
   61 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   soil type ',k,' > ms = max soil number, or 0'
      write (6,*)'   soil data name under nr ', k,' in soil.cio'
      write (6,*)'SWIM STOPS'
      pause
      stop     
   62 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   number of layers for soil ',k,' > 10 = max number, or 0'
      write (6,*)'SWIM STOPS'
      pause
      stop     
   63 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   number-of-ar-layers, soil ',k,' > number-of-layers, or 0'
      write (6,*)'SWIM STOPS'
      pause
      stop     
   64 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   soil depth for soil ',k,' layer ',l,' = 0'
      write (6,*)'SWIM STOPS'
      pause
      stop     
   65 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   soil texture for soil ',k,' layer ',l,' is wrong'
      write (6,*)'SWIM STOPS'
      pause
      stop     
   66 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   bulk density for soil ',k,' layer ',l,' = 0 '
      write (6,*)'SWIM STOPS'
      pause
      stop     
   67 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   porosity for soil ',k,' layer ',l,' = 0 '
      write (6,*)'SWIM STOPS'
      pause
      stop     
   68 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   av. water cap. for soil ',k,' layer ',l,' = 0 '
      write (6,*)'SWIM STOPS'
      pause
      stop     
   69 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   field capacity for soil ',k,' layer ',l,' = 0 '
      write (6,*)'SWIM STOPS'
      pause
      stop     
   78 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   sat. conductivity for soil ',k,' layer ',l,' = 0 '
      write (6,*)'SWIM STOPS'
      pause
      stop     
   79 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   erodibility for soil ',k,' = 0 '
      write (6,*)'SWIM STOPS'
      pause
      stop
   96 continue
      write (6,*)'ERROR in readsol:'
      write (6,*)'   number of soils in soil.cio > msdb = ',msdb
      write (6,*)'SWIM STOPS'
      pause
      stop
   97 continue
      write (6,*)'ERROR! Cannot open file: soil.cio. Does it exist?'
      pause
      stop     
           

end subroutine readsol
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

subroutine readCNtable
      use common_par   
      character dum(1)
      real(8) cnTableline (1:15)
      read(8,*) dum ! first header line
      read(8,*) dum ! second header line
      read(8,*) dum ! third header line
      read(8, *) cnTableline
      cn2a=cnTableline
      read(8, *) cnTableline
      cn2b=cnTableline
      read(8, *) cnTableline
      cn2c=cnTableline
      read(8, *) cnTableline
      cn2d=cnTableline
      
      read(8, *) cnTableline
      canmx = cnTableline    ! maximum canopy storage per land use class
      read(8, *) cnTableline
      veg_code = cnTableline ! vegetation code (crop.dat) per land use class

      close(8)
end subroutine readCNtable

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



