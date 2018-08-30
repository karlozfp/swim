!     FILE readsub.f
!
!     SUBROUTINES IN THIS FILE          CALLED FROM
!     subroutine  readsub		main



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


subroutine readsub
!**** PURPOSE: THIS SUBROUTINE READS:
!              1) SUBBASIN PARAMETERS
!              2) GROUNDWATER PARAMETERS
!              3) CHANNEL ROUTING PARAMETERS
!              4) INITIAL WATER STORAGE IN REACHES
!**** CALLED IN:   MAIN 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!   
!      >>>>> COMMON PARAMETERS & VARIABLES
!block ab         = .02083 needed  to estimate precip alpha factor 
!      abf(j)     = alpha factor for groundwater
!      abf0       = alpha factor for groundwater, calib. param. from .bsn file
!      abf1(j)    = exp function of alpha factor for groundwater (abf)
!      al(j)      = fun(tc,tp5,tp6,flu,da), used for peak runoff rate (peakq)
!      chc(j)     = channel USLE C factor
!      chcc0      = correction coef. for chnnel USLE C factor (from .bsn)
!      chd(j)     = channel depth, m
!      chk(2,j)   = effective hydraulic conductivity of main channel, mm/h
!      chl(2,j)   = main channel length, km
!      chn(j)     = channel N value 
!      chnn(j)    = channel N value
!      chs(j)     = main channel slope, m/m
!      chss(j)    = channel slope, m/m
!      chw(2,j)   = average width of main channel, m
!      chwc0      = coefficient to correct channel width (from .bsn)
!      chxk(j)    = channel USLE K factor
!      chxkc0     = correction coef. for chnnel USLE K factor (from .bsn)
!      css(j)     = sediment conc in return flow, ppm
!      da         = basin area, km2
!      dart(ih)   = drainage area for subbasin, km2
!      delay(j)   = groundwater delay, days
!      ecp(j)     = USLE erosion control practice factor P 
!      ffc(j)     = fraction of field capacity
!      flu(j)     = fraction of subbasin area in the basin
!      gwht(j)    = initial groundwater height, m
!      gwq(j)     = initial groundwater flow contribution to streamflow, mm
!      gwq0       = initial groundwater flow contribution to streamflow, mm 
!                   (1 for basin)
!      icodes(ih) = code to switch between routing subroutines
!      ihouts(ih) = Hyd. Storage Location
!      inum1s(ih) = Reach No.
!      inum2s(ih) = Inflow Hydrograph
!block k7         = parameters for rundom value generator
!      mb         = number of subbasins
!      ovn(j)     = overland flow N value
!      rchrg(j)   = groundwater recharge, mm
!      rchrgc(j)  = fraction (0-1) of root zone perc that percolates 
!                   past shallow g-w into deep g-w
!      revapc(j)  = fraction of root zone perc that goes to revap
!      revapmn(j) = revap storage, mm
!      rn1        = random number, used in subbasin to calc alpha
!      rt(j)      = return flow travel time, days. If 0, then estimated.
!      salb(j)    = soil albedo 
!      sdtsav(j)  = initial water storage in subbasins, m3
!      sl(j)      = average slope length for subbasin, m
!                   recalc to: USLE slope length/slope steepness factor  
!      slsoil(j)  = sl(), average slope length for subbasin (m)
!      sno(j)     = initial water content of snow, mm
!      snow1      = initial snow content, mm (from .bsn)
!      storc1     = storage correction coef (from .bsn)
!      stp(j)     = average slope steepness for subbasin, m/m
!      syld(j)    = specific yield for groundwater
!      tc(j)      = sum of overland and channel concentration times, hours
!      tp5(j)     = 10 year frequency of .5h rainfall, mm
!      tp6(j)     = 10 year frequency of .6h rainfall, mm
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      a2       = local parameter
!      ct       = local parameter
!      da7      = local parameter
!      dum      = local parameter
!      i        = local parameter
!      i1       = local parameter
!      id       = local parameter
!      j        = local parameter
!      ot       = local parameter
!      sxc      = local parameter
!      titldum  = text
!      vm       = local parameter
!      xm       = local parameter
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
use mod_snow !#### SNOW MODULE       ####
implicit NONE
   integer      :: i,i1,id,j
   real(8)      :: a2,ct,da7,dum,ot,sxc,vm,xm,randn
   character*80 :: titldum

   write (6,*) 'SUBBASIN & STAT. WEATHER PARAMETERS:'
  
!*********************************************************** START OF SUBB CYCLE
   if ( b3SubFiles ) then
      !**** READ 12 - SUBBASIN PARAMETERS
      open(12,file=trim(inputPath)//"Sub/subbasin.tab",status='old',action='read', ERR=96)
      read(12,*) titldum
      do i = 1, mb
         read(12,*)dum,salb(i),sno(i),chl(1,i),chs(i),chw(1,i),chk(1,i),chn(i),ovn(i),rt(i),css(i),ecp(i),sl(i),stp(i)
         if(chs(i).le.0.) chs(i)=.0001
         if(stp(i).le.0.) stp(i)=.0002
         slsoil(i) = sl(i)
      end do
      close(12)

      !**** READ 13 - GROUNDWATER PARAMETERS
      if ( .NOT. bSubcatch ) then ! otherwise it has been done in subroutine read_subcatch_gw (subcatch.f90) already!
         open(13,file=trim(inputPath)//"Sub/groundwater.tab",status='old',action='read', ERR=97)
         read(13,*) titldum
         do i = 1, mb
            read(13,*)dum,gwht(i),gwq(i),abf(i),syld(i),delay(i),revapc(i),rchrgc(i),revapmn(i)
            abf(i) = abf0
            gwq(i) = gwq0
         end do
         close(13)
      end if
      do i = 1, mb
         !**** CORRECTION factors:
         abf1(i) = exp(-abf(i))
         delay(i) = exp(-1./(delay(i)+1.e-6))
         rchrg(i) = gwq(i)
         revapmn(i) = -1. * revapmn(i)
      end do

      !**** READ 14 - CHANNEL ROUTING PARAMETERS
      open(14,file=trim(inputPath)//"Sub/routing.tab",status='old',action='read', ERR=98)
      read(14,*) titldum
      do i = 1, mb
         read(14,*)dum,chw(2,i),chd(i),chss(i),chl(2,i),chnn(i),chk(2,i),chxk(i),chc(i)
         if(chss(i).le.0.) chss(i)=.0001
         if(chl(2,i).le.0.) chl(2,i)=.0010

         !**** CORRECTION factors:
         chw(2,i) = chw(2,i) * chwc0
         chxk(i)  = chxk(i)  * chxkc0
         chc(i)   = chc(i)   * chcc0
         chnn(i)  = chnn(i)  * chnnc0 ! Correction on coefficient provided in *.bsn file
      end do
      close(14)

      !#### CALL READWET - read and calc weather stat. parameters
      open(11,file=trim(inputPath)//'wgen.dat', status='old',action='read', ERR=95)
      do i = 1, mb
         call readwet(i)
      end do

   else ! b3SubFiles
      ! READ FROM SINGLE FILES
   
      do i = 1, mb

         !#### CALL OPEN2 - opens subbasin files: 'wgen.dat',subdat,gwdat,routdat
         call opensub  

         !#### CALL READWET - read and calc weather stat. parameters
         call readwet(i)

         !**** READ 12 - SUBBASIN PARAMETERS
         read(12,*) titldum
         read(12,*) dum, dum, salb(i), sno(i), chl(1,i),chs(i), chw(1,i), chk(1,i), chn(i), ovn(i)
         read(12,*) rt(i), css(i), ecp(i), sl(i), stp(i)
         close(12)
         if(chs(i).le.0.) chs(i)=.0001
         if(stp(i).le.0.) stp(i)=.0002
         slsoil(i) = sl(i)

         !**** READ 13 - GROUNDWATER PARAMETERS
         if ( .NOT. bSubcatch ) then ! otherwise it has been done in subroutine read_subcatch_gw (subcatch.f90) already!
            read(13,*) titldum
            read(13,*) gwht(i), gwq(i), abf(i), syld(i), delay(i),revapc(i), rchrgc(i), revapmn(i)
            close(13)
            abf(i) = abf0
            gwq(i) = gwq0
         end if

         abf1(i) = exp(-abf(i))
         delay(i) = exp(-1./(delay(i)+1.e-6))
         rchrg(i) = gwq(i)
         revapmn(i) = -1. * revapmn(i)

         !**** READ 14 - CHANNEL ROUTING PARAMETERS
         read(14,*) titldum
         read(14,*) chw(2,i), chd(i), chss(i), chl(2,i), chnn(i),chk(2,i), chxk(i), chc(i)
         close(14)
         if(chss(i).le.0.) chss(i)=.0001
         if(chl(2,i).le.0.) chl(2,i)=.0010

         !**** CORRECTION factors:
         chw(2,i) = chw(2,i) * chwc0
         chxk(i)  = chxk(i)  * chxkc0
         chc(i)   = chc(i)   * chcc0
         chnn(i)  = chnn(i)  * chnnc0 ! Correction on coefficient provided in *.bsn file

      end do

   end if ! b3SubFiles
   !*********************************************************** END OF SUBB CYCLE

   !**** READ 20 - initial water storage in reaches from 'wstor.dat'
   do i=1,mb
      read(20,*) i1, sdtsav(i)
      sdtsav(i) = sdtsav(i) * storc1
      sno(i)    = snow1
   end do
   close (20)                   
        

   !**** COMPUTE DRAINAGE AREA FOR EACH SUBBASIN
   do id = 1, mhyd
      if (icodes(id).eq.0) go to 21
      if (icodes(id).eq.1) then
         dart(ihouts(id)) = da * flu(inum1s(id))
      end if
      if (icodes(id).eq.2) then
         dart(ihouts(id)) = dart(inum2s(id))
      end if
      if (icodes(id).eq.3) then
         dart(ihouts(id)) = dart(inum2s(id))
      end if
      if (icodes(id).eq.5) then
         dart(ihouts(id)) = dart(inum1s(id)) + dart(inum2s(id))
      end if
      if (icodes(id).eq.6) then
         dart(ihouts(id)) = 0.
      end if
   end do

!**** COMPUTE TIME OF CONCENTRATION for basins tc()
   21 do j = 1, mb
         ot = .0556 * (sl(j)*ovn(j)) ** .6 / stp(j) ** .3
         ct = .62 * chl(1,j) * chn(j) ** .75 / ((da * flu(j)) ** .125 * chs(j) ** .375)
         tc(j) = ot + ct
      end do

   !**** CALC subbasin parameters sl(), al(), css() 
   da7 = da / 3.6
   do j = 1, mb
      xm = .6 * (1.-exp(-35.835*stp(j)))
      sl(j) = (sl(j)/22.127) ** xm * (65.41*stp(j)*stp(j)+4.56*stp(j)+.065)
      a2 = -log10(tp5(j)/tp6(j)) / 1.0792
      al(j) = tp6(j) * (tc(j)/6.) ** a2 * da7 * flu(j) / tp5(j)
      if (chl(2,j).ne.0.) then 
         sxc = sqrt(chss(j)) / chnn(j) 
         vm = chd(j) ** .6667 * sxc 
      end if 
      css(j) = css(j) * 1.e-6 
   end do

   !**** WRITE SUBBASIN PARAMETERS in file 32
   write (32,106)
   write (32,107) (flu(i),i = 1,mb)
   write (32,100) (salb(i),i = 1,mb)
   write (32,101) (sno(i),i = 1,mb)
   write (32,102) (chl(1,i),i = 1,mb)
   write (32,122) (chs(i),i = 1,mb)
   write (32,132) (chw(1,i),i = 1,mb)
   write (32,131) (chk(1,i),i = 1,mb)
   write (32,123) (chn(i),i = 1,mb)
   write (32,124) (ovn(i),i = 1,mb)
   write (32,127) (rt(i),i = 1,mb)
   write (32,126) (css(i),i = 1,mb)
   write (32,130) (ecp(i),i = 1,mb)
   write (32,128) (sl(i),i = 1,mb)
   write (32,129) (stp(i),i = 1,mb)
   write (32,108) (gwht(i),i = 1,mb)
   write (32,109) (gwq(i),i = 1,mb)
   write (32,110) (abf(i),i = 1,mb)
   write (32,111) (syld(i),i = 1,mb)
   write (32,112) (delay(i),i = 1,mb)
   write (32,125) (tc(i),i = 1,mb)
   write (32,133) (al(i),i = 1,mb)
   write (32,134) (dart(i),i = 1,mb)
   write (32,135) (ffc(i),i = 1,mb)
   write (32,400)

   write (6,*) '===> Statistical weather parameters - READ!'
   write (6,*) '===> Subbasin, g-w & channel parameters - READ!'
   write (6,*) ' '
    
!**** WRITE ROUTING PARAMETERS in file 32
   write (32,140)
   write (32,141) (chw(2,i),i = 1,mb)
   write (32,142) (chd(i),i = 1,mb)
   write (32,143) (chss(i),i = 1,mb)
   write (32,144) (chl(2,i),i = 1,mb)
   write (32,145) (chnn(i),i = 1,mb)
   write (32,146) (chk(2,i),i = 1,mb)
   write (32,147) (chxk(i),i = 1,mb)
   write (32,148) (chc(i),i = 1,mb)
   write (32,400)

!**** CALC par
   rn1 = randn(k7)

   99 format (a) 
  400 format(/,'******************************************************')    
  200 format (10f8.3)
  300 format (10f10.4)
  140 format (///t20,'ROUTING DATA -- SUB-BASIN TO BASIN OUTLET')
  141 format (/t1,'AVE CHANNEL WIDTH(M)',/(10f10.2))
  142 format (/t1,'AVE CHANNEL DEPTH(M)',/(10f10.2))
  143 format (/t1,'CHANNEL SLOPE(M/M)',/(10f10.2))
  144 format (/t1,'CHANNEL LENGTH(KM)',/(10f10.2))
  145 format (/t1,'CHANNEL N VALUE',/,(10f10.2))
  146 format (/t1,'HYDR COND OF CHANNEL ALLUVIUM(MM/H)',/(10f10.2))
  147 format (/t1,'USLE SOIL FACTOR K FOR CHANNEL',/(10f10.3))
  148 format (/t1,'USLE SOIL FACTOR C FOR CHANNEL',/(10f10.3))
  100 format (/t1,'SOIL ALBEDO'/(10f10.2))
  101 format (/t1,'WATER CONTENT OF SNOW COVER (MM)'/(10f10.1))
  102 format (/t1,'MAIN CHANNEL LENGTH (KM)'/(10f10.2))
  106 format (///t20,'SUB-BASIN DATA'/)
  107 format (/t1,'SUB-BASIN AREA/BASIN AREA'/(10f10.3))
  108 format (/t1,'INITIAL GROUNDWATER HEIGHT (M)'/(10f10.3))
  109 format (/t1,'INITIAL GROUNDWATER FLOW (MM)'/(10f10.3))
  110 format (/t1,'GROUNDWATER RECESSION CONST. ALPHA '/(10f10.5))
  111 format (/t1,'GROUNDWATER SPECIFIC YIELD '/(10f10.5))
  112 format (/t1,'GROUNDWATER DELAY (DAYS) '/(10f10.5))
  122 format (/t1,'CHANNEL SLOPE(M/M)'/(10f10.4))
  123 format (/t1,'CHANNEL N VALUE'/(10f10.3))
  124 format (/t1,'OVERLAND FLOW N VALUE'/(10f10.3))
  125 format (/t1,'TIME OF CONCENTRATION FOR SUB-BASINS(H)'/(10f10.2))
  126 format (/t1,'RETURN FLOW SED CONC (PPM)'/(10f10.0))
  127 format (/t1,'RETURN FLOW TRAVEL TIME(D)'/(10f10.3))
  128 format (/t1,'SLOPE LENGTH(M)'/(10f10.2))
  129 format (/t1,'SLOPE STEEPNESS(M/M)'/(10f10.4))
  130 format (/t1,'EROSION CONTROL PRACTICE FACTOR(P)'/(10f10.2))
  132 format (/t1,'AVERAGE MAIN CHANNEL WIDTH (M)'/(10f10.2))
  131 format (/t1,'HYDR COND OF CHANNEL ALLUVIUM(MM/H)'/(10f10.2))
  133 format (/t1,'ALPHA - PROPORTION OF RAINFALL DUR. tc',/(10f10.2))
  134 format (/t1,'DRAINAGE AREA - dart ',/(10f10.2))
  135 format (/t1,'FRACTION OF FIELD CAPACITY - ffc ',/(10f10.2))        
      return

   95 continue
      write(6,*)'ERROR! Cannot open file: wgen.dat  Does it exist?'
      stop     

   96 continue
      write(6,*)'ERROR! Cannot open file: Sub/subbasin.tab, does it exist?'
      stop     
   97 continue
      write(6,*)'ERROR! Cannot open file: Sub/groundwater.tab, does it exist?'
      stop     
   98 continue
      write(6,*)'ERROR! Cannot open file: Sub/routing.tab, does it exist?'

end subroutine readsub
      
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
