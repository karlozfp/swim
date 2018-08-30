!     FILE writgen.f
!
!     SUBROUTINES IN THIS FILE          CALLED FROM
!     subroutine  wr_daily              main
!     subroutine wr_month(mo1)          main
!     subroutine wr_annual              main

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!
!     susb(30,j)  = monthly SUBBASIN outputs (dif. components) sysub
!     sysub(30,j) = annual SUBBASIN outputs (dif. components) 
!     stsub(30,j) = total SUBBASIN outputs = SUM(sysub)

!     sub(30)     = daily BASIN outputs (dif. components,weighted sums)  
!     smm(30)     = monthly BASIN outputs (dif. components)  
!     smy(30)     = annual BASIN outputs (dif. components)  
!     sm(30)      = average annual BASIN outputs (dif. components)  

!     srch(18,ir) = monthly REACH outputs (dif. components)
!     syrch(18,ir)= annual REACH outputs (dif. components)
!     strch(18,ir)= total REACH outputs = SUM(syrch)

!     sbp(j)      = monthly SUM of precipitation in subbasin
!     smq(j)      = monthly SUM of surface runoff for subbasin
!     smsq(j)     = monthly SUM of subsurface runoff for subbasin
!     sym(j)      = monthly SUM of sediment yield for subbasins
!     syq(j)      = SUM(smq)  annual SUM surface runoff for subbasins
!     sysq(j)     = SUM(smsq) annual SUM sub-surface runoff for subbasins        
!     syy(j)      = SUM(sym)  annual SUM sed. yield for subbasins  
!     sq(j)       = SUM(syq)  total SUM (whole period) surf. runoff in subb.   
!     ssq(j)      = SUM(sysq) total SUM (whole period) sub-surf. runoff in subb.
!     sy(j)       = SUM(syy)  total SUM (whole period) sed. yield in subb.

! DESCRIPTION of BASIN  OUTPUTS sub() (Analogue - for susb, smm, smy):
! sub(1)  = SUM(precip)                 precipitation, mm 
! sub(2)  = SUM(precip), if tx()<0.     snowfall, mm                   
! sub(3)  = SUM(sml)                    snowmelt(mm)
! sub(4)  = SUM(tmx*flu)                average "weighted" max temp.         
! sub(5)  = SUM(tmn*flu)                average "weighted" min temp. 
! sub(6)  = SUM(ra*flu)                 radiation
! sub(7)  = SUM(sumcn*flu)              curve number 
! sub(8)  = SUM(xqi*flu)                surface runoff, mm,         
! sub(9)  = SUM(xssf*flu)               subsurface runoff, mm,      
! sub(10) = SUM(qtl*flu)                channel transm. losses, mm 
! sub(11) = SUM(xsep*flu)               percolation, mm,            
! sub(12) = SUM(xeo*flu)                pot evapotr., mm            
! sub(13) = SUM(xet*flu)                evapotranspiration, mm      
! sub(14) = SUM(snoev*flu)              snow evaporation, mm
! sub(15) = SUM(gwq*flu)                gr. water q, mm 
! sub(16) = SUM(revap*flu)              revap from g-w to soil prof., mm
! sub(17) = SUM(gwseep*flu)             g-w percolation
! sub(18) = SUM(gwchrg*flu)             g-w recharge    
! sub(19) = SUM(xswimd*flu)             soil water                  
! sub(20) = SUM(wysb*flu)               wysb=qi+ssf+gwq(j)-qtl, water yield, mm        
! sub(21) = SUM(yd/(100*da*flu)         sed yield
! sub(22) = SUM(yon*flu)                org. N loss with sed    
! sub(23) = SUM(yph*flu)                org P loss with sed     
! sub(24) = SUM(ysp*flu)                sol P
! sub(25) = SUM(xyno3*flu)              no3 in sur.                 
! sub(26) = SUM(xssfn*flu)              no3 in sub sur              
! sub(27) = SUM(xpercn*flu)             no3 leached                 
! sub(28) = SUM(xuno3*flu)              N uptake by plants          
!         Analogue - for susb, smm, smy:
!         SUB(1)= DAILY PRECIPITATION 
!         SMM(1)= TOTAL MONTHLY PRECIPITATION 
!         SMY(1)= TOTAL YEARLY PRECIPITATION 
! DESCRIPTION OF monthly REACH OUTPUTS
! srch(1,ir)  = SUM(varoute(2,)/86400.)     surface flow, inflow   
! srch(2,ir)  = SUM(xxqd/86400.)            surface flow, outflow
! srch(3,ir)  = SUM(varoute(3,))            sediment in
! srch(4,ir)  = SUM(yd)                     sediment out
! srch(5,ir)  = SUM(sedcon)                 sediment conc.
! srch(6,ir)  = SUM(varoute(4,))            organic N in 
! srch(7,ir)  = SUM(yon*dart(ihout)*100)    organic N out
! srch(8,ir)  = SUM(varoute(5,2))           organic P in
! srch(9,ir)  = SUM(yph*dart(ihout)*100)    organic P out
! srch(10,ir) = SUM(evp/86400.)             evaporation         (not active!)
! srch(11,ir) = SUM(tlc/86400.              transmission losses (not active!)
! srch(12,ir) = SUM(rl/86400.               seepage             (not active!)
! srch(13,ir) = SUM(diver/86400.)           diversion           (not active!)
! srch(14,ir) = SUM(rflow/86400.)           return flow         (not active!)
! srch(15,ir) = SUM(varoute(6,2))           nitrate N in 
! srch(16,ir) = SUM(xxnit)                  nitrate N out
! srch(17,ir) = SUM(varoute(7,2)*100)       soluble P in
! srch(18,ir) = SUM(xysp*dart(ihout)*100)   soluble P out
!          Analogue - for syrch = SUM(srch) - annual sums
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



subroutine wr_daily
!**** PURPOSE: GENERAL DAILY WRITE 
!**** CALLED IN:  MAIN 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      >>>>> COMMON PARAMETERS & VARIABLES
!      ATTN:      full description of sub() see above 
!      ida      = current day
!      iyr      = current year
!      sub(30)  = daily BASIN outputs (weighted sums) 
!      xxswind  = average soil water index for basin
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      use mod_snow !#### SNOW MODULE       ####
      implicit NONE

!****   DAILY WRITE, FILE 71
!       THE FOLLOWING VARIABLES ARE OUTPUT ON DAILY BASIS 
!       SUB(1)= DAILY PRECIPITATION, etc.
       if (.NOT. bSnowModule ) then
         if( ida==1.and.iy==1 ) then
            write(71,101)
            write(71,102) iyr,ida,sub(1)+sub(2),sub(5),sub(29),sub(4),sub(8),sub(9),sub(11), sub(12),  &
                           sub(13),sub(15),sub(18),sub(20),xxswind,area_tot_snow/10.**6,depth_ave_snow/1000.
         else
            write(71,102) iyr,ida,sub(1)+sub(2),sub(5),sub(29),sub(4),sub(8),sub(9),sub(11), sub(12),  &
                           sub(13),sub(15),sub(18),sub(20),xxswind,area_tot_snow/10.**6,depth_ave_snow/1000.
         end if
      else
         if( ida==1.and.iy==1 ) then
            write(71,103)
            write(71,104) iyr,ida,sub(1)-sub(2),sub(2),sub(1),sub(29),sub(8),sub(9),sub(11), sub(12),  &
                           sub(13),sub(15),sub(18),sub(20),xxswind,area_tot_snow/10.**6,depth_ave_snow/1000., &
                           area_tot_glacier/10**6,depth_ave_glacier/1000.
         else
            write(71,104) iyr,ida,sub(1)-sub(2),sub(2),sub(1),sub(29),sub(8),sub(9),sub(11), sub(12),  &
                           sub(13),sub(15),sub(18),sub(20),xxswind,area_tot_snow/10.**6,depth_ave_snow/1000., &
                           area_tot_glacier/10**6,depth_ave_glacier/1000.
         end if
      end if


  101 format('YR   DAY    PREC    Tmin   Tmean    Tmax    SURQ    SUBQ    PERC     PET     AET',&
             5X,'GWQ   GWRCH    WYLD   SWIND     AREA_SNOW DEPTH_SNOW')
  102 format (2i4,13f8.1,f12.1,f8.1)

  103 format('YR   DAY    RAIN      SNOW      PREC      Tmean     SURQ      SUBQ      PERC       PET       AET',&
             5X,'GWQ     GWRCH      WYLD     SWIND     AREA_SNOW  DEPTH_SNOW  AREA_GLACIER  DEPTH_GLACIER')
  104 format (2i4,20f12.1)
end subroutine wr_daily

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




subroutine wr_month(mo1)
!**** PURPOSE:  GENERAL MONTHLY WRITE
!**** CALLED IN:   MAIN 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      >>>>> COMMON PARAMETERS & VARIABLES
!            full description for susb(), smm(), smy() see above 
!      iy           = current year as counter (1,...,nbyr)
!      iyr          = current year
!      subtot           = number of subbasins
!block              = number of julian days passed to the beginning of month
!      nvrch        = number of variables for reach output = 18
!      nvsub        = number of variables for subbasin output = 30
!      smm(30)      = monthly BASIN outputs (dif. components)
!      smq(j)       = SUM of surface runoff for subbasin
!      smsq(j)      = SUM of subsurface runoff for subbasin 
!      smy(30)      = annual BASIN outputs (dif. components)  
!      srch(18,ir)  = monthly REACH outputs (dif. components)
!      susb(30,j)   = monthly SUBBASIN outputs (dif. components) 
!      sym(j)       = SUM of sediment yield for subbasins
!      syq(j)       = SUM(smq)  annual SUM surface runoff
!      syrch(18,ir) = annual REACH outputs (dif. components)
!      sysq(j)      = SUM(smsq) annual SUM sub-surface runoff
!      sysub(30,j)  = annual SUBBASIN outputs (dif. components)
!      syy(j)       = SUM(sym)  annual SUM sed. yield
!      >>>>>

!      >>>>> STATI! PARAMETERS
!      ik      = local par
!      jk      = local par
!      ndmon   = number of days in a month
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
implicit NONE
   integer ik,jk,ndmon,mo1

!****     MONTHLY GENERAL WRITE, FILE 72
!         SMM(1)=TOTAL MONTHLY PRECIPITATION, etc 
   if (mo1.eq.1) then
      write(72,*) 'Year = ', iyr
         write(72,100)
   end if
   write(72,102) mo1,smm(1),smm(2),smm(1)+smm(2),smm(29),smm(8),smm(9),smm(11),smm(12),smm(13),smm(15),smm(18),smm(20),smm(19)

!****     MONTHLY OUTPUT WRITE to the subbasin file 62
   if ( bAllSubbasinsMonthly ) then
      do jk = 1, mb
         if (iy.eq.1.and.mo1.eq.1.and.jk.eq.1) write(62,103)          
         if (iy.eq.1.and.mo1.eq.1.and.jk.eq.1) write(62,104)          
         if (iy.eq.1) write(62,105) iy,mo1,jk, (susb(ik,jk),ik=1,3),(susb(ik,jk),ik=8,13), &
         &  (susb(ik,jk),ik=15,21)
      end do
   end if

!****     MONTHLY OUTPUT WRITE to the reach file
!         srch() - monthly reach outputs
!           convert m3 to cms
   do jk = 1, mch
      ndmon = float(nc(mo1+1)-nc(mo1))
      srch(1,jk)=srch(1,jk)/ndmon
      srch(2,jk)=srch(2,jk)/ndmon
      srch(10,jk)=srch(10,jk)/ndmon
      srch(11,jk)=srch(11,jk)/ndmon
      srch(12,jk)=srch(12,jk)/ndmon
      srch(13,jk)=srch(13,jk)/ndmon
      srch(14,jk)=srch(14,jk)/ndmon
!            write(40,106) jk, mo1, (srch(ik,jk),ik=1,12)
!      *          ,(srch(ik,jk),ik=14,18,19)
   end do

!****     CAL! YEARLY DATA, INIT monthly outputs           
   do ik = 1, nvsub
     smy(ik) = smy(ik) + smm(ik)
     smm(ik) = 0.
   end do
      
!****     CAL! sysub = SUM(susb)
!         sysub = SUM(susb)
   do ik = 1, nvsub
      do jk = 1, mb
! sl begin
         select case(ik)
            case(4,5,6,29)
               sysub(ik,jk) = sysub(ik,jk) + susb(ik,jk) / 365.
            case default
               sysub(ik,jk) = sysub(ik,jk) + susb(ik,jk)
         end select
!sl end
         susb(ik,jk) = 0.
      end do
   end do

!****     CAL! syrch = SUM(srch)
!         syrch = SUM(srch)
   do ik = 1, nvrch
      do jk = 1, mch
         syrch(ik,jk) = syrch(ik,jk) + srch(ik,jk)
         srch(ik,jk) = 0.
      end do
   end do

!****     CAL! syq(), sysq(), syy(), Initialise 
   do ik = 1, mb
      syq(ik) = syq(ik) + smq(ik)
      sysq(ik) = sysq(ik) + smsq(ik)
      syy(ik) = syy(ik) + sym(ik)
      smq(ik) = 0.
      smsq(ik) = 0.
      sym(ik) = 0.
   end do
      
100 format(' MON    RAIN    SNOW    PREC   Tmean    SURQ    SUBQ    PERC     PET     AET',5X,'GWQ   GWRCH    WYLD   SWIND')                
102 format (i4,20f8.2)
103 format ('   YR  MON  SUB',5X,'PREC',5X,'SNOW',6X,'SML',5X,'SURQ',5X,'SUBQ',5X,'TRAL',5X,'PERC',6X,'PET', &
      6X,'AET',6X,'GWQ',4X,'REVAP',3X,'GWSEEP',3X,'GWCHRG',4X,'SWIND',5X,'WYLD',5X,'YSED')
104 format ('   YR  MON  SUB',5X,'   1',5X,'   2',6X,'  3',5X,'   8',5X,'   9',5X,'  10',5X,'  11',6X,' 12', &
   6X,' 13',6X,' 15',4X,'   16',3X,'    17',3X,'    18',4X,'   19',5X,'  20',5X,'  21')
105 format (3i5,24f9.3)
106 format ('REACH ',i4,1x,i5,19f12.4)

end subroutine wr_month

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&





subroutine wr_annual
!**** PURPOSE: GENERAL ANNUAL WRITE  
!**** CALLED IN:  main 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      >>>>> COMMON PARAMETERS
!            full description for smy() see above 
!      iyr          = current year
!      subtot           = number of subbasins
!block nns          = number of variables for basin output = 18
!      nvrch        = number of variables for reach output = 18
!      nvsub        = number of variables for subbasin output = 30
!      sm(30)       = average annual BASIN outputs (dif. components)
!      smy(30)      = annual BASIN outputs (dif. components)
!      sq(j)        = SUM(syq)  total SUM (whole period) surface runoff
!      ssq(j)       = SUM(sysq) total SUM (whole period) sub-surface runoff
!      strch(18,ir) = total REACH outputs = SUM(syrch)
!      stsub(30,j)  = total SUBBASIN outputs = SUM(sysub)
!      sy(j)        = SUM(syy)  total SUM (whole period) sed. yield
!      syq(j)       = SUM(smq)  annual SUM surface runoff
!      syrch(18,ir) = annual REACH outputs (dif. components)
!      sysq(j)      = SUM(smsq) annual SUM sub-surface runoff 
!      sysub(30,j)  = annual SUBBASIN outputs (dif. components) 
!      syy(j)       = SUM(sym)  annual SUM sed. yield 
!      >>>>>

!      >>>>> STATI! PARAMETERS
!      ik     = local par
!      jk     = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
use common_par
use mod_snow  !#### SNOW MODULE       ####
implicit NONE
   integer ik,jk,s

!****       ANNUAL WRITE, FILE 73
!           SMY(1) = TOTAL YEARLY PRECIPITATION, etc. 
   if (iyr == istyr .AND..NOT. bSnowModule) write(73,100)
      if ( .NOT. bSnowModule ) then
         write(73,103) iyr, smy(1), smy(2),snow_acc_mm, soil_acc_mm, smy(8), smy(9), smy(11), smy(12), &
         &  smy(13), smy(15), smy(18),smy(20),smy(19), &
         &  smy(8)+smy(9)+smy(15),smy(8)+smy(9)+smy(15)+smy(13)
      else
         if( iyr == istyr ) write(73,101)
         write(73,103) iyr, smy(1)-smy(2),  smy(2), smy(1), smy(29), snow_acc_mm, glac_acc_mm, &
         &  soil_acc_mm, smy(8), smy(9), smy(11), smy(12), &
         &  smy(13), smy(15), smy(18),smy(20),smy(19), &
         &  smy(8)+smy(9)+smy(15),smy(8)+smy(9)+smy(15)+smy(13)
   end if

!             do 42 jk = 1, subtot
!             if (iy.eq.1) write(62,104) iy,mo1,jk,
!      *       (sysub(ik,jk),ik=1,3),
!      *       (sysub(ik,jk),ik=8,13),(sysub(ik,jk),ik=15,21)
!    42       continue


!****       CONVERT from m3 to cms
   do jk = 1, mch
      syrch(1,jk)=syrch(1,jk)/12.
      syrch(2,jk)=syrch(2,jk)/12.
      syrch(10,jk)=syrch(10,jk)/12.
      syrch(11,jk)=syrch(11,jk)/12.
      syrch(12,jk)=syrch(12,jk)/12.
      syrch(13,jk)=syrch(13,jk)/12.
      syrch(14,jk)=syrch(14,jk)/12. 
!              write(40,105) jk,iyr,(syrch(ik,jk),ik = 1,12)
!      *         ,(syrch(ik,jk),ik = 14,18)
   end do
   
!****       CAL! sq(), ssq(), sy(), Init      
   do ik = 1, mb
      sq(ik) = sq(ik) + syq(ik)
      ssq(ik) = ssq(ik) + sysq(ik)
      sy(ik) = sy(ik) + syy(ik)
      syq(ik) = 0.
      sysq(ik) = 0.
      syy(ik) = 0.
   end do

!****       CAL! sm(), Init yearly outputs
   do ik = 1, nns
      sm(ik) = sm(ik) + smy(ik)
      smy(ik) = 0.
   end do

!****       CAL! stsub = SUM(sysub)
   do ik = 1, nvsub
      do jk = 1, mb
         stsub(ik,jk) = stsub(ik,jk) + sysub(ik,jk)
!sl begin
         if ( bSubcatch .AND. subcatch_id(jk).NE.0 ) then
            subcatch_an(iy,subcatch_id(jk),ik) = subcatch_an(iy,subcatch_id(jk),ik) &
            & + sysub(ik,jk) * ( sbar(jk)/subcatch_area(subcatch_id(jk)) )

            ! whole basin
            subcatch_an(iy,n_subcatch+1,ik) = subcatch_an(iy,n_subcatch+1,ik) &
            & + sysub(ik,jk) * flu(jk)
         end if
!sl end
         sysub(ik,jk) = 0.
      end do
   end do

!****       CAL! strch() = SUM(syrch)
   do ik = 1, nvrch
      do jk = 1, mch
         strch(ik,jk) = strch(ik,jk) + syrch(ik,jk)
         syrch(ik,jk) = 0.
      end do
   end do

   99 format('YEAR    PREC    SURQ    SUBQ    PERC     PET     AET' &
   & ,5X,'GWQ   GWRCH    WYLD   SWIND      3Q  3Q+AET    ','ET_IND_km3   3Q_km3')
100 format('YEAR    PREC    SNOWFALL  SNOW_ACC    SOIL_ACC    SURQ     SUBQ     PERC      PET       AET' &
   & ,5X,'  GWQ       GWRCH     WYLD       SWIND    3Q        3Q+AET')
101 format('YEAR    RAIN    SNOWFALL   PCP    Tmean    SNOW_ACC    GLACIER_ACC   SOIL_ACC    SURQ    SUBQ    PERC     PET     AET' &
   & ,5X,'GWQ   GWRCH    WYLD   SWIND      3Q  3Q+AET')
103 format (i4,20f10.2)
104 format ('SUBBA ',i4,1x,i4,30f10.3)
105 format ('REACH ',i4,1x,i5,18e12.4)

end subroutine wr_annual

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
