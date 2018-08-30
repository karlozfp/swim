!-------------------------------------------------------------------------------
! Author  : stefan.liersch@pik-potsdam.de
! Date    : 2009-09-09
! Modified: for main version of swim (trunk)
!           2010-02-10 by Claus Rachimow
!
! PURPOSE : Allocating and deallocating dynamic arrays
!
! CALLED  : from program main
!-------------------------------------------------------------------------------

!///////////////////////////////////////////////////////////////////////////////
!
!   ALLOCATING DYNAMIC ARRAYS
!
!///////////////////////////////////////////////////////////////////////////////

subroutine swim_alloc
use common_par
implicit none
! SL
   character(len=1) :: a
   integer :: i, icd
   integer :: iht, inm1, inm2, inm3, inm4, rnm1
   
! **** count number of soils used in simulation (soil.cio)
   msdb = count_nbr_rows(15,.false.) ! if soil parms read from single soil files then no header
   ms   = msdb

! **** count number of crops in crop.dat
   mcrdb = count_nbr_rows(5,.true.)

! **** count number of managment operation years in landmgt.csv
   if ( bLandMgt ) then
      open(18,file=trim(inputPath)//trim(landmgtdat),status='OLD', ERR=10)
      mgt_tot = count_nbr_rows(18,.true.)
      !close(18)
   end if
   
! **** count number of output discharge gauges
   ngaugesout = count_nbr_rows(104,.true.)
   if ( ngaugesout <= 0 ) then
      write(*,*) "No output gauges provided in input file <gauges.output>"
      STOP
   end if

! **** READ 3 - routing structure file & count number of subbasins, reaches, res.
   mb = 0 ! count number of subbasins again
   mhyd = 0
   icd = 1
   do while (icd > 0)
      read(3,fmt="(a)") a
      if (a /= "*") then
         backspace 3

         read(3,200) a,icd, iht, inm1, inm2 !, inm3, rnm1, inm4

         select case (icd)
            case (1)                            ! icd = 1  SUBBASIN command
               mb = mb + 1                      ! subbasins
               !if (rnm1.gt.1.e-6) lub = lub + 1
            case (2)
               !nrch = nrch + 1
            case (3)
               !nres = nres + 1
            case default
               ! do nothing, continue loop
         end select

         mhyd = Max(mhyd,iht)                   ! assign max. number hydrograph notes

      end if
   end do
   rewind(3)

   mhyd = mhyd + 1
   mch  = mb

200 format (a1,9x,5i6,f6.3,5i8)

!******************************************************************************
!**** Begin: Arrays allocated with mhyd

!******************************************************************************
!  output gauges
   allocate(gaugesout(ngaugesout))
   gaugesout = 0
   allocate(gaugesouthyd(ngaugesout))
   gaugesouthyd = 0
   allocate(gaugesout_runoff(ngaugesout))
   gaugesout_runoff = 0.
   allocate(gaugesout_names(ngaugesout))
   gaugesout_names = ''
!******************************************************************************

! MANAGEMENT OPERATION PARAMETERS
   if ( bLandMgt ) then
      allocate(mgt_id(mgt_tot))
      mgt_id = 0
      allocate(mgt_nop(mgt_tot))
      mgt_nop = 0
      allocate(mgt_yr(mgt_tot))
      mgt_yr = 0
      allocate(mgt_lu_id(mgt_tot))
      mgt_lu_id = 0
      allocate(mgt_idop(mgt_tot,mop))
      mgt_idop = 0
      allocate(mgt_iopc(mgt_tot,mop))
      mgt_iopc = 0
      allocate(mgt_ncrp(mgt_tot,mop))
      mgt_ncrp = 0
      allocate(mgt_idfe(mgt_tot,mop))
      mgt_idfe = 0
      allocate(mgt_fen(mgt_tot,mop))
      mgt_fen = 0.
      allocate(mgt_feno(mgt_tot,mop))
      mgt_feno = 0.
      allocate(mgt_fep(mgt_tot,mop))
      mgt_fep = 0.
   end if
   
   allocate(icodes(mhyd))
   icodes = 0
   allocate(ihouts(mhyd))
   ihouts = 0
   allocate(inum1s(mhyd))
   inum1s = 0
   allocate(inum2s(mhyd))
   inum2s = 0
   allocate(inum3s(mhyd))
   inum3s = 0
   allocate(inum4s(mhyd))
   inum4s = 0
   allocate(rnum1s(mhyd))
   rnum1s = 0.
   allocate(dart(mhyd))
   dart = 0.
   allocate(qdilast(mhyd))
   qdilast = 0.
   allocate(qdolast(mhyd))
   qdolast = 0.
   allocate(qsilast(mhyd))
   qsilast = 0.
   allocate(qsolast(mhyd))
   qsolast = 0.
   allocate(varoute(19,mhyd))
   varoute = 0.
   allocate(qdinp(mhyd,366))
   qdinp = 0.
   allocate(qdout(mhyd,366))
   qdout = 0.
   allocate(qssinp(mhyd,366))
   qssinp = 0.
   allocate(qssout(mhyd,366))
   qssout = 0.

!**** End: Arrays allocated with mhyd
!******************************************************************************
   
!******************************************************************************
!**** Begin: Arrays allocated with: ms = total number of soils
   allocate(nsolgr(ms))
   nsolgr=0
   allocate(snum(ms))
   snum = 0
   allocate(ns(ms))             ! can be deleted if nslayers is fully implemented
   ns = 0
   allocate(ek(ms))
   ek = 0.
   allocate(bd(ms))
   bd = 0.
   allocate(abd(ms))
   abd = 0.
   allocate(swin(ms))
   swin = 0.
   allocate(sumul(ms))
   sumul = 0.
   allocate(sumfc(ms))
   sumfc = 0.
   allocate(nsa(ms))
   nsa = 0
   allocate(avylds(ms,mcrdb))
   avylds = 0.
   allocate(arylds(ms,mcrdb))
   arylds = 0.
   allocate(silt(1,ms))
   silt = 0.
   allocate(clay(1,ms))
   clay = 0.
   allocate(sand(1,ms))
   sand = 0.
   allocate(psz(5,ms))
   psz = 0.

!**** End: Arrays allocated with: ms = total number of subbasins
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with: ml=max soil layers & ms = total number of soils

   allocate(z(ml,ms))
   z = 0.
   allocate(cla(ml,ms))
   cla = 0.
   allocate(sil(ml,ms))
   sil = 0.
   allocate(san(ml,ms))
   san = 0.
   allocate(bden(ml,ms))
   bden = 0.
   allocate(poros(ml,ms))
   poros = 0.
   allocate(awc(ml,ms))
   awc = 0.
   allocate(fc(ml,ms))
   fc = 0.
   allocate(cbn(ml,ms))
   cbn = 0.
   allocate(wn(ml,ms))
   wn = 0.
   allocate(wno3(ml,ms))
   wno3 = 0.
   allocate(ap(ml,ms))
   ap = 0.
   allocate(sc(ml,ms))
   sc = 0.
   allocate(wmn(ml,ms))
   wmn = 0.
   allocate(wpo(ml,ms))
   wpo = 0.
   allocate(pmn(ml,ms))
   pmn = 0.
   allocate(op(ml,ms))
   op = 0.
   allocate(hum(ml,ms))
   hum = 0.
   allocate(wp(ml,ms))
   wp = 0.
   allocate(up(ml,ms))
   up = 0.
   allocate(por(ml,ms))
   por = 0.
   allocate(stin(ml,ms))
   stin = 0.
   allocate(ul(ml,ms))
   ul = 0.
   allocate(hk(ml,ms))
   hk = 0.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!**** End: Arrays allocated with: ms = total number of soils
!******************************************************************************


!******************************************************************************
!**** Begin: Arrays allocated with: mb = total number of subbasins

   !######## CaMa-Flood #######
   if ( bCamaFlood ) then
      allocate(runoff_mm(mb))
      runoff_mm = 0.
   end if
   !######## CaMa-Flood #######
   
   if ( bAllSubbasinsOut ) then
      allocate(subouthyd(mb))
      subouthyd = 0
      allocate(subs(mb))
      subs = 0
      allocate(runsub_m3s(mb))
      runsub_m3s = 0.
   end if

   allocate(lat(mb)) ! subbasins latitude from stat-outdat.csv
   lat = 0.

   allocate(elev0(mb))
   elev0 = 0.

   allocate(dormhr(mb))
   dormhr = 0.
   
   allocate(xkm_qd(mb))
   allocate(xkm_ssf(mb))
   allocate(c1_qd(mb))
   allocate(c2_qd(mb))
   allocate(c3_qd(mb))
   allocate(c4_qd(mb))
   allocate(c1_ssf(mb))
   allocate(c2_ssf(mb))
   allocate(c3_ssf(mb))
   allocate(c4_ssf(mb))

! subcatchment arrays
   allocate(subcatch_id(mb))
   subcatch_id = 0
   allocate(ecal(mb))
   ecal = 0.
   allocate(thc(mb))
   thc = 0.
   allocate(sccor(mb))
   sccor = 0.
   allocate(cncor(mb))
   cncor = 0.
   allocate(roc2(mb))
   roc2 = 0.
   allocate(roc4(mb))
   roc4 = 0.
   allocate(tsnfall(mb))
   tsnfall = 0.
   allocate(tmelt(mb))
   tmelt = 0.
   allocate(smrate(mb))
   smrate = 0.
   allocate(gmrate(mb))
   gmrate = 0.

   allocate(bff(mb))
   bff = 0.
   allocate(gwht(mb))
   gwht = 0.
   allocate(gwq(mb))
   gwq = 0.
   allocate(abf(mb))
   abf = 0.
   allocate(syld(mb))
   syld = 0.
   allocate(delay(mb))
   delay = 0.
   allocate(revapc(mb))
   revapc = 0.
   allocate(rchrgc(mb))
   rchrgc = 0.
   allocate(revapmn(mb))
   revapmn = 0.

   allocate(neap(mb))            ! number of HRUs in subbasin
   neap = 0
   allocate(sbar(mb))            ! subbasin area [m2]
   sbar = 0.

   allocate(tp5(mb))                ! 10 year frequency of .5h rainfall (mm)
   tp5 = 0.
   allocate(tp6(mb))                ! 10 year frequency of .6h rainfall (mm)
   tp6 = 0.
   allocate(tpnyr(mb))              ! number of years of record max .5h rainfall
   tpnyr = 0.

   allocate(gwqLastday(mb))
   gwqLastday = 0.
   allocate(additionalGwUptake(mb))
   additionalGwUptake = 0.

   allocate(flu(mb))
   flu = 0.
   allocate(yls(mb))
   yls = 0.
   allocate(ylc(mb))
   ylc = 0.
   allocate(daylmn(mb))
   daylmn = 0.

   allocate(avt(mb))
   avt = 0.
   allocate(amp(mb))
   amp = 0.
   allocate(ffc(mb))
   ffc = 0.
   allocate(daylen(mb))
   daylen = 0.

   allocate(salb(mb))
   salb = 0.
   allocate(sno(mb))
   sno = 0.

   allocate(chs(mb))
   chs = 0.
   allocate(chn(mb))
   chn = 0.
   allocate(ovn(mb))
   ovn = 0.
   allocate(rt(mb))
   rt = 0.

   allocate(css(mb))
   css = 0.
   allocate(ecp(mb))
   ecp = 0.
   allocate(sl(mb))
   sl = 0.
   allocate(stp(mb))
   stp = 0.
   allocate(slsoil(mb))
   slsoil = 0.

   allocate(abf1(mb))
   abf1 = 0.
   allocate(abf2(mb))
   abf2 = 0.
   allocate(gwqs(mb))
   gwqs = 0.
   allocate(gwqd(mb))
   gwqd = 0.

   allocate(sdtsav(mb))
   sdtsav = 0.
   allocate(tc(mb))
   tc = 0.
   allocate(al(mb))
   al = 0.
   allocate(cv(mb))
   cv = 0.01

   allocate(sq(mb))
   sq = 0.
   allocate(ssq(mb))
   ssq = 0.
   allocate(sy(mb))
   sy = 0.

   allocate(smq(mb))
   smq = 0.
   allocate(smsq(mb))
   smsq = 0.
   allocate(sym(mb))
   sym = 0.
   allocate(syq(mb))
   syq = 0.
   allocate(sysq(mb))
   sysq = 0.
   allocate(syy(mb))
   syy = 0.

   allocate(sbpy(mb))
   sbpy = 0.
   allocate(sbp(mb))
   sbp = 0.
   allocate(tmx(mb))
   tmx = 0.
   allocate(tmn(mb))
   tmn = 0.
   allocate(tx(mb))
   tx = 0.
   allocate(subp(mb))
   subp = 0.
   allocate(ra(mb))
   ra = 0.
   allocate(humi(mb))
   humi = 0.

   allocate(tmpNsur(mb))
   tmpNsur = 0.005
   allocate(tmpNsub(mb))
   tmpNsub = 0.005
   allocate(tmpNgrw(mb))
   tmpNgrw = 0.005
   allocate(tmpPsur(mb))
   tmpPsur = 0.005

   allocate(yone(mb))
   yone = 0.
   allocate(yphe(mb))
   yphe = 0.

   allocate(rchrg(mb))
   rchrg = 0.
   allocate(revapst(mb))
   revapst = 0.

   allocate(pet_day(mb)) ! for transmission losses
   pet_day = 0.
!-----------------------------------------------------------------------------

   allocate(obmx(12))
   obmx = 0.
   allocate(obmn(12))
   obmn = 0.
   allocate(rst(12))
   rst = 0.
   allocate(prw(2,12))
   rst = 0.
   allocate(wft(12,mb))
   wft = 0.
   allocate(wi(12,mb))
   wi = 0.
   allocate(wim(12))
   wi = 0.

   allocate(r(12))
   wi = 0.
   allocate(rsm(12))
   wi = 0.
   allocate(rsmm(12))
   wi = 0.
   allocate(rsmy(12))
   wi = 0.

   allocate(chl(2,mb))
   chl = 0.
   allocate(chw(2,mb))
   chw = 0.
   allocate(chk(2,mb))
   chk = 0.
   allocate(phi(20,mb))
   phi = 0.

   allocate(susb(30,mb))
   susb = 0.
   allocate(sysub(30,mb))
   sysub = 0.
   allocate(stsub(30,mb))
   stsub = 0.

   allocate(parsz(5,mb))
   parsz = 0.
   allocate(pct(5,mb))
   pct = 0.
   allocate(sda(10,mb))
   sda = 0.

!**** End: Arrays allocated with: mb = total number of subbasins
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with mb (mb) & ms & mcrdb

   allocate(ylda(mb,ms))
   ylda = 0.
   
   if ( icrop == 1 ) then
        allocate(avyld(mb,ms,mcrdb))
        avyld = 0.
        allocate(aryld(mb,ms,mcrdb))
        aryld = 0.
   endif

!**** End: Arrays allocated with mb (mb) & ms & mcrdb
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with mch

   allocate(chd(mch))
   chd = 0.
   allocate(chss(mch))
   chss = 0.
   allocate(chnn(mch))
   chnn = 0.
   allocate(chxk(mch))
   chxk = 0.
   allocate(chc(mch))
   chc = 0.

   allocate(srch(20,mch))
   srch = 0.
   allocate(syrch(18,mch))
   syrch = 0.
   allocate(strch(18,mch))
   strch = 0.

!**** End: Arrays allocated with mch
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with: mc = number of land use types

   allocate(cn2(ms,mc,mb))
   cn2 = 0.
   allocate(alai0(mc))
   alai0 = 0.

   allocate(idop(mc,mop))
   idop = 0
   allocate(iopc(mc,mop))
   iopc = 0
   allocate(ncrp(mc,mop))
   ncrp = 0

   allocate(idfe(mc,mfe))
   idfe = 0
   allocate(fen(mc,mfe))
   fen = 0.
   allocate(fep(mc,mfe))
   fep = 0.
   allocate(feno(mc,mfe))
   feno = 0.

!**** End: Arrays allocated with: mc = number of land use types
!******************************************************************************

!******************************************************************************
!**** Begin: Arrays allocated with: mcrdb = max number of crops in the database

   allocate(icnum(mcrdb))
   icnum = 0
   allocate(cnam(mcrdb)) ! crop name (4 letters)
   allocate(be(mcrdb))
   be = 0.
   allocate(hi(mcrdb))
   hi = 0.
   allocate(to(mcrdb))
   to = 0.
   allocate(tb(mcrdb))
   tb = 0.
   allocate(blai(mcrdb))
   blai = 0.
   allocate(dlai(mcrdb))
   dlai = 0.
   allocate(bn1(mcrdb))
   bn1 = 0.
   allocate(bn2(mcrdb))
   bn2 = 0.
   allocate(bn3(mcrdb))
   bn3 = 0.
   allocate(bp1(mcrdb))
   bp1 = 0.
   allocate(bp2(mcrdb))
   bp2 = 0.
   allocate(bp3(mcrdb))
   bp3 = 0.
   allocate(cnyld(mcrdb))
   cnyld = 0.
   allocate(cpyld(mcrdb))
   cpyld = 0.
   allocate(rdmx(mcrdb))
   rdmx = 0.
   allocate(cvm(mcrdb))
   cvm = 0.
   allocate(almn(mcrdb))
   almn = 0.
   allocate(sla(mcrdb))
   sla = 0.
   allocate(pt2(mcrdb))
   pt2 = 0.
   allocate(hun(mcrdb))
   hun = 0

   allocate(ilcc(mcrdb))
   ilcc = 0
   allocate(ald1(mcrdb))
   ald1 = 0.
   allocate(ald2(mcrdb))
   ald2 = 0.
   allocate(bnu1(mcrdb))
   bnu1 = 0.
   allocate(bnu2(mcrdb))
   bnu2 = 0.
   allocate(bpu1(mcrdb))
   bpu1 = 0.
   allocate(bpu2(mcrdb))
   bpu2 = 0.

   allocate(avyldc(mcrdb))
   avyldc = 0.
   allocate(aryldc(mcrdb))
   aryldc = 0.
   allocate(avylda(nbyr,mcrdb))
   avylda = 0.
   allocate(arylda(nbyr,mcrdb))
   arylda = 0.

!**** End: Arrays allocated with: mcrdb = max number of crops in the database
!******************************************************************************
   return
   10 continue
   write(*,*) 'ERROR! Cannot open file: landmgt.csv  Does it exist?'
   stop

end subroutine swim_alloc


!******************************************************************************

subroutine allocate_subcatch(n)
use common_par
implicit none
   integer, intent(in) :: n

   allocate(subcatch_an(nbyr,n+1,30))
   subcatch_an = 0.
   allocate(subcatch_area(n))
   subcatch_area = 0.
   allocate(subcatch_name(n))
   subcatch_name = ''

   ! allocate subcatchment groundwater parameters
   allocate(gw_gwht(n))
   gw_gwht = 0.
   allocate(gw_gwq(n))
   gw_gwq = 0.
   allocate(gw_abf(n))
   gw_abf = 0.
   allocate(gw_syld(n))
   gw_syld = 0.
   allocate(gw_delay(n))
   gw_delay = 0.
   allocate(gw_revapc(n))
   gw_revapc = 0.
   allocate(gw_rchrgc(n))
   gw_rchrgc = 0.
   allocate(gw_revapmn(n))
   gw_revapmn = 0.
   allocate(gw_bff(n))
   gw_bff = 0.

   ! allocate subcatch-bsn parameters
   allocate(bsn_ecal(n))
   bsn_ecal = 0.
   allocate(bsn_thc(n))
   bsn_thc = 0.
   allocate(bsn_sccor(n))
   bsn_sccor = 0.
   allocate(bsn_cncor(n))
   bsn_cncor = 0.
   allocate(bsn_roc2(n))
   bsn_roc2 = 0.
   allocate(bsn_roc4(n))
   bsn_roc4 = 0.
   allocate(bsn_tsnfall(n))
   bsn_tsnfall = 0.
   allocate(bsn_tmelt(n))
   bsn_tmelt = 0.
   allocate(bsn_smrate(n))
   bsn_smrate = 0.
   allocate(bsn_gmrate(n))
   gmrate = 0.

end subroutine allocate_subcatch

!******************************************************************************


subroutine allocate_meap
use common_par
IMPLICIT NONE
   character(len=1) :: a
   integer          :: i,n,j
   integer,dimension(:), allocatable :: nhru

   !---------------------------------------------------------------
   ! count maximal number of HRUs in subbasins (meap)
   !---------------------------------------------------------------
   open(7,file=trim(hydrotopePath)//struct, status='old')
   n = count_nbr_rows(7,.true.)
   n = n-1 ! last line is a line of zeros
   allocate(nhru(n))
   nhru = 0
   read(7,*) a ! skip header line

   meap = 0
   do i = 1,n
      read(7,*) j
      if ( j > 0 ) then
         nhru(j) = nhru(j) + 1  ! number of HRUs per subbasin
         if (j.gt.mb) then
            write(*,*) "subroutine: allocate_meap: j > mb"
            STOP
         end if

         if (nhru(j).gt.meap) meap = nhru(j)
      else
            EXIT
      end if ! (j > 0)
   end do ! i = 1,n
   deallocate(nhru)
   close(7)
   write(*,*) "Total number of HRUs: ", n
   write(*,*) "Max. number HRUs in subbasin:", meap
   !---------------------------------------------------------------

!******************************************************************************
!**** Begin: Arrays allocated with mb (mb) & meap (max number of hydrotopes in subbasin)
   allocate(frar(mb,meap))
   frar = 0.

   allocate(swe(mb,meap))
   swe = 0.
   allocate(snoa(mb,meap))
   snoa = 0.
   allocate(ste(mb,meap,ml))
   ste = 0.
   allocate(te(mb,meap,ml))
   te = 0.
   allocate(te0(mb,meap))
   te0 = 0.
   allocate(flate(mb,meap,ml))
   flate = 0.
   allocate(poe(mb,meap,ml))
   poe = 0.
   allocate(s1(mb,meap))
   s1 = 0.
   allocate(s2(mb,meap))
   s2 = 0.
   allocate(tv(mb,meap))
   tv = 0.

   allocate(smx(mb,meap))
   smx = 0.
   allocate(wf(2,mb,meap))
   wf = 0.

   allocate(bcv(mb,meap))
   bcv = 0.
   allocate(nveg(mb,meap))
   nveg = 0

   allocate(cklsp(mb,meap))
   cklsp = 0.

   allocate(mstruc(mb,meap,6))
   mstruc = 0

   allocate(nucr(mb,meap))
   nucr = 0
   allocate(idorm(mb,meap))
   idorm = 0
   
   allocate(nclc(mb,meap))
   nclc = 0
   allocate(avyldrot(23,mcrdb))
   avyldrot = 0
   allocate(aryldrot(23,mcrdb))
   aryldrot = 0
   allocate(irotup(mb,meap))
   irotup = 0
   allocate(iccup(mb,meap))
   iccup = 0
   
   
   allocate(rsd(mb,meap,2))
   rsd = 0.
   allocate(cva(mb,meap))
   cva = 0.
   allocate(dm(mb,meap))
   dm = 0.
   allocate(igro(mb,meap))
   igro = 0
   allocate(g(mb,meap))
   g = 0.
   allocate(huharv(mb,meap))
   huharv = 0.
   allocate(hia(mb,meap))
   hia = 0.
   allocate(hiad(mb,meap))
   hiad = 0.
   allocate(rwt(mb,meap))
   rwt = 0.
   allocate(olai(mb,meap))
   olai = 0.
   allocate(alai(mb,meap))
   alai = 0.
   allocate(fon(mb,meap,2))
   fon = 0.
   allocate(fop(mb,meap,2))
   fop = 0.
   allocate(yld(mb,meap))
   yld = 0.
   allocate(rd(mb,meap))
   rd = 0.
   allocate(ws(mb,meap))
   ws = 0.
   allocate(wsav(mb,meap))
   wsav = 0.
   allocate(tsav(mb,meap))
   tsav = 0.
   allocate(swh(mb,meap))
   swh = 0.
   allocate(swp(mb,meap))
   swp = 0.

   allocate(snup(mb,meap))
   snup = 0.
   allocate(spup(mb,meap))
   spup = 0.

   ! Annual and/or average annual GIS output
   if ( gis_y > 0 .OR. gis_ave > 0 ) then
      allocate(presum(mb,meap))
      presum = 0.
      allocate(pcpmean(mb,meap))
      pcpmean = 0.
      allocate(runsum(mb,meap))
      runsum = 0.
      allocate(evasum(mb,meap))
      evasum = 0.
      allocate(evamean(mb,meap))
      evamean = 0.
      allocate(petmean(mb,meap))
      petmean = 0.
      allocate(petsum(mb,meap))
      petsum = 0.
      allocate(gwrmean(mb,meap))
      gwrmean = 0.
      allocate(gwrsum(mb,meap))
      gwrsum = 0.
   end if

   ! Monthly GIS output variables
   if ( gis_m > 0 ) then
      allocate(presummon(mb,meap,12))
      presummon = 0.
      allocate(runsummon(mb,meap,12))
      runsummon = 0.
      allocate(evasummon(mb,meap,12))
      evasummon = 0.
      allocate(petsummon(mb,meap,12))
      petsummon = 0.
      allocate(gwssummon(mb,meap,12))
      gwssummon = 0.
      allocate(swisummon(mb,meap,12))
      swisummon = 0.
      allocate(npredays01(mb,meap,12))
      npredays01 = 0
      allocate(npredays05(mb,meap,12))
      npredays05 = 0
      allocate(npredays10(mb,meap,12))
      npredays10 = 0
      allocate(npredays20(mb,meap,12))
      npredays20 = 0
   end if
   
   allocate(canstor(mb,meap))
   canstor = 0.
   allocate(preinf(mb,meap))
   preinf = 0.
   allocate(hsumul(mb,meap))
   hsumul = 0.
   allocate(hsumfc(mb,meap))
   hsumfc = 0.

   allocate(ano3(mb,meap,ml))
   ano3 = 0.
   allocate(anora(mb,meap,ml))
   anora = 0.
   allocate(anors(mb,meap,ml))
   anors = 0.
   allocate(plab(mb,meap,ml))
   plab = 0.
   allocate(porg(mb,meap,ml))
   porg = 0.
   allocate(pma(mb,meap,ml))
   pma = 0.
   allocate(pms(mb,meap,ml))
   pms = 0.

   allocate(hrtc(mb,meap,ml))
   hrtc = 0.

   allocate(dflow(mb,meap,20))
   dflow = 0.
   allocate(dfloy(mb,meap,20))
   dfloy = 0.
   allocate(dflav(mb,meap,20))
   dflav = 0.

   allocate(hwss(2,mb,meap))
   hwss = 0.

   allocate(ihydRot(mb*meap))
   ihydRot = 0
   allocate(ihydRotCrp(mb*meap))
   ihydRot = 0
   allocate(ihydFert(mb*meap))
   ihydRot = 0

!**** End: Arrays allocated with mb (mb) & meap (max number of hydrotopes in subbasin)
!******************************************************************************

end subroutine allocate_meap



!///////////////////////////////////////////////////////////////////////////////
!
!   DEALLOCATING DYNAMIC ARRAYS
!
!///////////////////////////////////////////////////////////////////////////////

subroutine dealloc_parms
use common_par
IMPLICIT NONE

   deallocate(ns)
   deallocate(ek)
   deallocate(bd)
   deallocate(abd)
   deallocate(swin)
   deallocate(sumul)
   deallocate(sumfc)
   deallocate(nsa)

   deallocate(avylds)
   deallocate(arylds)

   deallocate(silt)
   deallocate(clay)
   deallocate(sand)
   deallocate(psz)

   deallocate(z)
   deallocate(cla)
   deallocate(sil)
   deallocate(san)
   deallocate(bden)
   deallocate(poros)
   deallocate(awc)
   deallocate(fc)
   deallocate(cbn)
   deallocate(wn)
   deallocate(wno3)
   deallocate(ap)
   deallocate(sc)
   deallocate(wmn)
   deallocate(wpo)
   deallocate(pmn)
   deallocate(op)
   deallocate(hum)
   deallocate(wp)
   deallocate(up)
   deallocate(por)
   deallocate(stin)
   deallocate(ul)
   deallocate(hk)

   deallocate(neap)
   deallocate(sbar)
   deallocate(tp5)
   deallocate(tp6)
   deallocate(tpnyr)
   deallocate(lat)
   deallocate(gwqLastday)
   deallocate(additionalGwUptake)

   deallocate(flu)

   deallocate(yls)
   deallocate(ylc)
   deallocate(daylmn)

   deallocate(avt)
   deallocate(amp)
   deallocate(ffc)
   deallocate(daylen)

   deallocate(salb)
   deallocate(sno)

   deallocate(chs)
   deallocate(chn)
   deallocate(ovn)
   deallocate(rt)

   deallocate(css)
   deallocate(ecp)
   deallocate(sl)
   deallocate(stp)
   deallocate(slsoil)

   deallocate(abf)
   deallocate(syld)
   deallocate(delay)
   deallocate(revapc)

   deallocate(rchrgc)
   deallocate(revapmn)
   deallocate(abf1)
   deallocate(abf2)
   deallocate(gwqs)
   deallocate(gwqd)

   deallocate(sdtsav)
   deallocate(tc)
   deallocate(al)
   deallocate(cv)

   deallocate(sq)
   deallocate(ssq)
   deallocate(sy)

   deallocate(smq)
   deallocate(smsq)
   deallocate(sym)
   deallocate(syq)
   deallocate(sysq)
   deallocate(syy)

   deallocate(sbpy)
   deallocate(sbp)
   deallocate(tmx)
   deallocate(tmn)
   deallocate(tx)
   deallocate(subp)
   deallocate(ra)
   deallocate(humi)

   deallocate(tmpNsur)
   deallocate(tmpNsub)
   deallocate(tmpNgrw)
   deallocate(tmpPsur)

   deallocate(yone)
   deallocate(yphe)

   deallocate(rchrg)
   deallocate(gwht)
   deallocate(gwq)
   deallocate(revapst)

   deallocate(obmx)
   deallocate(obmn)
   deallocate(rst)
   deallocate(prw)
   deallocate(wft)
   deallocate(wi)

   deallocate(chl)
   deallocate(chw)
   deallocate(chk)
   deallocate(phi)

   deallocate(susb)
   deallocate(sysub)
   deallocate(stsub)

   deallocate(parsz)
   deallocate(pct)
   deallocate(sda)

   deallocate(frar)

   deallocate(swe)
   deallocate(snoa)
   deallocate(ste)
   deallocate(te)
   deallocate(te0)
   deallocate(flate)
   deallocate(poe)
   deallocate(s1)
   deallocate(s2)
   deallocate(tv)

   deallocate(smx)
   deallocate(wf)

   deallocate(bcv)
   deallocate(nveg)

   deallocate(cklsp)

   deallocate(mstruc)

   deallocate(nucr)
   deallocate(idorm)
   
   deallocate(nclc)
   deallocate(avyldrot)
   deallocate(aryldrot)
   deallocate(irotup)
   deallocate(iccup)
   
   deallocate(rsd)
   deallocate(cva)
   deallocate(dm)
   deallocate(igro)
   deallocate(g)
   deallocate(huharv)
   deallocate(hia)
   deallocate(hiad)
   deallocate(rwt)
   deallocate(olai)
   deallocate(alai)
   deallocate(fon)
   deallocate(fop)
   deallocate(yld)
   deallocate(ylda)
   deallocate(rd)
   deallocate(ws)
   deallocate(wsav)
   deallocate(tsav)
   deallocate(swh)
   deallocate(swp)

   deallocate(snup)
   deallocate(spup)

   if ( gis_y > 0 .OR. gis_ave > 0 ) then
      deallocate(presum)
      deallocate(pcpmean)
      deallocate(runsum)
      deallocate(evasum)
      deallocate(evamean)
      deallocate(petmean)
      deallocate(petsum)
      deallocate(gwrmean)
      deallocate(gwrsum)
   end if

   if ( gis_m > 0 ) then
      deallocate(presummon)
      deallocate(runsummon)
      deallocate(evasummon)
      deallocate(petsummon)
      deallocate(gwssummon)
      deallocate(swisummon)
      deallocate(npredays01)
      deallocate(npredays05)
      deallocate(npredays10)
      deallocate(npredays20)
   end if
   
   deallocate(canstor)
   deallocate(preinf)
   deallocate(hsumul)
   deallocate(hsumfc)

   deallocate(ano3)
   deallocate(anora)
   deallocate(anors)
   deallocate(plab)
   deallocate(porg)
   deallocate(pma)
   deallocate(pms)

   deallocate(hrtc)

   deallocate(dflow)
   deallocate(dfloy)
   deallocate(dflav)

   if ( icrop == 1 ) then
   deallocate(avyld)
   deallocate(aryld)
   endif
   
   deallocate(hwss)

   deallocate(chd)
   deallocate(chss)
   deallocate(chnn)
   deallocate(chxk)
   deallocate(chc)

   deallocate(srch)
   deallocate(syrch)
   deallocate(strch)

   deallocate(icodes)
   deallocate(ihouts)
   deallocate(inum1s)
   deallocate(inum2s)
   deallocate(inum3s)
   deallocate(inum4s)
   deallocate(rnum1s)

   deallocate(dart)

   deallocate(qdilast)
   deallocate(qdolast)
   deallocate(qsilast)
   deallocate(qsolast)

   deallocate(varoute)

   deallocate(qdinp)
   deallocate(qdout)
   deallocate(qssinp)
   deallocate(qssout)

   deallocate(cn2)
   deallocate(alai0)

   deallocate(idop)
   deallocate(iopc)
   deallocate(ncrp)

   deallocate(idfe)
   deallocate(fen)
   deallocate(fep)
   deallocate(feno)

   deallocate(icnum)
   deallocate(cnam)
   deallocate(be)
   deallocate(hi)
   deallocate(to)
   deallocate(tb)
   deallocate(blai)
   deallocate(dlai)
   deallocate(bn1)
   deallocate(bn2)
   deallocate(bn3)
   deallocate(bp1)
   deallocate(bp2)
   deallocate(bp3)
   deallocate(cnyld)
   deallocate(cpyld)
   deallocate(rdmx)
   deallocate(cvm)
   deallocate(almn)
   deallocate(sla)
   deallocate(pt2)
   deallocate(hun)

   deallocate(ilcc)
   deallocate(ald1)
   deallocate(ald2)
   deallocate(bnu1)
   deallocate(bnu2)
   deallocate(bpu1)
   deallocate(bpu2)

   deallocate(avyldc)
   deallocate(aryldc)
   deallocate(avylda)
   deallocate(arylda)

end subroutine dealloc_parms
