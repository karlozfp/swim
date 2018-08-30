c ///////////////////////////////////////////////////////////
c //
c //  Swim model engine
c //
c //  Author:	    Claus Rachimow, PIK
c //  Created on:   11 Septembewr 2008
c //  Modified for SwimCodeVersion 2010
c //  Last on:      2 February 2009
c //  Version:      1.1.1 
c //
c ///////////////////////////////////////////////////////////

C**** Main Program Engine
C	is calling
C      	readconfig: reads the main pathes
C	open and closeall: open and close files
C	readcod, readbas, readcrp, readsub, readsol: reads input data
C       makes initializations
C       starts annual cycle
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C
C      >>>>> COMMON PARAMETERS & VARIABLES
C      accf(7),accf(8) = monthly water discharge observed and simulated
C      aryld(j,k,icr)  = fraction of area by crop per sub, soil
C      arylda(iy,icr)  = fraction of area by crop per year
C      aryldc(icr)     = fraction of area by crop
C      arylds(k,icr)   = fraction of area by crop per soil
C      avyld(j,k,icr)  = av. yld per sub,soil,crop, kg/ha
C      avylda(iy,icr)  = av yld per year, crop, kg/ha
C      avyldc(icr)     = av yld per crop, kg/ha
C      avylds(k,icr)   = av. yld per soil,crop, kg/ha
C      cn              = Curve Number, current
C      da9             = 100. * da = basin area in ha
C      dflow(j,je,20)  = monthly flows for water and N (see writhru.f)
C      diso(ida)       = observed river discharge at the outlet 
C                        for the total sim. period, m3/sec. 
C      diss(ida)       = simulated river discharge at the outlet
C                        for the total sim. period, m3/sec. 
C      flu(j)          = fraction of subbasin area in the basin
C      icodes(ih)      = code to switch between routing subroutines
C      ida             = current day
C      idaf            = beginning  day of simulation
C      idal            = last day of simulation
C      idayx           = par = ida, to calc ndgro - number of growth days
C      ieap            = index for GRASS output in subbasin (yield), init. here
C      ieapu           = index for GRASS output in subbasin (water), init. here
C      ifloa           = swith code to write annual flows for HRUs 
C                        in subbasin=inusb, HRUs=(inuhd,...,inuhd+8) 
C      iflom           = swith code to write monthly flows for HRUs
C                        in subbasin=inusb, HRUs=(inuhd,...,inuhd+8) 
C      ihouts(ih)      = Hyd. Storage Location
C      inuhd           = number of hydrotope to print from ncycle and writhru
C      inum1s(ih)      = Reach No.
C      inum2s(ih)      = Inflow Hydrograph
C      inusb           = number of subbasin to print from ncycle (if inutr=1) 
C                        and writhru
C      iy              = current year as counter (1,...,nbyr)
C      iyr             = current year
C      mb              = number of subbasins
C      lub             = number of "flow transfer"
C      mo              = current month
C      nbyr            = number of years of simulation
C      nc              = number of julian days passed to the beginning of month
C      ndmo(mo)        = number days in the month
C      neap(j)         = number of HRUs in subbasin
Cblock nsb             = 30, dimension of massiv sub() - daily BASIN outputs
C      nt              = 0 if 365 days, =1 if 366 days
C      omega           = month factor for Turc-Ivanov evap
C      pr              = peak runoff, m3/sec.
C      routin          = internal (program code) name for the routing input file
C      runo(ida)       = water discharge observed, annual massiv, m3/sec.
C      runs(ida)       = water discharge simulated, annual massiv, m3/sec.
C      sbar(j)         = subbasin area, m2
C      smm(30)         = monthly BASIN outputs (dif. components, see writgen)  
C      smy(30)         = annual BASIN outputs (dif. components, see writgen)  
C      snoev           = snov evap, mm
C      sub(30)         = daily BASIN outputs (dif. components, see writgen)  
C      wy              = water yield in the basin, mm = 
C                        SUM(xqi*flu)+SUM(xssf*flu)+SUM(gwq*flu)-SUM(qtl*flu)
C      xnflow(17)      = N flows for a choosen hydrotope to write in nutr.prn 
C                       (see ncycle)
C      xwysb           = water yield for subbasin, mm
C      xxswind         = soil water index for basin
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      aop    = local par
C      dtot   = days total
C      i      = local par
C      icode  = code to switch between routing subroutines (here: 2)
C      icodep = local par
C      icr    = local par
C      id1    = starting day of simulation
C      idum   = local par
C      ihout  = hydrological storage location
C      ii     = local par
C      iik    = local par
C      ik     = local par
C      im     = local par
C      immo   = local par
C      inum1  = reach number
C      inum2  = inflow hydrograph (inum2 hydrograph is routed through inum1)
C      ix     = local par
C      j      = local par
C      jj     = local par
C      k      = local par
C      lu1    = local par
C      lutot  = local par
C      mo1    = current month number
C      nbmo   = local par
C      nbsb   = local par
C      nd     = number of days in the current year
C      ndmon  = number of days in the current month
C      ndsum  = local par
C      xn     = local par
C      xx     = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c ----------------------------------------------------------------------
      logical function InitYear(jahr)
c ----------------------------------------------------------------------
!DEC$ IFDEFINED (_WIN32)
!dec$ attributes dllexport::InitYear
!DEC$ ENDIF
      
      use common_par
      implicit none
      
      integer jahr, jj

C     iy - current year
C     mo - current month
C     nt - =1 if 365 days, =0 if 366 days
C     nd - number of days in the current year

      iy=jahr

      mo = 1
      nt = 1
      if (mod(iyr,4).eq.0) nt = 0
      ida = 1
      nd = 366 - nt
      ndsum = ndsum + nd      

C#### INITIALIZE CROPS: CALL INITCROP

      do 542 jj=1,mcrdb
        avylda(iy,jj) = 0.
        arylda(iy,jj) = 0.
  542 continue

      call initcrop(iy)
      
      if ( gis_y > 0 .OR. gis_ave > 0 ) call initsums

	InitYear=.true.
      return
      end function InitYear


c ----------------------------------------------------------------------
      logical function FinitYear()
c ----------------------------------------------------------------------
!DEC$ IFDEFINED (_WIN32)
!dec$ attributes dllexport::FinitYear
!DEC$ ENDIF
      
      use common_par
      implicit none

	integer im, icr 

C**** OPTION FOR REGIONAL CROP SIMULATION:
C---> 'goto 540' should be opened, if criteria of fit (Nash&Sutcliff etc)
C      are not needed, e.g. for regional crop simulation (to omit call xnash)
ccc      goto 540
      
C**** CALCULATE diso(), diss() for the total period evaluations
      if ( bRunoffDat ) then
         do im=1,nd
         diso(ndsum-nd+im) = runo(im,1)
         diss(ndsum-nd+im) = runs(im)
         end do
      end if
       
C#### CALL xnash() to calculate Nash & Sutcliff and other criteria
      if ( bRunoffDat ) call xnash(runo,runs,nd,1)

C**** Write average crop yield annually
      if (icrop.eq.1) then       
        do 544 icr=1,mcrdb
          if (arylda(iy,icr).gt.0.0001.and.icr.ne.51) then 
          write(59,1015) iy, icr, avylda(iy,icr)/arylda(iy,icr)
          write(6,1015)  iy, icr, avylda(iy,icr)/arylda(iy,icr)
          endif      
 544    continue
      endif
      
	FinitYear=.true.
 1015 format(' Year=', i5,' Crop=',i3,'  Av. Yield=',f7.1,' dt/ha') 
 1025 format('finish computation of year ',i6, ' ', f7.1)
      return
      end function FinitYear


c ----------------------------------------------------------------------
      logical function PerformTimeStep(jahr, monat)
c ----------------------------------------------------------------------
!DEC$ IFDEFINED (_WIN32)
!dec$ attributes dllexport::PerformTimeStep
!DEC$ ENDIF
      
      use common_par
      implicit none

c --- Locals
      logical lok
      integer jahr, monat

      mo  = monat
      
	call monthly(monat)
                      
C#### CALL MONTHLY FLOW WRITE: dflow
      if (iflom.eq.1) call flomon(monat)

C#### CALL MONTHLY WRITE
      call wr_month(monat)

                   
C****   WRITE MONTHLY WATER DISCHARGE
        write (70,1009) iyr,monat,accf(7),accf(8)
        accf(7) = 0. 
        accf(8) = 0.           

C****   ANNUAL CALCULATION & WRITE
        if (monat.eq.12) then
c         smy(2) = smy(2) / 12.
c         smy(9) = smy(9) / 12.
c         smy(10) = smy(10) / 12.
c         smy(8) = smy(8) / 12.
          smy(19) = smy(19) / 12.

C####     CALL ANNUAL FLOWS WRITE: dfloy
          if (ifloa.eq.1) call floann
          
C####     CALL ANNUAL WRITE
          call wr_annual

C     SL monthly GIS output
          if ( gis_m > 0 ) call hydro_gis_monthly()
C     SL monthly GIS output
        end if

      if (monat.eq.12) iyr = iyr + 1

      lok=.true.
      PerformTimeStep=lok

 1009 format (2i5,5f12.3)
      return
      end function PerformTimeStep


c ----------------------------------------------------------------------
      logical function Initialize(filePath_in)
c ----------------------------------------------------------------------
!DEC$ IFDEFINED (_WIN32)
!dec$ attributes dllexport::Initialize
!DEC$ ENDIF
      
      use common_par
      use mod_reservoir !#### RESERVOIR MODULE ####
      use mod_snow      !#### SNOW MODULE      ####

      implicit none
    
c --- Locals
      logical lok
      character(*) filePath_in
      character(len=1) :: header
      integer          :: i, j, s
      real(8)             :: rdum

      lok=.true.
      configPath = filePath_in

C#### READ PARAMETERS AND OPEN ALL FILES
      call readconfig
      call openfiles

c readcod now included in open
c      call readcod

C#### READ INPUT PARAMETERS: CALL READBAS, READCRP, READSUB, READSOL
      call readbas
      call readcrp
C SL begin
      !**** read output gauges (subbasins) and gauge names
      call readgaugesout
      
      !**** if routed output of all subbasins is written to output file
      if ( bAllSubbasinsOut ) then
         do i = 1, mb
            subs(i) = i
         end do

         ! Identify hydrograph storage location for each subbasin
         subouthyd = get_ihouts(mb, subs)

         ! array subs not used thereafter...
         deallocate(subs)
      end if
      
! SL read subbasins' latitude and average elevation
      open(27,file=trim(inputPath)//'stat-outdat.csv')
      read(27,*)header
      do i=1,mb
         read(27,*)s,rdum,lat(s),elev0(s) ! read latitude from 3rd column and elevation from 4th column
      end do
      close(27)

C     read file subcatch.def and corresponding subcatch files
C     enable assignment of individual subbasin parameters, write out and
C     domain subsetting (subbasins with subcatch_id=0 are not computed)
      if ( bSubcatch ) then
        call read_subcatch_def
      else
        subcatch_id = 1
      end if
   
      close (2)
      close (3)
      close (4)       

C###########################
C#### SNOW MODULE       ####
C###########################
      if ( bSnowModule ) call snow_init
C###########################

      call readsub
      call readCNtable
      call readsol    
      write (6,*) '===> INPUT PARAMETERS READ - O.K.!' 
      write (6,*)' '      

C SL  read cropland management
      if ( bLandMgt ) then 
         call readmgt(18)
         close(18)
      end if
C SL end


C###########################
C#### RESERVOIR MODULE ####
C###########################
      if ( bRsvModule ) then
         write(*,*) ""
         write(*,*) "Reservoir MODULE IS ACTIVE !!!"
         call RSV_init_reservoir
         write(*,*) "===> Number of reservoir subbasins: ",
     *              rsv_nReservoirs
         write(*,*) "===> Reservoir number(s)",rsv_ResSubbasins
         write(*,*) ""
         write(*,*) ""
      end if
C###########################


C#### INITIALIZATION: CALL INIT     
      idayx = -99
	call init

C#### CALL TTCOEFI() - calculate channel routing coefficients 
      do 80 j = 1, mb
        call ttcoefi(j)
   80 continue

C#### CALL CLOSEF to close files
      call closef
      ndsum = 0

      lok=.true.
9999  Initialize=lok
1008  format (' YEAR  MON        QOBS        QSIM')
      return
      end function Initialize

c ----------------------------------------------------------------------
      logical function RunSimulation(filePath_in)
c ----------------------------------------------------------------------
!DEC$ IFDEFINED (_WIN32)
!dec$ attributes dllexport::RunSimulation
!DEC$ ENDIF
      
      use common_par
      implicit none

c --- Functions
      logical Initialize,PerformTimeStep,Finish,InitYear,FinitYear
c --- Locals
      character(*) filePath_in
      logical lok, ldummy
      integer jahr, monat

      lok=.false.
      if(.not.Initialize(filePath_in))goto 9999

      write(6,*) 'SWIM starts simulation'

C#### BEGIN RUNOFF SIMULATION, ANNUAL LOOP - CYCLE 540
      iyrrot = 0

      do 540 jahr = 1, nbyr
         ldummy=InitYear(jahr)
C SL begin !!! count year of rotation (1-3)
         if ( iyrrot < nrotyrs ) then
            iyrrot = iyrrot + 1
         else
            iyrrot = 1
         end if
         if (iy==1) iyrrot = 0
C SL end
C####   Monthly TimeStep
	  do 530 monat = 1, 12
          if(.not.PerformTimeStep(jahr, monat))goto 9999
  530   continue
	  ldummy=FinitYear()
  540 continue
  
      if(.not.Finish())goto 9999
      lok=.true.
 9999 RunSimulation=lok

      return
      end function RunSimulation


c ----------------------------------------------------------------------
      logical function Finish()
c ----------------------------------------------------------------------
!DEC$ IFDEFINED (_WIN32)
!dec$ attributes dllexport::Finish
!DEC$ ENDIF
      
      use common_par
      implicit none

c --- Locals
      character*3 aop
      integer idum, icr, ik, jj

      iy = nbyr

C#### LAST YEAR: CALL xnash() to calculate criteria of fit, whole period
      if ( bRunoffDat ) write (6,1007) 
      write (6,*) 'Number of days =',ndsum
      write (6,*) 'Number of years =',nbyr
      if ( bRunoffDat ) call xnash(diso,diss,ndsum,2)
 
C**** LAST YEAR: Inform where are the final routed Q: rvQ.prn or rvaddQ.prn
        open(3,file=trim(inputPath)//trim(routin))
        do 666 idum = 1,mhyd
          read(3,*) aop, icodes(idum)
          if (aop.eq.'fin') then
            backspace(3)
            backspace(3)
            read(3,*) aop
            if (aop.eq.'rou') write(6,1005) 
            if (aop.eq.'add') write(6,1006) 
          endif
          if (icodes(idum).eq.0) go to 667
  666   continue
  667   continue

C**** LAST YEAR: WRITE average crop yield
      if (icrop.eq.1) then       
        write(59,2000)  
        do 673 icr=1,mcrdb
        do 672 ik=1,ms
        do 671 jj=1,mb
          if (aryld(jj,ik,icr).gt.0.0001.and.icr.ne.51) then 
          write(59,1010) icr,ik,jj, avyld(jj,ik,icr)/aryld(jj,ik,icr)
          endif
  671 continue
  672 continue
  673 continue
        write(59,2002)  
        do 675 icr=1,mcrdb
        do 674 ik=1,ms
          if (arylds(ik,icr).gt.0.0001.and.icr.ne.51) then 
          write(59,1011) icr,ik, avylds(ik,icr)/arylds(ik,icr)
          endif
  674 continue
  675 continue
        write(59,2003)  
        write(6,2003)  
        do 676 icr=1,mcrdb
          if (aryldc(icr).gt.0.0001.and.icr.ne.51) then 
          write(59,1012) icr, avyldc(icr)/aryldc(icr)
          write(6,1014) icr, avyldc(icr)/aryldc(icr)
          endif
  676 continue
      endif


C sl begin
!#### SUBCATCH AGGREGATED OUTPUT
      if ( bSubcatch ) call write_subcatch_output
C sl end

C**** ####### CALL AVERAGE ANNUAL FLOW WRITE
      if (ifloa.eq.1) call floave  

      call closeall
      
      Finish=.true.

 1005 format(/,'---> Attention! Routed Q - in Res/rvQ.prn',/)
 1006 format(/,'---> Attention! Routed Q - in Res/rvaddQ.prn',/)
 1007 format(/,' TOTAL EFFICIENCY')
 1010 format(' Crp=',i3,'  Sol=',i4,'  Sub=',i4,'  Yld=',f7.1,' dt/ha')
 1011 format(' Crp=',i3,'  Sol=',i4,'  Yld=',  f7.1,' dt/ha')
 1012 format(' Crp=',i3,'  AvYld=',  f7.1,' dt/ha')
 1014 format(' Crop=',i3,'  Average Yield=',  f7.1,' dt/ha')
 2000 format ('Average crop yield per subbasin & soil type',/)
 2002 format (/,'Average crop yield per soil type',/)      
 2003 format (/,'Average basin yield per crop type',/)      
 8080 format (6f12.5)   
      return
      end function Finish

