!     FILE open.f90
!
!     SUBROUTINES IN THIS FILE          CALLED FROM
!     subroutine open		        main
!     subroutine opensub		readsub
!     subroutine opensoil		readsol
!     subroutine closef			main
!     subroutine caps (cmd)		open, opensub, openstruct, opensoil
!    
!     DIRECTORIES NEEDED TO RUN SWIM
!     Directory         Status          For what
!-------------------------------------------------------------------------------
!     Clim              optional        climate input data
!     Code              optional        SWIM code for compilation
!     Flo               needed          to write output data
!     GIS               needed          to write output data for GIS	
!     Res               needed          to write output data
!     Soil              needed          soil input data		
!     Sub               needed          subbasin parameters from SWIM/GRASS
!
!     INPUT DATA NEEDED TO RUN SWIM
!     File name         Ccontains & prepared by				
!-------------------------------------------------------------------------------
!     file.cio          contains:    input file names
!                       prepared by: SWIM/GRASS Interface
!     str.cio           contains:    file names to write subb structure
!                       prepared by: SWIM/GRASS Interface
!     soil.cio          contains:    file names in soil database		
!                       prepared by: user, basin-specific
!
!     basname.bsn       contains:    basin, switch & calibration parameters
!                       prepared by: user, basin-specific  
!     basname.cod       contains:    period of simulation + print codes	
!                       prepared by: user, basin-specific 
!     basname.fig       contains:    routing structure			
!                       prepared by: SWIM/GRASS Interface
!     basname.str       contains:    hydrotope & subbasin structure		
!                       prepared by: SWIM/GRASS Interface
!
!     crop.dat          contains:    crop parameters				
!                       prepared by: SWIM-standard file
!     wgen.dat          contains:    weather statistics			
!                       prepared by: user, basin-specific
!     wstor.dat         contains:    initial water storage in subbasins	
!                       prepared by: user, basin-specific
!
!     clim1.dat         contains:    daily climate data 			
!                       prepared by: user, basin-specific
!     clim2.dat         contains:    daily climate data 			
!                       prepared by: user, basin-specific
!     runoff.dat        contains:    water discharge in the outlet    	
!                       prepared by: user, basin-specific  	
!
!     Sub/basnameNN.sub contains:    subbasin parameters			
!                       prepared by: SWIM/GRASS Interface
!     Sub/basnameNN.rte contains:    routing parameters			
!                       prepared by: SWIM/GRASS Interface
!     Sub/basnameNN.gw  contains:    groundwater parameters for subbasins
!                       prepared by: SWIM/GRASS Interface
!     Soil/soilNN.dat   contains:    soil parameters				
!                       prepared by: user, basin-specific
!-------------------------------------------------------------------------------



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine openfiles
!**** PURPOSE: 	  to open input/output files
!**** CALLED IN:  main
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     PARAMETERS & VARIABLES
!   
!      >>>>> COMMON PARAMETERS
!      routin   = internal (program code) name for the routing input file
!      title    = title of the program for outputs
!      >>>>>

!      >>>>> STATIC PARAMETERS
!      agrman   = internal name for the agric. manag. input file
!      basndat  = internal name for the basin parameter file
!      clim1dat, clim2dat  = internal name for the climate input file
!      CNtabledat = internal name for the curve number definition  table file
!      codedat  = internal name for the code input file
!      cropdb   = internal name for the cropdatabase input file
!      
! 

!      header   = header
!      precdat  = internal name for the precip file
!      radidat  = internal name for the rad. file
!      soillist = internal name for the soillist input file
!      struct   = internal name for the structure input file
!      tempdat  = internal name for the temper. file
!      wgener   = internal name for the weather gen. input file
!      wrunoff  = internal name for the runoff file
!      zzz      = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters & descriptions  
      use common_par
      implicit NONE
      character(len=80) :: header
      character(len=4)  :: title
      dimension title(60)
      character(len=13) :: codedat,basndat,cropdb,agrman,wgener, &
           soillist,CNtabledat,precdat,tempdat,radidat,wrunoff,zzz
      character(len=80) :: clim1dat,clim2dat
      integer           :: i,k,idum

      write(6,*) ' ' 
      write(6,*) 'SWIM STARTS: OPEN INPUT FILES'

!**** OPEN file.cio and read
      open(1,file=trim(inputPath)//'file.cio', status='old', ERR=80)
! SL      read(1,*) title
      read(1,*) codedat,routin,struct,basndat,CNtabledat
      read(1,*) cropdb,agrman,wgener
      read(1,*) soillist              
!      read(1,*) precdat
!      read(1,*) tempdat
!      read(1,*) radidat
      read(1,*) wrunoff
      read(1,*) clim1dat
      read(1,*) clim2dat
!      read(1,*) zzz
!      read(1,*) zzz
!      read(1,*) zzz
!####   CALL CAPS
	call caps(codedat)
	call caps(routin)
	call caps(struct)
	call caps(basndat)    
	  call caps(CNtabledat)       
        call caps(cropdb)
        call caps(agrman)
        call caps(wgener)
        call caps(soillist)
!        call caps(precdat)
!        call caps(tempdat)
!        call caps(radidat)
        call caps(wrunoff)
        call caps(clim1dat)
        call caps(clim2dat)

   write(*,*) "----------------------------------------------"
   write(*,*) "file.cio"
   write(*,*) "codedat: ",trim(codedat)
   write(*,*) "routin: ",trim(routin)
   write(*,*) "struct: ",trim(struct)
   write(*,*) "basndat: ",trim(basndat)
   write(*,*) "cropdb: ",trim(cropdb)
   write(*,*) "agrman: ",trim(agrman)
   write(*,*) "wgener: ",trim(wgener)
   write(*,*) "soldb: ",trim(soillist)
   !write(*,*) "strlist: ",trim(strlist)
   !write(*,*) "lutdat: ",trim(lutdat)
   !write(*,*) "landmandat: ",trim(landmandat)
   !write(*,*) "vegdat: ",trim(vegdat)
   write(*,*) "clim1dat: ",trim(clim1dat)
   write(*,*) "clim2dat: ",trim(clim2dat)
   write(*,*) "----------------------------------------------"
   write(*,*) ""
   
!**** OPEN INPUT FILES 2-5,15,20
      open(2,file=trim(inputPath)//codedat, status='old', ERR=81)
      open(3,file=trim(inputPath)//routin, status='old', ERR=82)
      open(5,file=trim(inputPath)//cropdb, status='old', ERR=84)
      open(15,file=trim(inputPath)//soillist, status='old', ERR=87)

      open(4,file=trim(inputPath)//basndat, status='old', ERR=83)
      
      open(100,file=trim(swimPath)//'Res/Q_gauges_sel_sub_routed_m3s.csv',status='REPLACE')
      open(104,file=trim(inputPath)//'gauges.output',status='OLD')
      call readbsnswitches
 
      call readcod
      
      if ( bAllSubbasinsOut ) then
         open(761,file=trim(swimPath)//'Res/Q_gauges_all_sub_routed_m3s.csv')
         write(761,'(2a5,9999999i15)') 'YEAR','DAY',(k,k=1,mb)
      end if
      
      call allocate_meap

      open(8,file=trim(inputPath)//CNtabledat, status='old', ERR=85)

      open(20,file=trim(inputPath)//'wstor.dat', status='old', ERR=88)

!**** OPEN clim1.dat and clim2.dat, OPEN runoff.dat
      open(21,file=trim(climPath)//trim(clim1dat), status='old', ERR=89)
      read(21,'(a)') header 
      open(22,file=trim(climPath)//trim(clim2dat), status='old', ERR=90)
      read(22,'(a)') header
      if ( bRunoffDat ) then
         open(25,file=trim(inputPath)//wrunoff, status='old', ERR=91)
         read(25,*) header
         read(25,*) nqobs,idum,idum, (obssb(i), i=1,nqobs)
      end if

!**** GENERAL OUTPUT
          open(31,file=trim(swimPath)//'Res/wgen.out')
          open(32,file=trim(swimPath)//'Res/rin.out')

!**** SUBROUTINES OUTPUT
      if (icurn.eq.1) open(41,file=trim(swimPath)//'Res/curn.out')
      if (isolt.eq.1) open(42,file=trim(swimPath)//'Res/solt.out')
      if (itran.eq.1) open(43,file=trim(swimPath)//'Res/tran.out')
      if (iperc.eq.1) open(44,file=trim(swimPath)//'Res/perc.out')
      if (ievap.eq.1) open(45,file=trim(swimPath)//'Res/evap.out')
      if (icrop.eq.1) open(46,file=trim(swimPath)//'Res/crop.out')
      if (ieros.eq.1) open(47,file=trim(swimPath)//'Res/eros.out')
      if (inutr.eq.1) open(48,file=trim(swimPath)//'Res/nutr.prn')
      if (irout.eq.1) open(49,file=trim(swimPath)//'Res/rout.out')
      if (iwstr.eq.1) open(50,file=trim(swimPath)//'Res/wstr.out')

!**** HYDROTOPE OUTPUT
      open(51,file=trim(swimPath)//'Res/htp-1.prn')
      open(52,file=trim(swimPath)//'Res/htp-2.prn')
      open(53,file=trim(swimPath)//'Res/htp-3.prn')
      open(54,file=trim(swimPath)//'Res/htp-4.prn')
      open(55,file=trim(swimPath)//'Res/htp-5.prn')
      open(56,file=trim(swimPath)//'Res/htp-6.prn')
      open(57,file=trim(swimPath)//'Res/htp-7.prn')

!**** SUBBASIN OUTPUT
      if ( bAllSubbasinsDaily )   open(61,file=trim(swimPath)//'Res/subd.prn')
      if ( bAllSubbasinsMonthly ) open(62,file=trim(swimPath)//'Res/subm.prn')
      open(63,file=trim(swimPath)//'Res/sub-1.prn')	
      open(64,file=trim(swimPath)//'Res/sub-2.prn')
      open(65,file=trim(swimPath)//'Res/sub-3.prn')	
      open(66,file=trim(swimPath)//'Res/sub-4.prn')     
      open(67,file=trim(swimPath)//'Res/sub-5.prn')     

!**** BASIN & RIVER OUTPUT
      open(71,file=trim(swimPath)//'Res/bad.prn')	
      open(72,file=trim(swimPath)//'Res/bam.prn')
      open(73,file=trim(swimPath)//'Res/bay.prn')
      open(74,file=trim(swimPath)//'Res/rch.prn')	
      open(75,file=trim(swimPath)//'Res/rvQ.prn')
      open(76,file=trim(swimPath)//'Res/rvaddQ.prn')

      open(40,file=trim(swimPath)//'Res/rch-mn.prn')
      open(70,file=trim(swimPath)//'Res/rvQ-mn.prn')
      open(80,file=trim(swimPath)//'Res/rvQ-ev.out')

!**** FLOWS OUTPUT: monthly, annual & aver. annual flows
!     in file 77 - monthly flows for 3 selected HRUs in subbasin inusb
!     in file 78 - annual flows  for 3 selected HRUs in subbasin inusb
!     in file 79 - average annual flows  for 9 HRUs in subbasin inusb
      if (ifloa.eq.1) then
          open(77,file=trim(swimPath)//'Flo/floMON.prn')
          open(78,file=trim(swimPath)//'Flo/floANN.prn')
          open(79,file=trim(swimPath)//'Flo/floAVE.prn')
      endif
      
!     in files 81-89: monthly flows for 9 sequential HRUs in subbasin inusb
      if (iflom.eq.1) then
          open(81,file=trim(swimPath)//'Flo/fm-s1')
          open(82,file=trim(swimPath)//'Flo/fm-s2')
          open(83,file=trim(swimPath)//'Flo/fm-s3')
          open(84,file=trim(swimPath)//'Flo/fm-s4')
          open(85,file=trim(swimPath)//'Flo/fm-s5')
          open(86,file=trim(swimPath)//'Flo/fm-s6')
          open(87,file=trim(swimPath)//'Flo/fm-s7')
          open(88,file=trim(swimPath)//'Flo/fm-s8')
          open(89,file=trim(swimPath)//'Flo/fm-s9')
      endif
      
!     in files 91-99: annual flows for 9 sequential HRUs in subbasin inusb
      if (ifloa.eq.1) then
          open(91,file=trim(swimPath)//'Flo/fa-s1')
          open(92,file=trim(swimPath)//'Flo/fa-s2')
          open(93,file=trim(swimPath)//'Flo/fa-s3')
          open(94,file=trim(swimPath)//'Flo/fa-s4')
          open(95,file=trim(swimPath)//'Flo/fa-s5')
          open(96,file=trim(swimPath)//'Flo/fa-s6')
          open(97,file=trim(swimPath)//'Flo/fa-s7')
          open(98,file=trim(swimPath)//'Flo/fa-s8')
          open(99,file=trim(swimPath)//'Flo/fa-s9')
      endif

! SL begin
!**** MONTHLY HYDROPTOPE OUTPUT
      open(181,file=trim(swimPath)//'Res/htp-1_mon.csv',status='REPLACE')
      open(182,file=trim(swimPath)//'Res/htp-2_mon.csv',status='REPLACE')
      open(183,file=trim(swimPath)//'Res/htp-3_mon.csv',status='REPLACE')
      open(184,file=trim(swimPath)//'Res/htp-4_mon.csv',status='REPLACE')
      open(185,file=trim(swimPath)//'Res/htp-5_mon.csv',status='REPLACE')
      open(186,file=trim(swimPath)//'Res/htp-6_mon.csv',status='REPLACE')
      open(187,file=trim(swimPath)//'Res/htp-7_mon.csv',status='REPLACE')
      write(181,105)
      write(182,105)
      write(183,105)
      write(184,105)
      write(185,105)
      write(186,105)
      write(187,105)
      
! SL BEGIN
      ! annual subcatch output file
      open(305,file=trim(swimPath)//'Res/bay_sc.csv')
      
      ! create a file for specific runtime messages, like errors
      if ( bErrorFile ) open(1000,file=trim(swimPath)//'swim.err.log',status='REPLACE')

!######## CaMa-Flood #######
      if ( bCamaFlood ) then
         open(1001,file=trim(swimPath)//'Res/Q_gauges_all_sub_mm.csv',status='REPLACE')
         write(1001,fmt='(a9,2000i10)') "Year Day ", (i,i=1,mb)
      end if
!######## CaMa-Flood #######

!#### SNOW MODULE
   !open(350,file=trim(swimPath)//'Res/snow_module.out',status='REPLACE')
   !open(351,file=trim(swimPath)//'GIS/glacier.out',status='REPLACE')

!#### RESERVOIR MODULE
   ! if (bRsvModule)
   ! open/close during reading input files
   !open(400,file=trim(swimPath)//rsv_Reservoir_CTRL_file,status='OLD') ! closed
   !open(400,file=trim(swimPath)//rsv_Reservoir_parm_file,status='OLD') ! closed
   !open(400,file=trim(swimPath)//rsv_Reservoir_month_file,status='OLD') ! closed
   !open(400,file=trim(swimPath)//rsv_Reservoir_stor_file,status='OLD') ! closed

   ! files opened during entire simulation
   !do funit = 401, 401+rsv_nReservoirs-1
   !   rsv_funit(res) = funit
   !   filename = trim(swimPath)//"Res/reservoir_"//trim(rsv_ResNames(res))//".out"
   !
!####
! SL end

!**** GIS OUTPUT
     if ( gis_y > 0 ) then ! annual output
        open(33,file=trim(swimPath)//'GIS/yld-gis.out')
        open(34,file=trim(swimPath)//'GIS/wat-gis.out')
        !open(35,file=trim(swimPath)//'GIS/etp-gis.out')
        open(36,file=trim(swimPath)//'GIS/pre-gis.out')
        open(37,file=trim(swimPath)//'GIS/eva-gis.out')
        open(38,file=trim(swimPath)//'GIS/run-gis.out')
        open(39,file=trim(swimPath)//'GIS/gwr-gis.out')
     end if
     if ( gis_ave > 0 ) then ! average annual output
        open(101,file=trim(swimPath)//'GIS/evapmean-gis.out',status='REPLACE')
        open(102,file=trim(swimPath)//'GIS/pcpmean-gis.out',status='REPLACE')
        open(103,file=trim(swimPath)//'GIS/petmean-gis.out',status='REPLACE')
        open(105,file=trim(swimPath)//'GIS/gwrmean-gis.out',status='REPLACE')
     end if
     if ( gis_m > 0 ) then ! monthly output
        open(120,file=trim(swimPath)//'GIS/premon-gis.out',status='REPLACE')
        open(121,file=trim(swimPath)//'GIS/runmon-gis.out',status='REPLACE') ! surface and subsurface flows
        open(122,file=trim(swimPath)//'GIS/evamon-gis.out',status='REPLACE')
        open(123,file=trim(swimPath)//'GIS/petmon-gis.out',status='REPLACE')
        open(124,file=trim(swimPath)//'GIS/gwsmon-gis.out',status='REPLACE') ! groundwater seepage
        open(125,file=trim(swimPath)//'GIS/swimon-gis.out',status='REPLACE') ! soil water index
        open(126,file=trim(swimPath)//'GIS/npredays01-gis.out',status='REPLACE')
        open(127,file=trim(swimPath)//'GIS/npredays05-gis.out',status='REPLACE')
        open(128,file=trim(swimPath)//'GIS/npredays10-gis.out',status='REPLACE')
        open(129,file=trim(swimPath)//'GIS/npredays20-gis.out',status='REPLACE')
     end if

!**** CROP OUTPUT
      if (icrop.eq.1) then       
          open(58,file=trim(swimPath)//'Res/cryld.prn')
          open(59,file=trim(swimPath)//'Res/cryld-av.prn')
      endif

      return

   80 continue
      write(6,*)'ERROR! Cannot open file: file.cio  Does it exist?'
      pause
      stop     
   81 continue
      write(6,*)'ERROR! Cannot open file: ',codedat, 'Does it exist?'
      pause
      stop     
   82 continue
      write(6,*)'ERROR! Cannot open file: ',routin, 'Does it exist?'
      pause
      stop     
   83 continue
      write(6,*)'ERROR! Cannot open file: ',basndat, 'Does it exist?'
      pause
      stop     
   84 continue
      write(6,*)'ERROR! Cannot open file: ',cropdb, 'Does it exist?'
      pause
      stop     
   85 continue
      write(6,*)'ERROR!Cannot open file: ',CNtabledat, 'Does it exist?'
      pause
      stop   
   87 continue
      write(6,*)'ERROR! Cannot open file: ',soillist, 'Does it exist?'
      pause
      stop     
   88 continue
      write(6,*)'ERROR! Cannot open file: wstor.dat  Does it exist?'
      pause
      stop     
   89 continue
      write(6,*)'ERROR! Cannot open file: clim1.dat  Does it exist?'
      pause
      stop     
   90 continue
      write(6,*)'ERROR! Cannot open file: clim2.dat  Does it exist?'
      pause
      stop     
   91 continue
      write(6,*)'ERROR! Cannot open file: ',wrunoff, 'Does it exist?'
      pause
      stop     

11600 format (a4,i4)
  105 format('YEAR     MON       PRECIP      SURQ         SUBQ       PERC       '&
             &"PET       PLANT_ET     SOIL_ET  PLANT_SOIL_ET     ALAI")

end subroutine openfiles



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine opensub
!**** PURPOSE: to open subbasin input files
!**** CALLED IN:  readsub
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      >>>>> STATIC PARAMETERS
!      direc   = directory
!      gwdat   = internal name for groundwater parameters
!      i1      = local par
!      i2      = local par
!      i3      = local par
!      i4      = local par
!      i5      = local par
!      i6      = local par
!      i7      = local par
!      routdat = internal name for channel routing parameters 
!      subdat  = internal name for subbasin parameters 
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters & descriptions        
      use common_par
      implicit NONE
      integer          :: i1,i2,i3,i4,i5
      character(len=80):: subdat,routdat,gwdat
      character(len=4) :: direc = 'Sub/'
      !data direc /'Sub/'/

!**** READ file.cio 
      read(1,*) i1,i2,i3,i4,i5,subdat,routdat,gwdat
      !read(1,*) i6,i7
!####   CALL CAPS
        call caps(subdat) 
        call caps(routdat)
        call caps(gwdat)

!**** OPEN 11-14
      open(11,file=trim(inputPath)//'wgen.dat', status='old',action='read', ERR=96)
      open(12,file=trim(inputPath)//direc//subdat, status='old',action='read', ERR=97)
      open(13,file=trim(inputPath)//direc//gwdat, status='old',action='read', ERR=98)
      open(14,file=trim(inputPath)//direc//routdat, status='old',action='read', ERR=99)

      return

   96 continue
      write(6,*)'ERROR! Cannot open file: wgen.dat  Does it exist?'
      pause
      stop     
   97 continue
      write(6,*)'ERROR! Cannot open file: ',subdat, 'Does it exist?'
      pause
      stop     
   98 continue
      write(6,*)'ERROR! Cannot open file: ',gwdat, 'Does it exist?'
      pause
      stop     
   99 continue
      write(6,*)'ERROR! Cannot open file: ',routdat, 'Does it exist?'
      pause
      stop     
      
  102 format(i4,i2,i4,i2,i3,5a)
  103 format(2x,2i4)
      end subroutine opensub



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



!       subroutine opensoil
! !**** PURPOSE: to open soil input files
! !**** CALLED IN:  readsol
! !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! !      >>>>> STATIC PARAMETERS
! !      direct  = directory
! !      soildat = internal name for soil parameters file
! !      >>>>>
! !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! !**** Include common parameters & descriptions      
!       use common_par
!       implicit NONE
!       character*5 direct
!       character*13  soildat
!       data direct /'Soil/'/     
! 
! !**** READ 15 and OPEN 16
!       read(15,100) soildat
! !####   CALL CAPS
!         call caps(soildat)
!       open(16,file=trim(inputPath)//direct//soildat, status='old',ERR=99)
!   100 format(1a)
! 
!       return
! 
!    99 continue
!       write(6,*)'ERROR! Cannot open file: ',soildat, 'Does it exist?'
!       pause
!       stop     
! 
!       end subroutine opensoil



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&






!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      
      
      subroutine closef
!**** PURPOSE: to close subbasin input files
!**** CALLED IN:  main
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      close(11)
      close(12)
      close(13)
      close(14)
      return
      end subroutine closef



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine caps(file_name)
!**** PURPOSE: to remove extra blacks in the file name 
!**** CALLED IN:  open, opensub, openstruct, opensoil   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      >>>>> STATIC
!      cm    = local par
!      i     = local par
!      j     = local par
!      ll    = local par
!      small = local par
!      >>>>>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Descriptions and data
IMPLICIT NONE
!**** PURPOSE: to convert capital letters to lower case letters
!        and to remove leading blanks in the file name
!**** CALLED IN:  open, opensub, openstruct, opensoil
   character(len=*),intent(inout) :: file_name
   character(len=26)              :: lower_case = "abcdefghijklmnopqrstuvwxyz"
   character(len=26)              :: upper_case = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   integer                        :: i, j = 0

!**** Convert upper case letters to lower case letters
   do i = 1, LEN(file_name)
      j = INDEX (upper_case,file_name(i:i))
      if (j /= 0) file_name(i:i) = lower_case(j:j)
   end do

!**** Removes leading blanks and adds them at the end
   file_name = ADJUSTL(file_name)

end subroutine caps
! 
!       implicit NONE
!       integer i,j
!       character cmd*13, cm*13,ll*1,small*1
!       dimension small(26),ll(26)
!       data small/'a','b','c','d','e','f','g','h','i','j','k','l',
!      1 'm','n','o','p','q','r','s','t','u','v','w','x','y','z'/
!       DATA ll/'A','B','C','D','E','F','G','H','I','J','K','L',
!      1 'M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/            
! 
! !**** Remove extra blacks
!       do 1 i=1,13
!         if(cmd(i:i) .eq. ' ') go to 1 
!          do 2 j=1,26
!             if (cmd(i:i) .ne.ll(j)) go to 2 
!       		cmd(i:i)=small(j)
!                 go to 1
!     2 continue
!     1 continue
!       cm = '             '
!       do 3 i = 1,13      
!       if (cmd(i:i) .ne. ' ') go to 4 
!     3 continue
!     4 continue
!       cm=cmd(i:13)
!       cmd = cm
!       return
!       end subroutine caps



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
