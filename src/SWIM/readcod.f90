! C     FILE readcod.f
! C
! C     SUBROUTINES IN THIS FILE          CALLED FROM
! C     subroutine readcod 		main
! C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

subroutine readcod
! C**** PURPOSE: THIS SUBROUTINE READS
! C              1) CODE FILE basin_name.cod containing
! C                 CONTROL PARAMETERS for period of simulation and
! C                 number of subbasins & CONTROL CODES for printout
! C              2) ROUTING STRUCTURE FILE basin_name.fig
! C**** CALLED IN:  MAIN
! C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! C      PARAMETERS & VARIABLES
! C
! C      >>>>> COMMON PARAMETERS & VARIABLES
! C      icodes(ih) = code to switch between routing subroutines, ih=1,...,mhyd
! C      icrop      = switch code to print from crop()
! C      icrsb      = number of subbasin to print from crop(), if icrop = 1
! C      icrso      = number of soil to print from crop(), if icrop = 1
! C      icurn      = switch code to print from curn()
! C      icursb     = number of subbasin to print from curn(), if icurn = 1
! C      idaf       = beginning  day of simulation
! C      idal       = last day of simulation
! C      ieros      = switch code to print from eros()
! C      iersb      = number of subbasin to print from eros(), if ieros = 1
! C      ievap      = switch code to print from evap()
! C      ievhd      = number of hydrotope to print from evap(), if ievap = 1
! C      ievsb      = number of subbasin to print from evap(), if ievap = 1
! C      ifloa      = switch code to write annual flows for HRUs
! C                   in subbasin=inusb, HRUs=(inuhd,...,inuhd+8)
! C      iflom      = switch code to write monthly flows for HRUs
! C                   in subbasin=inusb, HRUs=(inuhd,...,inuhd+8)
! C      gis_m      = switch code to write monthly results (water & crops) for GIS output
! C      gis_y      = switch code to write annual results (water & crops) for GIS output
! C      gis_ave    = switch code to write average annual results (water & crops) for GIS output
! C      ih1,ih2,ih3,ih4 = hydrotopes for HYDROTOPE PRINTOUT
! C      ihouts(ih) = Hyd. Storage Location, ih=1,...,mhyd
! C      inuhd      = number of hydrotope to print from ncycle if (inutr=1)
! C                   and from writhru
! C      inum1s(ih) = Reach No., ih=1,...,mhyd
! C      inum2s(ih) = Inflow Hydrograph, ih=1,...,mhyd
! C      inum3s(ih) = Irrigation transfer code: not used in SWIM, ih=1,...,mhyd
! C      inum4s(ih) = Reservoir No. for hyd. out: not used in SWIM, ih=1,...,mhyd
! C      inusb      = number of subbasin to print from ncycle (if inutr=1)
! C                   and from writhru
! C      inutr      = switch code to print from ncycle()
! C      ipehd      = number of hydrotope to print from perc(), if iperc = 1
! C      iperc      = switch code to print from perc()
! C      ipesb      = number of subbasin to print from perc(), if iperc = 1
! C      irout      = switch code to print from routfun()
! C      isb1,isb2,isb3,isb4 = subbasins for HYDROTOPE PRINTOUT
! C      isolt      = switch code to print from solt()
! C      isosb      = number of subbasin to print from solt(), if isolt = 1
! C      istyr      = starting year
! C      isu1,isu2,isu3 = subbasins for SUBBASIN PRINTOUT
! C      itran      = switch code to print from tran()
! C      iwshd      = number of hydrotope to print from wstress(), if iwstr = 1
! C      iwssb      = number of subbasin to print from wstress(), if iwstr = 1
! C      iwstr      = switch code to print from wstress()
! C      iyr        = beginning year of simulation
! C      nbyr       = number of years of simulation
! C      rnum1s(ih) = Flow amount transferred: not used in SWIM, ih=1,...,mhyd
! C      mb     = number of subbasins
!
! C      >>>>> STATIC PARAMETERS
! C      a        = code to finish reading (*)
! C      abc      = text
! C      idum     = cycle parameter
! C      nrch     = number of reaches
! C      nres     = number of reservoirs
! C      titldum  = text
! C      >>>>>
! C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!**** Include common parameters
      use common_par
      implicit NONE
      integer            :: idum, icd,errorFile, allSubbasinsOut, cama_switch
      integer            :: allsubbasinsDaily, allsubbasinsMonthly
      character(len= 80) :: titldum, a*1
      character(len= 40) :: abc

      write (6,*) ' '
      write (6,*) 'PROGRAM CONTROL PARAMETERS:'
!**** READ 2 - codedat
      read(2,99)  titldum
      read(2,99)  titldum
      read(2,*) nbyr, iyr, idaf, idal, mb
      if (mb == 0) goto 103 
      read(2,99)  titldum
! SL      
!      read(2,102) ml, mc, mcrdb, msdb,ms, myr, mop, mfe
!      if (mfe == 0) goto 103
!      read(2,99)  titldum
      read(2,99)  titldum
      read(2,*)   isb1,ih1,isb2,ih2,isb3,ih3,isb4,ih4,isb5,ih5,isb6,ih6,isb7,ih7
      read(2,99)  titldum
      read(2,99)  titldum
      read(2,*)   isu1,isu2,isu3,isu4,isu5
      read(2,99)  titldum
      read(2,*)   icurn, icursb,abc
      read(2,*)   isolt, isosb,abc
      read(2,*)   itran,abc
      read(2,*)   iperc, ipesb, ipehd,abc
      read(2,*)   ievap, ievsb, ievhd,abc
      read(2,*)   icrop, icrsb, icrso,abc
      read(2,*)   ieros, iersb,abc
      read(2,*)   inutr, inusb, inuhd,abc
      read(2,*)   irout,abc
      read(2,*)   iwstr, iwssb, iwshd,abc
      read(2,99)  titldum
      read(2,*)   gis_m, abc
      read(2,*)   gis_y, abc
      read(2,*)   gis_ave, abc
      read(2,*)   iflom, abc
      read(2,*)   ifloa, abc
      read(2,*)   errorFile
      read(2,*)   allSubbasinsOut
      read(2,*)   cama_switch      ! 0 = do not create cama flood output; 1 = generate output for cama flood
      read(2,*)   allsubbasinsDaily
      read(2,*)   allsubbasinsMonthly

      istyr = iyr
      close(2)

      bErrorFile = .false.
      if ( errorFile > 0 ) bErrorFile = .true.
      
      bAllSubbasinsOut = .false.
      if ( allSubbasinsOut > 0 ) bAllSubbasinsOut = .true.

      bCamaFlood = .false.
      if (cama_switch > 0) then 
         bCamaFlood = .true.
         write(*,*) "Output for the cama flood model will be generated"
      end if
      
      bAllSubbasinsDaily = .false.
      if (allsubbasinsDaily > 0) bAllSubbasinsDaily = .true.

      bAllSubbasinsMonthly = .false.
      if (allsubbasinsMonthly > 0) bAllSubbasinsMonthly = .true. 

      call swim_alloc
      
      idum = 1
      icd = 1
      icodes(1) = 1
      do while (icd > 0)
         read(3,fmt="(a)") a
         if (a /= "*") then
            backspace 3
            read(3,200) a,icodes(idum), ihouts(idum), inum1s(idum), inum2s(idum), inum3s(idum), rnum1s(idum), inum4s(idum)
            icd = icodes(idum)
            idum = idum + 1
         end if
      end do

!       meap = 150
!       call allocate_meap

 !**** WRITE control parameters and codes
    write (6,*) 'MAX number of soil layers            = ', ml
    write (6,*) 'MAX number of simulation years       = ', nbyr
    write (6,*) ' '

    write (6,*) 'PROGRAM CODES: '
    write (6,*) 'Number of years of simulation', nbyr
    write (6,*) 'First year                   ', iyr
    write (6,*) 'First day                    ', idaf
    write (6,*) 'Last day                     ', idal
    write (6,*) 'Number of subbasins          ', mb
    write (6,*) 'Number of soil types         ', ms
    write (6,*) 'Number of crops in crop.dat  ', mcrdb
    write (6,*) 'Number of land use types in lut.dat  ', mc
    write (6,*) '===> Program codes & Routing structure files - READ!'
    write (6,*) ' '
    return

  99 format (a)
 200 format (a1,9x,5i6,f6.3,5i8)
 101 format (20i5)
 102 format (8i5)

 103 continue
     write (6,*)'ERROR in reading parameter in readcod'
     write (6,*)'check the cod-file '
     write (6,*)'SWIM STOPS'
     pause
     stop

end subroutine readcod
