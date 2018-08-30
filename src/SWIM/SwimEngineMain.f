c ///////////////////////////////////////////////////////////
c //
c // purpose:  Swim model, used as main for SwimEngine
c // file: SwimEngineMain.f
c //
c ///////////////////////////////////////////////////////////
c //  
c //  Author:	    Claus Rachimow, PIK
c //  Created on:   10 Septembewr 2008
c //  Modified for SwimCodeVersion 2008
c //  Last on:      12.Februar 2009
c //  Version:      1.1.0 
c //
c ///////////////////////////////////////////////////////////

c ----------------------------------------------------------------------
c --- PROGRAM: SwimFortranEngine.
c --- PURPOSE: Calling SwimFortranEngineDll.
c ----------------------------------------------------------------------

      program SwimFortranEngine
      implicit none

c --- Functions
      logical RunSimulation
c --- Local
      logical lok, lokPath
      integer i,n
      character*200 string
      
      lok=.false.
      lokPath = .false.

      if (iargc().gt.1) then
        write(6,'(/,a)')'Too many arguments!'
      elseif (iargc().lt.1) then
        write(6,'(/,a)')'One argument needed!'
      else
        lokPath=.true.
      endif
        if (.not. lokPath) then
          write(6,'(a)')'Please specify a workspace like'
          write(6,'(/,a)')'./'
          write(6,'(a)')'or "./"'
          write(6,'(a)')'or "c:/path to/wp"'
          write(6,'(a)')'or ../../path/wp'
        else
          call getarg(1,string)
          write(6,'(/,a)')'Start Simulation in workspace', trim(string)
c ---     Run the simulation
          lok=RunSimulation(trim(string))
        endif

c --- Termination      
      if(lok)then
        write(6,'(/,a)')'Normal termination.'
      else
        write(6,'(/,a)')'Abnormal termination.'
      endif
c      pause
      end program SwimFortranEngine
