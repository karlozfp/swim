C     FILE readconfig.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine readconfig 	    	Initialize

C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


      subroutine readconfig
C**** PURPOSE: THIS SUBROUTINE READS THE CONFIGURATION FILE
C
C**** CALLED IN:  SwimEngineDLL.f 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C      COMMON PARAMETERS & VARIABLES
C      currently not used
C      noResout = ban of Res-output
C      ioxs     = if 0, no oxygen stress
C      noNp  	= if 0, no computation of N and P
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      character*300 dir
      character*80 titldum
      character*200 wsdir, inputdir, outputdir, climdir, hydrotopedir

      write (6,*) ' '    
      write (6,*) 'Workspace specification:'

      open(222,file=trim(configPath)//'swim.conf')

      read(222,99)  titldum
      read(222,99) dir
      call split(dir,'#',wsdir)

      read(222,99) dir
      call split(dir,'#',inputdir)
      inputPath = trim(wsdir)//trim(inputdir)
      write (6,*) trim(inputPath)

      read(222,99) dir
      call split(dir,'#',outputdir)
      swimPath = trim(wsdir)//trim(outputdir)
      write (6,*) trim(swimPath)

      read(222,99) dir
      call split(dir,'#',climdir)
C      climPath = trim(wsdir)//trim(climdir)
      climPath = trim(wsdir)//trim(climdir)
      write (6,*) trim(climPath)
      
      read(222,99) dir
      call split(dir,'#',hydrotopedir)
      hydrotopePath = trim(wsdir)//trim(hydrotopedir)
      write (6,*) trim(hydrotopePath)
      
      close(222)

   99 format (a) 
      return

C future use      
   30 continue
      write (6,*)'ERROR in readconfig:'
      write (6,*)'SWIM STOPS'
      close(222)
      pause
      stop     
   
      end

      subroutine split(str,delim,before)
c     Routine splits the string 'str' by the first occurence of delim.
c     The characters before the found delimiter are output in 'before'.
c     The characters after the found delimiter are output in 'str'.

      implicit NONE
      character*300 str
      character*200 before
      character     delim
      integer       ipos

      str=trim(str)
      if(len(str) == 0) return   ! string str is empty

      ipos=index(str,delim)
      if(ipos > 0) then          ! next character is a delimiter
        before = str(1:ipos-1)
        str=str(ipos+1:)
        before=trim(before)
        str=trim(str)
      end if
      return

      end subroutine split

