C     FILE initcrp.f
C     INCLUDES VARIANTS - FOR SINGLE CROP: 
C     potatoes - po, silage mais - ma, summer barley - sb, 
C     winter barley - wb, winter rye - wr, winter wheat - ww
C     winter rape - wrape
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine initcrop_po		main
C     subroutine initcrop_ma		main
C     subroutine initcrop_sb		main
C     subroutine initcrop_wb		main
C     subroutine initcrop_wr		main
C     subroutine initcrop_ww		main
C     subroutine initcrop_wwII		main
C     subroutine initcrop_wrape		main
C     ATTN! The currently needed subroutine must be 'opened': 
C           subroutine initcrop(iyear)



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine initcrop_po(iyear)
C**** PURPOSE: THIS SUBROUTINE INITIALIZES CROP MANAGEMENT DATA
C**** CALLED IN: MAIN
C     Every year -  potatoes!
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C
C      >>>>> COMMON PARAMETERS & VARIABLES
C      fen(5,if)  = amount of min N fertilizers applied, kg N/ha
C      feno(5,if) = amount of org N fertilizers applied, kg N/ha
C      fep(5,if)  = amount of P fertilizers applied, kg P/ha
C      icc        = index for cover crop corr. number in crop database
C      idfe(5,if) = day of fertilization
C      idop(5,io) = day of operation
C      iopc(5,io) = operation code: 1 - planting, 2 - harvest & kill
C                                   3 - harvest,  4 - kill
C      imai       = index for maize corr. number in crop database
C      ipo        = index for potatoes corr. number in crop database
C      irp        = index for w. rape corr. number in crop database
C      isba       = index for s. barley corr. number in crop database
C      iwb        = index for w. barley corr. number in crop database 
C      iwr        = index for w. rye corr. number in crop database
C      iww        = index for w. wheat corr. number in crop database
C      ncrp(5,io) = crop number
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters 
      use common_par
      implicit NONE
      integer iyear

C**** OPERATIONS:
      
      if (iyear.gt.1) goto 2      
                                  
    1 continue
        write (6,*) '------------>   Crop - potatoes'
        idop(5,1)=105
        iopc(5,1)=1
        ncrp(5,1)=ipo           
        idop(5,2)=253
        iopc(5,2)=2
        ncrp(5,2)=ipo           
        idop(5,3)=254
        iopc(5,3)=1
        ncrp(5,3)=icc
        idop(5,4)=0
        iopc(5,4)=0
        ncrp(5,4)=0         
      idfe(5,1)= 110
      fen(5,1)=  80.
      feno(5,1)= 40.
      fep(5,1)=  28.
      idfe(5,2)= 0
      fen(5,2)=  0.
      feno(5,2)= 0.
      fep(5,2)=  0.
      idfe(5,3)= 0
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 0
      fen(5,4)=  0.
      feno(5,4)= 0.
      fep(5,4)=  0.
      goto 99
               
    2 continue
        write (6,*) '------------>   Crop - potatoes'
        idop(5,1)=104
        iopc(5,1)=2
        ncrp(5,1)=icc           
        idop(5,2)=105
        iopc(5,2)=1
        ncrp(5,2)=ipo           
        idop(5,3)=253
        iopc(5,3)=2
        ncrp(5,3)=ipo
        idop(5,4)=254
        iopc(5,4)=1
        ncrp(5,4)=icc         
      idfe(5,1)= 110
      fen(5,1)=  80.
      feno(5,1)= 40.
      fep(5,1)=  28.
      idfe(5,2)= 0
      fen(5,2)=   0.
      feno(5,2)=  0.
      fep(5,2)=   0.
      idfe(5,3)= 0
      fen(5,3)=   0.
      feno(5,3)=  0.
      fep(5,3)=   0.
      idfe(5,4)= 0
      fen(5,4)=   0.
      feno(5,4)=  0.
      fep(5,4)=   0.
      goto 99
      
   99 continue
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


      subroutine initcrop_ma(iyear)
C**** PURPOSE: THIS SUBROUTINE INITIALIZES CROP MANAGEMENT DATA
C**** CALLED IN: MAIN
C     Every year -  silage maize!
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C
C      >>>>> COMMON PARAMETERS & VARIABLES
C      fen(5,if)  = amount of min N fertilizers applied, kg N/ha
C      feno(5,if) = amount of org N fertilizers applied, kg N/ha
C      fep(5,if)  = amount of P fertilizers applied, kg P/ha
C      icc        = index for cover crop corr. number in crop database
C      idfe(5,if) = day of fertilization
C      idop(5,io) = day of operation
C      iopc(5,io) = operation code: 1 - planting, 2 - harvest & kill
C                                   3 - harvest,  4 - kill
C      imai       = index for maize corr. number in crop database
C      ipo        = index for potatoes corr. number in crop database
C      irp        = index for w. rape corr. number in crop database
C      isba       = index for s. barley corr. number in crop database
C      iwb        = index for w. barley corr. number in crop database 
C      iwr        = index for w. rye corr. number in crop database
C      iww        = index for w. wheat corr. number in crop database
C      ncrp(5,io) = crop number
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer iyear

C**** OPERATIONS:   

      if (iyear.gt.1) goto 2      
                                  
    1 continue
        write (6,*) '------------>   Crop - s. maize'
        idop(5,1)=110
        iopc(5,1)=1
        ncrp(5,1)=imai           
        idop(5,2)=278
        iopc(5,2)=2
        ncrp(5,2)=imai           
        idop(5,3)=279
        iopc(5,3)=1
        ncrp(5,3)=icc
        idop(5,4)=0
        iopc(5,4)=0
        ncrp(5,4)=0         
      idfe(5,1)= 180
      fen(5,1)= 100.
      feno(5,1)= 50.
      fep(5,1)=  40.
      idfe(5,2)= 0
      fen(5,2)=  0.
      feno(5,2)= 0.
      fep(5,2)=  0.
      idfe(5,3)= 0
      fen(5,3)= 0.
      feno(5,3)= 0.
      fep(5,3)= 0.
      idfe(5,4)= 0
      fen(5,4)= 0.
      feno(5,4)= 0.
      fep(5,4)= 0.
      goto 99
               
    2 continue
        write (6,*) '------------>   Crop - s. maize'
        idop(5,1)=109
        iopc(5,1)=2
        ncrp(5,1)=icc           
        idop(5,2)=110
        iopc(5,2)=1
        ncrp(5,2)=imai           
        idop(5,3)=278
        iopc(5,3)=2
        ncrp(5,3)=imai
        idop(5,4)=279
        iopc(5,4)=1
        ncrp(5,4)=icc         
      idfe(5,1)= 180
      fen(5,1)= 100.
      feno(5,1)= 50.
      fep(5,1)=  40.
      idfe(5,2)= 0
      fen(5,2)=  0.
      feno(5,2)= 0.
      fep(5,2)=  0.
      idfe(5,3)= 0
      fen(5,3)= 0.
      feno(5,3)= 0.
      fep(5,3)= 0.
      idfe(5,4)= 0
      fen(5,4)= 0.
      feno(5,4)= 0.
      fep(5,4)= 0.
      goto 99
      
   99 continue
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


      subroutine initcrop_sb(iyear)
C**** PURPOSE: THIS SUBROUTINE INITIALIZES CROP MANAGEMENT DATA
C**** CALLED IN: MAIN
C     Every year - summer barley
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C
C      >>>>> COMMON PARAMETERS & VARIABLES
C      fen(5,if)  = amount of min N fertilizers applied, kg N/ha
C      feno(5,if) = amount of org N fertilizers applied, kg N/ha
C      fep(5,if)  = amount of P fertilizers applied, kg P/ha
C      icc        = index for cover crop corr. number in crop database
C      idfe(5,if) = day of fertilization
C      idop(5,io) = day of operation
C      iopc(5,io) = operation code: 1 - planting, 2 - harvest & kill
C                                   3 - harvest,  4 - kill
C      imai       = index for maize corr. number in crop database
C      ipo        = index for potatoes corr. number in crop database
C      irp        = index for w. rape corr. number in crop database
C      isba       = index for s. barley corr. number in crop database
C      iwb        = index for w. barley corr. number in crop database 
C      iwr        = index for w. rye corr. number in crop database
C      iww        = index for w. wheat corr. number in crop database
C      ncrp(5,io) = crop number
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer iyear

C**** OPERATIONS:
      
      if (iyear.gt.1) goto 2      
                                  
    1 continue
        write (6,*) '------------>   Crop - summer barley'
        idop(5,1)=79
        iopc(5,1)=1
        ncrp(5,1)=isba           
        idop(5,2)=218
        iopc(5,2)=2
        ncrp(5,2)=isba           
        idop(5,3)=219
        iopc(5,3)=1
        ncrp(5,3)=icc
        idop(5,4)=0
        iopc(5,4)=0
        ncrp(5,4)=0         
      idfe(5,1)= 85
      fen(5,1)=  40.
      feno(5,1)= 20.
      fep(5,1)=  23.
      idfe(5,2)= 120
      fen(5,2)=  14.
      feno(5,2)= 6.
      fep(5,2)=  0.
      idfe(5,3)= 0
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 0
      fen(5,4)=  0.
      feno(5,4)= 0.
      fep(5,4)=  0.
      goto 99
               
    2 continue
        write (6,*) '------------>   Crop - summer barley'
        idop(5,1)=78
        iopc(5,1)=2
        ncrp(5,1)=icc           
        idop(5,2)=79
        iopc(5,2)=1
        ncrp(5,2)=isba           
        idop(5,3)=218
        iopc(5,3)=2
        ncrp(5,3)=isba
        idop(5,4)=219
        iopc(5,4)=1
        ncrp(5,4)=icc         
      idfe(5,1)= 85
      fen(5,1)=  40.
      feno(5,1)= 20.
      fep(5,1)=  23.
      idfe(5,2)= 120
      fen(5,2)=  14.
      feno(5,2)= 6.
      fep(5,2)=  0.
      idfe(5,3)= 0
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 0
      fen(5,4)=  0.
      feno(5,4)= 0.
      fep(5,4)=  0.
      goto 99
      
   99 continue
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


      subroutine initcrop_wr(iyear)
C**** PURPOSE: THIS SUBROUTINE INITIALIZES CROP MANAGEMENT DATA
C**** CALLED IN: MAIN
C     I yr - summer barley, then winter rye
C     II yr & later - winter rye
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C
C      >>>>> COMMON PARAMETERS & VARIABLES
C      fen(5,if)  = amount of min N fertilizers applied, kg N/ha
C      feno(5,if) = amount of org N fertilizers applied, kg N/ha
C      fep(5,if)  = amount of P fertilizers applied, kg P/ha
C      icc        = index for cover crop corr. number in crop database
C      idfe(5,if) = day of fertilization
C      idop(5,io) = day of operation
C      iopc(5,io) = operation code: 1 - planting, 2 - harvest & kill
C                                   3 - harvest,  4 - kill
C      imai       = index for maize corr. number in crop database
C      ipo        = index for potatoes corr. number in crop database
C      irp        = index for w. rape corr. number in crop database
C      isba       = index for s. barley corr. number in crop database
C      iwb        = index for w. barley corr. number in crop database 
C      iwr        = index for w. rye corr. number in crop database
C      iww        = index for w. wheat corr. number in crop database
C      ncrp(5,io) = crop number
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters 
      use common_par
      implicit NONE
      integer iyear

C**** OPERATIONS:
                                  
      if (iyear.gt.1) goto 2      

    1 continue
        write (6,*) '------------>   Crop - s. barley, w. rye plant'
        idop(5,1)=79
        iopc(5,1)=1
        ncrp(5,1)=isba           
        idop(5,2)=218
        iopc(5,2)=2
        ncrp(5,2)=isba           
        idop(5,3)=268
        iopc(5,3)=1
        ncrp(5,3)=iwr
        idop(5,4)=0
        iopc(5,4)=0
        ncrp(5,4)=0         
      idfe(5,1)= 85
      fen(5,1)=  40.
      feno(5,1)= 20.
      fep(5,1)=  23.
      idfe(5,2)= 120
      fen(5,2)=  14.
      feno(5,2)= 6.
      fep(5,2)=  0.
      idfe(5,3)= 150
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 293
      fen(5,4)=  26.
      feno(5,4)= 14.
      fep(5,4)=  0.
      goto 99
               
    2 continue
       write (6,*) '------------>   Crop - winter rye'
        idop(5,1)=198
        iopc(5,1)=2
        ncrp(5,1)=iwr           
        idop(5,2)=199
        iopc(5,2)=1
        ncrp(5,2)=icc
        idop(5,3)=267
        iopc(5,3)=2
        ncrp(5,3)=icc         
        idop(5,4)=268
        iopc(5,4)=1
        ncrp(5,4)=iwr         
      idfe(5,1)= 95
      fen(5,1)=  40.
      feno(5,1)= 20.
      fep(5,1)=  26.
      idfe(5,2)= 110
      fen(5,2)=  0.
      feno(5,2)= 0.
      fep(5,2)=  0.
      idfe(5,3)= 125
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 293
      fen(5,4)=  26.
      feno(5,4)= 14.
      fep(5,4)=  0.
      goto 99
      
   99 continue
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine initcrop_wb(iyear)
C**** PURPOSE: THIS SUBROUTINE INITIALIZES CROP MANAGEMENT DATA
C**** CALLED IN MAIN
C     I yr - summer barley, then winter barley
C     II yr & later - winter barley
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C
C      >>>>> COMMON PARAMETERS & VARIABLES
C      fen(5,if)  = amount of min N fertilizers applied, kg N/ha
C      feno(5,if) = amount of org N fertilizers applied, kg N/ha
C      fep(5,if)  = amount of P fertilizers applied, kg P/ha
C      icc        = index for cover crop corr. number in crop database
C      idfe(5,if) = day of fertilization
C      idop(5,io) = day of operation
C      iopc(5,io) = operation code: 1 - planting, 2 - harvest & kill
C                                   3 - harvest,  4 - kill
C      imai       = index for maize corr. number in crop database
C      ipo        = index for potatoes corr. number in crop database
C      irp        = index for w. rape corr. number in crop database
C      isba       = index for s. barley corr. number in crop database
C      iwb        = index for w. barley corr. number in crop database 
C      iwr        = index for w. rye corr. number in crop database
C      iww        = index for w. wheat corr. number in crop database
C      ncrp(5,io) = crop number
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters 
      use common_par
      implicit NONE
      integer iyear

C**** OPERATIONS:
                                  
      if (iyear.gt.1) goto 2      

    1 continue
        write (6,*) '------------>   Crop - s. barley, w. barley plant'
        idop(5,1)=79
        iopc(5,1)=1
        ncrp(5,1)=isba           
        idop(5,2)=218
        iopc(5,2)=2
        ncrp(5,2)=isba           
        idop(5,3)=258
        iopc(5,3)=1
        ncrp(5,3)=iwb
        idop(5,4)=0
        iopc(5,4)=0
        ncrp(5,4)=0         
      idfe(5,1)= 85
      fen(5,1)=  40.
      feno(5,1)= 20.
      fep(5,1)=  23.
      idfe(5,2)= 120
      fen(5,2)=  14.
      feno(5,2)= 6.
      fep(5,2)=  0.
      idfe(5,3)= 150
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 283
      fen(5,4)=  26.
      feno(5,4)= 14.
      fep(5,4)=  0.
      goto 99
               
    2 continue
       write (6,*) '------------>   Crop - winter barley'
        idop(5,1)=186
        iopc(5,1)=2
        ncrp(5,1)=iwb           
        idop(5,2)=187
        iopc(5,2)=1
        ncrp(5,2)=icc
        idop(5,3)=257
        iopc(5,3)=2
        ncrp(5,3)=icc         
        idop(5,4)=258
        iopc(5,4)=1
        ncrp(5,4)=iwb         
      idfe(5,1)= 95
      fen(5,1)=  40.
      feno(5,1)= 20.
      fep(5,1)=  26.
      idfe(5,2)= 110
      fen(5,2)=  0.
      feno(5,2)= 0.
      fep(5,2)=  0.
      idfe(5,3)= 125
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 283
      fen(5,4)=  26.
      feno(5,4)= 14.
      fep(5,4)=  0.
      goto 99
      
   99 continue
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine initcrop(iyear)
C**** PURPOSE: THIS SUBROUTINE INITIALIZES CROP MANAGEMENT DATA
C**** CALLED IN: MAIN
C     I yr - summer barley, then winter wheat
C     II yr & later - winter wheat
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C
C      >>>>> COMMON PARAMETERS & VARIABLES
C      fen(5,if)  = amount of min N fertilizers applied, kg N/ha
C      feno(5,if) = amount of org N fertilizers applied, kg N/ha
C      fep(5,if)  = amount of P fertilizers applied, kg P/ha
C      icc        = index for cover crop corr. number in crop database
C      idfe(5,if) = day of fertilization
C      idop(5,io) = day of operation
C      iopc(5,io) = operation code: 1 - planting, 2 - harvest & kill
C                                   3 - harvest,  4 - kill
C      imai       = index for maize corr. number in crop database
C      ipo        = index for potatoes corr. number in crop database
C      irp        = index for w. rape corr. number in crop database
C      isba       = index for s. barley corr. number in crop database
C      iwb        = index for w. barley corr. number in crop database 
C      iwr        = index for w. rye corr. number in crop database
C      iww        = index for w. wheat corr. number in crop database
C      ncrp(5,io) = crop number
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters 
      use common_par
      implicit NONE
      integer iyear

C**** OPERATIONS:
      
      if (iyear.gt.1) goto 2      

    1 continue
        write (6,*) '------------>   Crop - s. barley, w. wheat plant'
        idop(5,1)=79
        iopc(5,1)=1
        ncrp(5,1)=isba           
        idop(5,2)=218
        iopc(5,2)=2
        ncrp(5,2)=isba           
        idop(5,3)=283
        iopc(5,3)=1
        ncrp(5,3)=iww
        idop(5,4)=0
        iopc(5,4)=0
        ncrp(5,4)=0         
      idfe(5,1)= 85
      fen(5,1)=  40.
      feno(5,1)= 20.
      fep(5,1)=  23.
      idfe(5,2)= 120
      fen(5,2)=  14.
      feno(5,2)= 6.
      fep(5,2)=  0.
      idfe(5,3)= 150
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 300
      fen(5,4)=  26.
      feno(5,4)= 14.
      fep(5,4)=  0.
      goto 99
               
    2 continue
       write (6,*) '------------>   Crop - winter wheat'
        idop(5,1)=217
        iopc(5,1)=2
        ncrp(5,1)=iww           
        idop(5,2)=218
        iopc(5,2)=1
        ncrp(5,2)=icc
        idop(5,3)=282
        iopc(5,3)=2
        ncrp(5,3)=icc         
        idop(5,4)=283
        iopc(5,4)=1
        ncrp(5,4)=iww         
      idfe(5,1)= 95
      fen(5,1)=  54.
      feno(5,1)= 26.
      fep(5,1)=  28.
      idfe(5,2)= 110
      fen(5,2)=  0.
      feno(5,2)= 0.
      fep(5,2)=  0.
      idfe(5,3)= 125
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 300
      fen(5,4)=  26.
      feno(5,4)= 14.
      fep(5,4)=  0.
      goto 99
      
   99 continue
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine initcrop_wwII(iyear)
C**** PURPOSE: THIS SUBROUTINE INITIALIZES CROP MANAGEMENT DATA
C**** CALLED IN: MAIN
C     I yr - summer barley, then winter wheat II
C     II yr & later - winter wheat II version: doubled amount of fertilizers (C. Hesse) 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C
C      >>>>> COMMON PARAMETERS & VARIABLES
C      fen(5,if)  = amount of min N fertilizers applied, kg N/ha
C      feno(5,if) = amount of org N fertilizers applied, kg N/ha
C      fep(5,if)  = amount of P fertilizers applied, kg P/ha
C      icc        = index for cover crop corr. number in crop database
C      idfe(5,if) = day of fertilization
C      idop(5,io) = day of operation
C      iopc(5,io) = operation code: 1 - planting, 2 - harvest & kill
C                                   3 - harvest,  4 - kill
C      imai       = index for maize corr. number in crop database
C      ipo        = index for potatoes corr. number in crop database
C      irp        = index for w. rape corr. number in crop database
C      isba       = index for s. barley corr. number in crop database
C      iwb        = index for w. barley corr. number in crop database 
C      iwr        = index for w. rye corr. number in crop database
C      iww        = index for w. wheat corr. number in crop database
C      ncrp(5,io) = crop number
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters 
      use common_par
      implicit NONE
      integer iyear

C**** OPERATIONS:
      
      if (iyear.gt.1) goto 2      

    1 continue
        write (6,*) '------------>   Crop - s. barley, w. wheat plant'
        idop(5,1)=79
        iopc(5,1)=1
        ncrp(5,1)=isba           
        idop(5,2)=218
        iopc(5,2)=2
        ncrp(5,2)=isba           
        idop(5,3)=283
        iopc(5,3)=1
        ncrp(5,3)=iww
        idop(5,4)=0
        iopc(5,4)=0
        ncrp(5,4)=0         
      idfe(5,1)= 85
      fen(5,1)=  80.
      feno(5,1)= 40.
      fep(5,1)=  46.
      idfe(5,2)= 120
      fen(5,2)=  28.
      feno(5,2)= 12.
      fep(5,2)=  0.
      idfe(5,3)= 150
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 300
      fen(5,4)=  52.
      feno(5,4)= 28.
      fep(5,4)=  0.
      goto 99
               
    2 continue
       write (6,*) '------------>   Crop - winter wheat'
        idop(5,1)=217
        iopc(5,1)=2
        ncrp(5,1)=iww           
        idop(5,2)=218
        iopc(5,2)=1
        ncrp(5,2)=icc
        idop(5,3)=282
        iopc(5,3)=2
        ncrp(5,3)=icc         
        idop(5,4)=283
        iopc(5,4)=1
        ncrp(5,4)=iww         
      idfe(5,1)= 95
      fen(5,1)=  108.
      feno(5,1)= 52.
      fep(5,1)=  56.
      idfe(5,2)= 110
      fen(5,2)=  0.
      feno(5,2)= 0.
      fep(5,2)=  0.
      idfe(5,3)= 125
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 300
      fen(5,4)=  52.
      feno(5,4)= 28.
      fep(5,4)=  0.
      goto 99
      
   99 continue
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine initcrop_wrape(iyear)
C**** PURPOSE: THIS SUBROUTINE INITIALIZES CROP MANAGEMENT DATA
C**** CALLED IN: MAIN
C     I yr - summer barley, then winter rape
C     II yr & later - winter rape
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C
C      >>>>> COMMON PARAMETERS & VARIABLES
C      fen(5,if)  = amount of min N fertilizers applied, kg N/ha
C      feno(5,if) = amount of org N fertilizers applied, kg N/ha
C      fep(5,if)  = amount of P fertilizers applied, kg P/ha
C      icc        = index for cover crop corr. number in crop database
C      idfe(5,if) = day of fertilization
C      idop(5,io) = day of operation
C      iopc(5,io) = operation code: 1 - planting, 2 - harvest & kill
C                                   3 - harvest,  4 - kill
C      imai       = index for maize corr. number in crop database
C      ipo        = index for potatoes corr. number in crop database
C      irp        = index for w. rape corr. number in crop database
C      isba       = index for s. barley corr. number in crop database
C      iwb        = index for w. barley corr. number in crop database 
C      iwr        = index for w. rye corr. number in crop database
C      iww        = index for w. wheat corr. number in crop database
C      ncrp(5,io) = crop number
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters 
      use common_par
      implicit NONE
      integer iyear

C**** OPERATIONS:
                            
      if (iyear.gt.1) goto 2      

    1 continue
        write (6,*) '------------>   Crop - s. barley, w. rape plant'
        idop(5,1)=79
        iopc(5,1)=1
        ncrp(5,1)=isba           
        idop(5,2)=218
        iopc(5,2)=2
        ncrp(5,2)=isba           
        idop(5,3)=244
        iopc(5,3)=1
        ncrp(5,3)=irp
        idop(5,4)=0
        iopc(5,4)=0
        ncrp(5,4)=0         
      idfe(5,1)= 85
      fen(5,1)=  40.
      feno(5,1)= 20.
      fep(5,1)=  23.
      idfe(5,2)= 120
      fen(5,2)=  14.
      feno(5,2)= 6.
      fep(5,2)=  0.
      idfe(5,3)= 150
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 257
      fen(5,4)=  40.
      feno(5,4)= 20.
      fep(5,4)=  0.
      goto 99
               
    2 continue
       write (6,*) '------------>   Crop - winter rape'
        idop(5,1)=215
        iopc(5,1)=2
        ncrp(5,1)=irp           
        idop(5,2)=218
        iopc(5,2)=1
        ncrp(5,2)=icc
        idop(5,3)=240
        iopc(5,3)=2
        ncrp(5,3)=icc         
        idop(5,4)=244
        iopc(5,4)=1
        ncrp(5,4)=irp         
      idfe(5,1)= 95
      fen(5,1)=  80.
      feno(5,1)= 40.
      fep(5,1)=  27.
      idfe(5,2)= 110
      fen(5,2)=  0.
      feno(5,2)= 0.
      fep(5,2)=  0.
      idfe(5,3)= 125
      fen(5,3)=  0.
      feno(5,3)= 0.
      fep(5,3)=  0.
      idfe(5,4)= 257
      fen(5,4)=  40.
      feno(5,4)= 20.
      fep(5,4)=  0.
      goto 99
      
   99 continue
      return
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
