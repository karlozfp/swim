C     FILE writhru.f
C
C     SUBROUTINES IN THIS FILE          CALLED FROM
C     subroutine flomon(mo1) 		main
C     subroutine floann			main 
C     subroutine floave			main



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




      subroutine flomon(mo1)
C**** PURPOSE: Monthly flows write 
C     in file 77 - monthly flows for 3 selected HRUs in subbasin inusb
C     in files 81-89: monthly flows for 9 sequential HRUs in subbasin inusb
C**** CALLED IN:  main
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C     PARAMETERS & VARIABLES
C   
C      >>>>> COMMON PARAMETERS & VARIABLES
C      dflow(j,jea,1)  = mon. sum of surface runoff, mm
C      dflow(j,jea,2)  = mon. sum of sub-surafec runoff, mm 
C      dflow(j,jea,3)  = mon. sum of percolation to g-w, mm
C      dflow(j,jea,4)  = mon. sum of evapotranspiration, mm
C      dflow(j,jea,5)  = mon. sum of N loss with surf. runoff (nlch), kg/ha 
C      dflow(j,jea,6)  = mon. sum of N loss with subsurf. runoff (nlch), kg/ha  
C      dflow(j,jea,7)  = mon. sum of N loss with perc. to g-w (nlch), kg/ha 
C      dflow(j,jea,8)  = mon. sum of N plant uptake (nuptake), kg/ha
C      dflow(j,jea,9)  = mon. sum of N denitrification (ncycle), kg/ha 
C      dflow(j,jea,10) = mon. sum of N-NO3 content in soil (5 l.) (nlch), kg/ha 
C      dflow(j,jea,11) = mon. sum of active org N (nlch), kg/ha
C      dflow(j,jea,12) = mon. sum of stabile org N (nlch), kg/ha
C      dflow(j,jea,13) = mon. sum of N-NO3 content in soil (l. 3) (nlch), kg/ha
C      dflow(j,jea,14) = mon. sum of N-NO3 content in soil (l. 4) (nlch), kg/ha
C      dflow(j,jea,15) = mon. sum of N-NO3 content in soil (l. 5) (nlch), kg/ha
C      dflow(j,jea,16) = mon. sum of N miner. from org mat (ncycle), kg/ha 
C      dflow(j,jea,17) = mon. sum of N miner. from humus (ncycle), kg/ha
C      dflow(j,jea,20) = mon. sum of N added as min & org fertil. (fert), kg/ha
C      dfloy(j,jea,1:20) = annual sums, analogue as dflow
C      inuhd           = number of FIRST HRU for writing dflow(), in .cod file 
C      inusb           = number of subbasin for writing dflow(), read in .cod 
C      iy              = current year as counter (1, ..., nbyr)
C      sbp(j)          = monthly  SUM of precipitation in subbasin
C      sbpy(j)         = annual SUM of precipitation in subbasin
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      iu  = local par
C      ix  = local par
C      iz  = local par
C      >>>>>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C**** Include common parameters
      use common_par
      implicit NONE
      integer mo1,iu,ix,iz
 
          write (77,2233) iy,mo1,sbp(inusb),
     *             (dflow(inusb,inuhd+1,iu),iu=1,9)
     *             ,dflow(inusb,inuhd+1,20),
     *             (dflow(inusb,inuhd+1,iu),iu=16,17),
     *             (dflow(inusb,inuhd+5,iu),iu=1,9)
     *             ,dflow(inusb,inuhd+5,20),
     *             (dflow(inusb,inuhd+5,iu),iu=16,17),
     *             (dflow(inusb,inuhd+8,iu),iu=1,9)
     *             ,dflow(inusb,inuhd+8,20),
     *             (dflow(inusb,inuhd+8,iu),iu=16,17)
     
          write (81,2233) iy,mo1,sbp(inusb),
     *     (dflow(inusb,inuhd,iu),iu=1,9),dflow(inusb,inuhd,20), 
     *     (dflow(inusb,inuhd,iu),iu=16,17) 
          write (82,2233) iy,mo1,sbp(inusb),
     *     (dflow(inusb,inuhd+1,iu),iu=1,9),dflow(inusb,inuhd,20), 
     *     (dflow(inusb,inuhd+1,iu),iu= 16,17) 
          write (83,2233) iy,mo1,sbp(inusb),
     *     (dflow(inusb,inuhd+2,iu),iu=1,9),dflow(inusb,inuhd,20), 
     *     (dflow(inusb,inuhd+2,iu),iu= 16,17) 
          write (84,2233) iy,mo1,sbp(inusb),
     *     (dflow(inusb,inuhd+3,iu),iu=1,9),dflow(inusb,inuhd,20), 
     *     (dflow(inusb,inuhd+3,iu),iu= 16,17) 
          write (85,2233) iy,mo1,sbp(inusb),
     *     (dflow(inusb,inuhd+4,iu),iu=1,9),dflow(inusb,inuhd,20), 
     *     (dflow(inusb,inuhd+4,iu),iu= 16,17) 
          write (86,2233) iy,mo1,sbp(inusb),
     *     (dflow(inusb,inuhd+5,iu),iu=1,9),dflow(inusb,inuhd,20), 
     *     (dflow(inusb,inuhd+5,iu),iu= 16,17) 
          write (87,2233) iy,mo1,sbp(inusb),
     *     (dflow(inusb,inuhd+6,iu),iu=1,9),dflow(inusb,inuhd,20), 
     *     (dflow(inusb,inuhd+6,iu),iu= 16,17) 
          write (88,2233) iy,mo1,sbp(inusb),
     *     (dflow(inusb,inuhd+7,iu),iu=1,9),dflow(inusb,inuhd,20), 
     *     (dflow(inusb,inuhd+7,iu),iu= 16,17) 
          write (89,2233) iy,mo1,sbp(inusb),
     *     (dflow(inusb,inuhd+8,iu),iu=1,9),dflow(inusb,inuhd,20), 
     *     (dflow(inusb,inuhd+8,iu),iu= 16,17) 
               
C****     CALC annual flows
          do 281 iz=1,20
          do 285 ix=1,9
            dfloy(inusb,inuhd+ix-1,iz) = dfloy(inusb,inuhd+ix-1,iz)
     *                            + dflow(inusb,inuhd+ix-1,iz)     
  285 continue    
  281 continue
  
          sbpy(inusb) = sbpy(inusb) + sbp(inusb)           
          sbp(inusb) = 0.     

          do 282 iz=1,20
          do 286 ix=1,9
            dflow(inusb,inuhd+ix-1,iz)=0.
  286 continue          
  282 continue 

 2233 format (2i4,10f12.3, 30f12.3)    
      return 
      end



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




      subroutine floann
C**** PURPOSE: Annual flows write 
C**** CALLED IN:  main 
C     in file 78 - annual flows  for 3 selected HRUs in subbasin inusb
C     in files 91-99: annual flows for 9 sequential HRUs in subbasin inusb
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C      >>>>> COMMON PARAMETERS & VARIABLES
C      dflow(j,jea,1)  = mon. sum of surface runoff, mm
C      dflow(j,jea,2)  = mon. sum of sub-surafec runoff, mm 
C      dflow(j,jea,3)  = mon. sum of percolation to g-w, mm
C      dflow(j,jea,4)  = mon. sum of evapotranspiration, mm
C      dflow(j,jea,5)  = mon. sum of N loss with surf. runoff (nlch), kg/ha 
C      dflow(j,jea,6)  = mon. sum of N loss with subsurf. runoff (nlch), kg/ha  
C      dflow(j,jea,7)  = mon. sum of N loss with perc. to g-w (nlch), kg/ha 
C      dflow(j,jea,8)  = mon. sum of N plant uptake (nuptake), kg/ha
C      dflow(j,jea,9)  = mon. sum of N denitrification (ncycle), kg/ha 
C      dflow(j,jea,10) = mon. sum of N-NO3 content in soil (5 l.) (nlch), kg/ha 
C      dflow(j,jea,11) = mon. sum of active org N (nlch), kg/ha
C      dflow(j,jea,12) = mon. sum of stabile org N (nlch), kg/ha
C      dflow(j,jea,13) = mon. sum of N-NO3 content in soil (l. 3) (nlch), kg/ha
C      dflow(j,jea,14) = mon. sum of N-NO3 content in soil (l. 4) (nlch), kg/ha
C      dflow(j,jea,15) = mon. sum of N-NO3 content in soil (l. 5) (nlch), kg/ha
C      dflow(j,jea,16) = mon. sum of N miner. from org mat (ncycle), kg/ha 
C      dflow(j,jea,17) = mon. sum of N miner. from humus (ncycle), kg/ha
C      dflow(j,jea,20) = mon. sum of N added as min & org fertil. (fert), kg/ha
C      dfloy(j,jea,1:20) = annual sums, analogue as dflow
C      dflav(j,jea,1:20) = total period average sums, analogue as dflow
C      inuhd           = number of FIRST HRU for writing dflow(), in .cod file 
C      inusb           = number of subb. for writing dflow(), read in .cod file
C      iy              = current year as counter (1, ..., nbyr)
C      sbpy(j)         = annual SUM of precipitation in subbasin
C      >>>>>

C      >>>>> STATIC PARAMETERS 
C      iu   = local par
C      ix   = local par
C      iz   = local par
C      >>>>>

C**** Include common parameters
      use common_par
      implicit NONE
      integer iu,ix,iz

C****     CALC aver annual concentrations:::
          do 284 iz=10,15
          do 287 ix=1,9
           dfloy(inusb,inuhd+ix-1,iz)=dfloy(inusb,inuhd+ix-1,iz)/ 12.
  287     continue
  284     continue          
          
          write (78,2234) iy,sbpy(inusb),
     *           (dfloy(inusb,inuhd+1,iu),iu=1,9),
     *            dfloy(inusb,inuhd+1,20),
     *           (dfloy(inusb,inuhd+1,iu),iu=16,17),
     *           (dfloy(inusb,inuhd+5,iu),iu=1,9),
     *            dfloy(inusb,inuhd+5,20),
     *           (dfloy(inusb,inuhd+5,iu),iu=16,17),
     *           (dfloy(inusb,inuhd+8,iu),iu=1,9), 
     *            dfloy(inusb,inuhd+8,20),
     *           (dfloy(inusb,inuhd+8,iu),iu=16,17)

          write (91,2234) iy,sbpy(inusb),
     *     (dfloy(inusb,inuhd,iu),iu=1,9),dfloy(inusb,inuhd,20),
     *           (dfloy(inusb,inuhd,iu),iu= 16,17)
          write (92,2234) iy,sbpy(inusb),
     *     (dfloy(inusb,inuhd+1,iu),iu=1,9),dfloy(inusb,inuhd,20),
     *           (dfloy(inusb,inuhd+1,iu),iu= 16,17)
          write (93,2234) iy,sbpy(inusb),
     *     (dfloy(inusb,inuhd+2,iu),iu=1,9),dfloy(inusb,inuhd,20),
     *           (dfloy(inusb,inuhd+2,iu),iu= 16,17)
          write (94,2234) iy,sbpy(inusb),
     *     (dfloy(inusb,inuhd+3,iu),iu=1,9),dfloy(inusb,inuhd,20),
     *           (dfloy(inusb,inuhd+3,iu),iu= 16,17)
          write (95,2234) iy,sbpy(inusb),
     *     (dfloy(inusb,inuhd+4,iu),iu=1,9),dfloy(inusb,inuhd,20),
     *           (dfloy(inusb,inuhd+4,iu),iu= 16,17)
          write (96,2234) iy,sbpy(inusb),
     *     (dfloy(inusb,inuhd+5,iu),iu=1,9),dfloy(inusb,inuhd,20),
     *           (dfloy(inusb,inuhd+5,iu),iu= 16,17)
          write (97,2234) iy,sbpy(inusb),
     *     (dfloy(inusb,inuhd+6,iu),iu=1,9),dfloy(inusb,inuhd,20),
     *           (dfloy(inusb,inuhd+6,iu),iu= 16,17)
          write (98,2234) iy,sbpy(inusb),
     *     (dfloy(inusb,inuhd+7,iu),iu=1,9),dfloy(inusb,inuhd,20),
     *           (dfloy(inusb,inuhd+7,iu),iu= 16,17)
          write (99,2234) iy,sbpy(inusb),
     *     (dfloy(inusb,inuhd+8,iu),iu=1,9),dfloy(inusb,inuhd,20),
     *           (dfloy(inusb,inuhd+8,iu),iu= 16,17)

          do 381 iz=1,20
          do 385 ix=1,9
           dflav(inusb,inuhd+ix-1,iz) = dflav(inusb,inuhd+ix-1,iz)
     *                            + dfloy(inusb,inuhd+ix-1,iz)     
  385 continue    
  381 continue

          sbpy(inusb)=0.          

          do 283 iz=1,20
          do 288 ix=1,9
            dfloy(inusb,inuhd+ix-1,iz)=0.
  288 continue          
  283 continue

 2234 format (i4, 10f12.3, 30f12.3)    
      return
      end


C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      subroutine floave      
C**** PURPOSE:  average annual flow write
C     in file 79 - average annual flows  for 9 HRUs in subbasin inusb
C**** CALLED IN:   main
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C      >>>>> COMMON PARAMETERS & VARIABLES
C      dflow(j,jea,1)  = mon. sum of surface runoff, mm
C      dflow(j,jea,2)  = mon. sum of sub-surafec runoff, mm 
C      dflow(j,jea,3)  = mon. sum of percolation to g-w, mm
C      dflow(j,jea,4)  = mon. sum of evapotranspiration, mm
C      dflow(j,jea,5)  = mon. sum of N loss with surf. runoff (nlch), kg/ha 
C      dflow(j,jea,6)  = mon. sum of N loss with subsurf. runoff (nlch), kg/ha  
C      dflow(j,jea,7)  = mon. sum of N loss with perc. to g-w (nlch), kg/ha 
C      dflow(j,jea,8)  = mon. sum of N plant uptake (nuptake), kg/ha
C      dflow(j,jea,9)  = mon. sum of N denitrification (ncycle), kg/ha 
C      dflow(j,jea,10) = mon. sum of N-NO3 content in soil (5 l.) (nlch), kg/ha 
C      dflow(j,jea,11) = mon. sum of active org N (nlch), kg/ha
C      dflow(j,jea,12) = mon. sum of stabile org N (nlch), kg/ha
C      dflow(j,jea,13) = mon. sum of N-NO3 content in soil (l. 3) (nlch), kg/ha
C      dflow(j,jea,14) = mon. sum of N-NO3 content in soil (l. 4) (nlch), kg/ha
C      dflow(j,jea,15) = mon. sum of N-NO3 content in soil (l. 5) (nlch), kg/ha
C      dflow(j,jea,16) = mon. sum of N miner. from org mat (ncycle), kg/ha 
C      dflow(j,jea,17) = mon. sum of N miner. from humus (ncycle), kg/ha
C      dflow(j,jea,20) = mon. sum of N added as min & org fertil. (fert), kg/ha
C      dfloy(j,jea,1:20) = annual sums, analogue as dflow
C      dflav(j,jea,1:20) = total period average sums, analogue as dflow
C      inuhd           = number of FIRST HRU for writing dflow(), in .cod file 
C      inusb           = number of subb. for writing dflow(), read in .cod file
C      nbyr            = number of year in the simulation period
C      >>>>>

C      >>>>> STATIC PARAMETERS
C      iu  = local par
C      ix  = local par
C      iz  = local par
C      >>>>>

C**** Include common parameters
      use common_par
      implicit NONE
      integer iu,ix,iz      

      do 386 ix=1,9
          write (79,2234) ix,  
     *           (dflav(inusb,inuhd+ix-1,iu)/nbyr,iu=1,9),
     *           (dflav(inusb,inuhd+ix-1,20)/nbyr),
     *           (dflav(inusb,inuhd+ix-1,iu)/nbyr,iu=16,17)
  386 continue

      do 481 iz=1,20
      do 485 ix=1,9
          dflav(inusb,inuhd+ix-1,iz) = 0.     
  485 continue    
  481 continue

 2234 format (i4, 10f12.3, 30f12.3)    
      return
      end


C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



