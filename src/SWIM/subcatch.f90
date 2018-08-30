
subroutine read_subcatch_def
!-------------------------------------------------------------------------------
! Author  : stefan.liersch@pik-potsdam.de
! Date    : 2010-02-24
! modified: 2010-02-25
!
! PURPOSE : Reading file subcatch.def
!           count number of subcatchments (user defined aggregation of subbasins)
!
! CALLED  : from subroutine main program
!
! ToDo    : - Writing output like pcp etc. at subcatchment level to specific output files
!
!-------------------------------------------------------------------------------
use common_par
implicit none
! read file subcatch.def
! enable assignment of individual subbasin parameters

integer           :: opensucc,readsucc
integer           :: i, id, n
character(len=20), dimension(mb) :: name
character(len=1)  :: c

   ! open file
   open(110,file=trim(inputPath)//'subcatch.def',status='old',IOSTAT=opensucc)
   if ( opensucc /= 0 ) then
      write(*,*)'ERROR! Cannot open file: subcatch.def, does it exist?'
      STOP
   end if

   n = count_nbr_rows(110,.true.) ! n = number of lines in subcatch.def
   if ( n /= mb ) then
      write(*,*)
      write(*,*) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      write(*,*) "YOU ARE SIMULATING WITH A SUBSET OF SUBCATCHMENTS"
      write(*,*) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      write(*,*)
   end if

   ! read file and count number of user defined subcatchments
   read(110,*) c ! read header line
   do i = 1, mb
      read(110,*,IOSTAT=readsucc) id,subcatch_id(id),name(id)
      if (readsucc > 0) then
         write(*,*) "Error while reading file subcatch.def"
         STOP
      else if (readsucc < 0) then
        write(*,*) 'N subbasins found in subcatch.def will be run:',i-1
        exit ! end of file reached, exit do loop
      end if
   end do
   close(110)

   write(*,*) "SUBCATCHMENT PARAMETERS"
   ! allocate arrays
   n_subcatch = MAXVAL(subcatch_id)
   call allocate_subcatch(n_subcatch)
   do i = 1, mb
      if (subcatch_id(i)/=0) then
         subcatch_name(subcatch_id(i)) = name(i)
         subcatch_area(subcatch_id(i)) = subcatch_area(subcatch_id(i)) + sbar(i) !subarea(i)
      end if
   end do

   ! read individual subcatchment bsn parameters
   call read_subcatch_prm

   ! assign parameters at the subcatchment level
   call assign_subcatch
   
end subroutine read_subcatch_def

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


subroutine read_subcatch_prm
use common_par
implicit none
! read file subcatch.prm
! enable assignment of individual subbasin parameters
integer           :: opensucc,readsucc
integer           :: i, id
character(len=1)  :: c

   ! read parameter file subcatch.prm
   open(111,file=trim(inputPath)//'subcatch.prm',status='old',IOSTAT=opensucc)
   if ( opensucc /= 0 ) then
      write(*,*)'ERROR! Cannot open file: subcatch.prm, does it exist?'
      STOP
   end if
   
   read(111,*) c ! read header line
   write(*,*) "subcatch.prm"
   do i = 1, MAXVAL(subcatch_id)
      read(111,*,IOSTAT=readsucc) id,bsn_ecal(id),bsn_thc(id), &
         bsn_roc2(id),bsn_roc4(id),bsn_cncor(id),bsn_sccor(id), &
         bsn_tsnfall(id),bsn_tmelt(id),bsn_smrate(id),bsn_gmrate(id), &
         gw_bff(id),gw_abf(id),gw_delay(id),gw_revapc(id),gw_rchrgc(id),gw_revapmn(id)
      if ( bsn_cncor(id) > 1.25 ) then
         bsn_cncor(id) = 1.25
         write(*,*) ""
         write(*,*) "cncor value out of range! Set to 1.25 !!!"
         write(*,*) ""
      end if
      if ( bsn_cncor(id) < .25 ) then
         bsn_cncor(id) = .25
         write(*,*) ""
         write(*,*) "cncor value out of range! Set to 0.25 !!!"
         write(*,*) ""
      end if

      if (i==1) write(*,fmt="(A154)")"Subcatch      ecal      thc     roc2     roc4    &
         &cncor    sccor   tsnfall   tmelt   smrate   gmrate   bff      abf      delay    revapc   rchrgc  revapmn"
      write(*,fmt='(i10,16f9.4)') id,bsn_ecal(id),bsn_thc(id), &
         bsn_roc2(id),bsn_roc4(id),bsn_cncor(id),bsn_sccor(id), &
         bsn_tsnfall(id),bsn_tmelt(id),bsn_smrate(id),bsn_gmrate(id), &
         gw_bff(id),gw_abf(id),gw_delay(id),gw_revapc(id),gw_rchrgc(id),gw_revapmn(id)
      if (readsucc > 0) then
         write(*,*) "Error while reading file subcatch.prm"
         STOP
      else if (readsucc < 0) then
         write(*,*) 'N subcatchIDs found in subcatch.prm:',i-1
         exit ! end of file reached, exit do loop
      end if
   end do
   close(111)
   
end subroutine read_subcatch_prm

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


subroutine assign_subcatch
!-------------------------------------------------------------------------------
! Author  : stefan.liersch@pik-potsdam.de
! Date    : 2010-02-25
! modified: 2010-02-26
!
! PURPOSE : Assigning individual subbasin parameters read from subcatch.prm
!
! CALLED  : from subroutine read_subcatch_def
!-------------------------------------------------------------------------------
use common_par
implicit none
integer :: i,si

   do i = 1, mb
      si = subcatch_id(i)
      if (si/=0) then
      ! groundwater parameters
         bff(i)     = gw_bff(subcatch_id(i))
         gwht(i)    = .5  ! gw_gwht(subcatch_id(i))
         gwq(i)     = .5  ! gw_gwq(subcatch_id(i))
         abf(i)     = gw_abf(subcatch_id(i))
         syld(i)    = .003 ! gw_syld(subcatch_id(i))
         delay(i)   = gw_delay(subcatch_id(i))
         revapc(i)  = gw_revapc(subcatch_id(i))
         rchrgc(i)  = gw_rchrgc(subcatch_id(i))
         revapmn(i) = gw_revapmn(subcatch_id(i))
         
         ! bsn parameters
         ecal(i)    = bsn_ecal(subcatch_id(i))
         thc(i)     = bsn_thc(subcatch_id(i))
         sccor(i)   = bsn_sccor(subcatch_id(i))
         roc2(i)    = bsn_roc2(subcatch_id(i))
         roc4(i)    = bsn_roc4(subcatch_id(i))
         cncor(i)   = bsn_cncor(subcatch_id(i))
         tsnfall(i) = bsn_tsnfall(subcatch_id(i))
         tmelt(i)   = bsn_tmelt(subcatch_id(i))
         smrate(i)  = bsn_smrate(subcatch_id(i))
         gmrate(i)  = bsn_gmrate(subcatch_id(i))
      end if
   end do

end subroutine assign_subcatch

!-------------------------------------------------------------------------------
subroutine write_subcatch_output
use common_par
implicit none
integer :: i,j,y

   ! WRITE annually aggregated SUBCATCHMENT output FILE 305 'bay_sc.csv'
   write(305,*) "Entire basin"
   write(305,100)
   i = n_subcatch + 1
   do j=1,nbyr
      y = iyr+j-1-nbyr
     write(305,103) y, subcatch_an(j,i,1), subcatch_an(j,i,3), &
         subcatch_an(j,i,5),subcatch_an(j,i,29),subcatch_an(j,i,4), &
         subcatch_an(j,i,6),subcatch_an(j,i,8),subcatch_an(j,i,9), &
         subcatch_an(j,i,11),subcatch_an(j,i,12),subcatch_an(j,i,13),subcatch_an(j,i,15), &
         subcatch_an(j,i,18),subcatch_an(j,i,17),subcatch_an(j,i,20),subcatch_an(j,i,19), &
         subcatch_an(j,i,8)+subcatch_an(j,i,9)+subcatch_an(j,i,15), &
         subcatch_an(j,i,8)+subcatch_an(j,i,9)+subcatch_an(j,i,15)+subcatch_an(j,i,13)
   end do

   do i=1,n_subcatch
      write(305,*) trim(subcatch_name(i))
      write(305,100)
      do j=1,nbyr
         y = iyr+j-1-nbyr
        write(305,103) y, subcatch_an(j,i,1), subcatch_an(j,i,3), &
            subcatch_an(j,i,5), subcatch_an(j,i,29), subcatch_an(j,i,4), &
            subcatch_an(j,i,6), subcatch_an(j,i,8), subcatch_an(j,i,9), &
            subcatch_an(j,i,11), subcatch_an(j,i,12), subcatch_an(j,i,13), subcatch_an(j,i,15), &
            subcatch_an(j,i,18),subcatch_an(j,i,17),subcatch_an(j,i,20),subcatch_an(j,i,19), &
            subcatch_an(j,i,8)+subcatch_an(j,i,9)+subcatch_an(j,i,15), &
            subcatch_an(j,i,8)+subcatch_an(j,i,9)+subcatch_an(j,i,15)+subcatch_an(j,i,13)
      end do
   end do

   write(305,*)""
   write(305,*)""
   write(305,*)"SC_ID   SC_Name m^2     km^2"
   do i=1,n_subcatch
      write(305,*) i,trim(subcatch_name(i)),subcatch_area(i),subcatch_area(i)/10**6
   end do
   write(305,*) "entire ","basin",da*10**6,da

   close(305)

  100 format('YEAR    PREC      SNWMLT       TMN     TMEAN       TMX        RA      SURQ      SUBQ      PERC       PET       AET',&
             5X,'  GWQ     GWRCH    GWSEEP      WYLD     SWIND        3Q    3Q+AET')
  103 format (i4,20f10.1)

end subroutine write_subcatch_output


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
