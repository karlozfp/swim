!-------------------------------------------------------------------------------
! Author  : stefan.liersch@pik-potsdam.de
! Date    : 2012-11-08
! modified: 2012-11-08
!
! PURPOSE : Reading subbasin numbers and gauge names for output in Q_gauges.csv
!           The number of output gauges is not restricted.
!
! CALLED  : from program main
!-------------------------------------------------------------------------------

subroutine readgaugesout
use common_par
implicit none

   character(len=1) :: c
   integer          :: i,j,status

   write(*,*)
   write(*,*) "Identifying hydrograph storage locations for output gauges..."

   !-------------------------------------------------------------------
   ! read file: data/output.gauges
   read(104,*,IOSTAT=status) c
   if (status/=0) then
      write(*,*) "Error while reading input file output.gauges"
      STOP
   end if
   do i=1,ngaugesout ! ngaugesout calculated in getallo.f90
      read(104,*,IOSTAT=status) gaugesout(i),gaugesout_names(i)
      if (status/=0) then
         write(*,*) "Error while reading input file output.gauges"
         STOP
      end if
   end do
   close(104)
   !-------------------------------------------------------------------

   !-------------------------------------------------------------------
   ! Identify hydrograph storage location for each output gauge
   ! Function: get_ihouts() is defined in common.f90
   ! gaugesouthyd is required to write output of specific subbasins into:
   ! Q_gauges_sel_sub_routed_m3s.csv in subroutine 'add' in file: route.f90
   gaugesouthyd = get_ihouts(ngaugesout, gaugesout) 
   !-------------------------------------------------------------------

   !-------------------------------------------------------------------
   ! write to screen
   write(*,fmt='(3A8)') 'Subbasin',"ihout","Name"
   do i=1,ngaugesout
      write(*,fmt='(2i8,a1,a)')gaugesout(i),gaugesouthyd(i),' ',trim(gaugesout_names(i))
   end do
   write(*,*)
   !-------------------------------------------------------------------

   !-------------------------------------------------------------------
   ! write Q_gauges.csv header line
   write(100,fmt='(a)',advance="NO") 'YEAR,DAY,observed'
   do i = 1, ngaugesout-1
      write(100,fmt='(a1,a)',advance='NO')',',trim(gaugesout_names(i))
   end do
   write(100,fmt='(a1,a)',advance='YES')',',trim(gaugesout_names(ngaugesout))
   !-------------------------------------------------------------------

end subroutine readgaugesout
