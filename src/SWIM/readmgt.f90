subroutine readmgt(funit)
!-------------------------------------------------------------------------------
! Author  : stefan.liersch@pik-potsdam.de
! Date    : 2009-11-25
! modified: 2009-12-08
!
! PURPOSE : THIS SUBROUTINE READS CROP MANAGEMENT DATA FROM FILE: landman.csv
!
! CALLED  : from program main
!-------------------------------------------------------------------------------

use common_par
implicit none
   integer, intent(in) :: funit
   character(len=1) :: c
   character(len=80) :: ch
   integer :: i=0, j=0

   read(funit,*) c ! reading the header line
   do i = 1, mgt_tot
      read(funit,*) mgt_id(i),ch,mgt_nop(i),mgt_yr(i),mgt_lu_id(i), &
                   ( mgt_idop(i,j),mgt_iopc(i,j),mgt_ncrp(i,j), &
                     mgt_idfe(i,j),mgt_fen(i,j),mgt_feno(i,j),mgt_fep(i,j), &
                     j = 1, mgt_nop(i) &
                   )
   end do
   close(funit)

end subroutine readmgt