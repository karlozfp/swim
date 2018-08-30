c ///////////////////////////////////////////////////////////
c //
c //  Swim model engine: closeall
c //  PURPOSE: to close all files if finish
c //
c //  Author:	    Claus Rachimow, PIK
c //  Created on:   11 Septembewr 2008
c //  Modified for SwimCodeVersion 2008
c //  Last on:      10.August 2011
c //  Version:      1.1.2 
c //
c ///////////////////////////////////////////////////////////

C     SUBROUTINES IN THIS FILE        CALLED FROM
C     subroutine closeall		      finish

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      
      subroutine closeall
      use common_par
      implicit NONE

      integer :: i

	close(1)
	close(3)
	close(10)
	close(15)
	close(21)
	close(22)
	close(25)
	close(100)

C**** GENERAL OUTPUT
          close (31)
          close (32)

C**** SUBROUTINES OUTPUT
      if (icurn.eq.1) close(41)
      if (isolt.eq.1) close(42)
      if (itran.eq.1) close(43)
      if (iperc.eq.1) close(44)
      if (ievap.eq.1) close(45)
      if (icrop.eq.1) close(46)
      if (ieros.eq.1) close(47)
      if (inutr.eq.1) close(48)
      if (irout.eq.1) close(49)
      if (iwstr.eq.1) close(50)

C**** HYDROTOPE OUTPUT       
      close(51)
      close(52)
      close(53)
      close(54)

C**** SUBBASIN OUTPUT
      close(61)
      close(62)
	close(63)
	close(64)
	close(65)

C**** BASIN & RIVER OUTPUT
      close(71)
      close(72)
      close(73)
      close(74)
      close(75)
      close(76)

      close(40)
      close(70)
      close(80)

C**** FLOWS OUTPUT   
C     monthly, annual & aver annual flows
      if (ifloa.eq.1) then
        close(77)
        close(78)
        close(79)
      endif

C     monthly flows for 9 soils
      if (iflom.eq.1) then
        close(81)
        close(82)
        close(83)
        close(84)
        close(85)
        close(86)
        close(87)
        close(88)
        close(89)
	endif

C     annual flows for 9 soils
	if (ifloa.eq.1) then
        close(91)
        close(92)
        close(93)
        close(94)
        close(95)
        close(96)
        close(97)
        close(98)
        close(99)
	endif

C**** GIS OUTPUT 
	if ( gis_y > 0 ) then
	   do i=33,39
	      close(i)
	   end do
	end if
	if ( gis_ave > 0 ) then
	   close(101)
	   close(102)
	   close(103)
	   close(105)
	end if
	if ( gis_y > 0 ) then
	   do i=120,129
	      close(i)
	   end do
	end if
	      
C**** CROP OUTPUT
      if (icrop.eq.1) then       
          close(58)
          close(59)
      endif

C**** CROP OUTPUT for Brandenburg
      close(66)
      close(67)

	close(16)

      return
      end
