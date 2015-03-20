
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRIPTION:                                                               !
!'quadsolvplotmodule.mod' is an external module that runs a subroutine that !
!opens the data file depending if the user wants to see it. If user wnats to!
!to view data file, it is opened in emacs, and all graphing is performed by !
!gnuplot at the discretion of the user.                                      !
!                                                                           !
!ORIGIN INFO:                                                               !
!Code written in gedit (pseudocode) and emacs (hard code)                   !
!Created Monday, September 13, 2013                                         !
!Author: Michael Conroy                                                     !
!Contact: sietekk@gmail.com                                                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module quadsolvplotmodule
	 implicit none

	 !DEFINE VARIABLES
	 integer :: datanow

	 !BODY OF SUBROUTINE
	 contains subroutine graph(x,y,n)
15	 print *, 'Open data file first? ***NOTE: Data file must be closed out before program can continue.'
	 read *, datanow
	 if (datanow==0) then
	 	print *, 'Not opening data file...'
		stop
	 else if (graphoption==1) then
		print *, 'Opening data file...'
		call system('emacs '/media/hdd/SCSU\ Fall\ 2013/PHY\ 499/Code/quadsolvplot.data'')	
	 else !For condition: datfilecreate/=1.or.datfile/=0
		print *, 'WARNING: INVALID INPUT! Please try again...'
		goto 15
	 end if
	 
	 !CALL TO SYSTEM
	call system('gnuplot 'quadsolvplot.data' gnustyle.gnu)
end module quadsolvplotmodule
