!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRIPTION:                                                               !
!'quadsolveplot' is a program that solves and graphs quadratic functions    !
!depending on user entered coefficients a, b, and c of the general form     !
!being ax^2 + bx + c = 0. The program will work with the function even if   !
!coefficients equalling zero make the function linear. However, the program !
!will NOT work with unsolvable forms or ones where all real numbers         !
!are the solutions to x.                                                    !
!                                                                           !
!ORIGIN INFO:                                                               !
!Code written in gedit (pseudocode) and emacs (hard code)                   !
!Created Monday, September 13, 2013                                         !
!Author: Michael Conroy                                                     !
!Contact: sietekk@gmail.com                                                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program quadsolvplot
use quadsolvplotmodule !Graphing module
implicit none

!DECLARE VARIABLES, ARRAYS, AND CONSTANTS
!The integers hold single bit values corresponding to choices made by queries
integer :: varcheck !Holds integer values from 1 to 4 according to 
integer :: discrim !Holds value for discriminant
integer :: redoentry !If unsolvable, redoentry will be 1 = new vars, 0 = quit program
integer :: datfilecreate !For creating a graphable data file, either yes = 1 or no = 0
integer :: graphoption !For graphing query, either yes = 1 or no = 0
integer, parameter :: n=101 !For incremented values in x and y data/graphing arrays; 51 => extra place for 0.0 in arrays

!Variables to hold the entered coefficients, real roots, linear roots, and complex roots, and the discriminant (external function)
!a, b, c: user coefficiets; root1, root2: real roots; complex1, complex2: complex roots; linroot (a=0), linroot2 (a=c=0): linear roots
real :: a, b, c, root1, root2, linroot1, linroot2   
complex :: comproot1, comproot2
real, external :: discrimcalcfunc !Computes the determinant

!Declare arrays for data file creation and graphing
real, dimension(n) :: x, y !2D arrays that hold x- and y-axis data

!!Subroutines (Not declared like functions => listed here only for my own benefit)
!!varentrysub(a,b,c)
!!realrootsub(a,b,discrim,root1,root2)
!!complexrootsub(a,b,discrim,complex1,complex2)
!!datafilesub(a,b,c,varcheck,n,x,y)
	
!DATA ENTRY
!Subroutine varentrysub is called on to prompt user to enter coefficients
10 call varentrysub(a,b,c) !10 is the goto location for the COEFFICIENT CHECK section

!COEFFICIENT CHECK
!1) Depending on coefficient values entered and the combinations of those values, a value of 1-7 is
!   given to 'varcheck', which will tell the program how to solve the function 
!2) Detailed description of 'varcheck' values
!   If 1, then a=0 (linear); or 2, then b=0 (quadratic, double root); or 3, then c=0 (quadratic);
!   or 4, then a=b=0 (Unsolvable); or 5, then a=c=0 (linear); or 6, then b=c=0 (quadratic, double root);
!   or 7, then a=b=c=0 (all real numbers are roots)
if (a==0) then !Linear, one root at x=(-c)/b
	varcheck = 1
else if (b==0) then !Quadratic, double root at x=(+/-)sqrt((-c)/a)
	varcheck = 2
else if (c==0) then !Quadratic
	varcheck = 3
else if (a==0.and.b==o) then !Unsolvable, ASK TO RE-ENTER
	varcheck = 4
else if (a==0.and.c==0) then !Linear, one root at x=(-b)
	varcheck = 5
else if (b==0.and.c==0) then !Quadratic, double root at x=0
	varcheck = 6
else !For condition: a==0.and.b==0.and.c==0 = true, where a=b=c=0 (all are roots)
	varcheck = 7
end if

!CALL TO SOLUTION STRUCTURES AND PRINT ROOT(S)
!A case statement calls on a function or subroutine to solve for the discriminant, linear roots,
!real roots, and/or the complex roots. The quadratic function entered according to the
!coefficients is displayed along with the number of and the values of the root(s).
select case (varcheck)
	case (1) !a=0 (linear)
		print *, 'Equation is linear if a=0!'
		print *, 'Solving as a linear equation...'
		linroot1 = (-c)/b
		print *, 'You entered coefficients for the linear function ', b, 'x', ' + ', c, ' = 0,'
		print *, 'and this function has one real root at x = ', linroot1, '.'
11 	case (4) !a=b=0 (unsolvable)
		print *, 'Equation is unsolvable if a=b=0!'
		print *, 'Do you want to re-enter coefficients or quit? ('1'=re-entry, '0'=quit program):'
		read *, redoentry
		if (redoentry==0) then
			print *, 'Quitting now...'
			stop
		else if (redoentry==1) then
			goto 10 !Returns program back to varentrysub() subroutine to re-enter variables
		else !For condition: redoentry/=1.or.redoentry/=0
			print *, 'WARNING: INVALID INPUT! Please try again...'
			goto 11
		end if
	case (5) !a=c=0 (linear)
		print *, 'Equation is linear if a=c=0!'
		print *, 'Solving as a linear equation...'
		linroot1 = (-b)
		print *, 'You entered coefficients for the linear function ', b, 'x', ' + ', c, ' = 0,'
		print *, 'and this function has one real root at x = ', linroot2, '.'
	case (7) !a=b=c=0 (all are roots)
		print *, 'All real numbers are solutions!'
	case default !All quadratic options: cases (2, 3, and 6)
		print *, 'Equation is quadratic!'
		print *, 'Solving as a quadratic equation...'
		discrim = discrimcalcfunc(a,b,c)
		if (discrim>0) then !Real roots
			call realrootsub(a,b,discrim,root1,root2)
			print *, 'You entered coefficients for the quadratic function ', a, 'x^2 + ', b, 'x', ' + ', c, ' = 0,'
			print *, 'and this function has two real roots at x = ', root1, ' and x = ', root2, '.'
		else if (discrim==0) !Works for both b=0 and b=c=0
			call realrootsub(a,b,discrim,root1,root2)
			print *, 'You entered coefficients for the quadratic function ', a, 'x^2 + ', b, 'x', ' + ', c, ' = 0,'
			print *, 'and this function has a double real root at x = ', root1, ' and x = ', root2, '.'
		else !For condition: discrim<0
			call complexrootsub(a,b,discrim,complex1,complex2)
			print *, 'You entered coefficients for the linear function ', a, 'x^2 + ', b, 'x', ' + ', c, ' = 0,'
			print *, 'and this function has two complex roots at x = ', complex1, ' and x = ', complex2, '.'
end select

!DATA FILE CREATION
!Data file creation query
if (varcheck==1.or.varcheck==5) then !Linear
	print *, 'Your function is linear! Do you wish to save graphable data for your function? ('1'=re-entry, '0'=quit program):'
	read *, datfilecreate
12 	if (datfilecreate==0) then
		print *, 'Exiting data file creation...'
		stop
	else if (datfilecreate==1) then
		call datafilesub(a,b,c,varcheck,n,x,y)
	else !For condition: datfilecreate/=1.or.datfile/=0
		print *, 'WARNING: INVALID INPUT! Please try again...'
		goto 12
	end if
else if (varcheck==2.or.varcheck==3.or.varcheck==6) then
	print *, 'Your function is quadratic! Do you wish to save graphable data for your function? ('1'=re-entry, '0'=quit program):'
	read *, datfilecreate
13 	if (datfilecreate==0) then
		print *, 'Exiting data file creation...'
		stop
	else if (datfilecreate==1) then
		print *, 'Creating data file...'
		call datafilesub(a,b,c,varcheck,n,x,y)
	else !For condition: datfilecreate/=1.or.datfile/=0
		print *, 'WARNING: INVALID INPUT! Please try again...'
		goto 13
	end if
else !For condition: varcheck==4.or.varcheck==7
	print *, 'Sorry, your function is neither quadratic nor linear, so data cannot be generated for you.'
	print *, 'Exiting data file creation...'
end if

!GRAPHING THE FUNCTION
14 print *, 'Graph the function with the new data file?  ('1'= yes, '0'= no):'
read *, graphoption
if (graphoption==0) then
	print *, 'Exiting the graphing module...'
	stop
else if (graphoption==1) then
	print *, 'Graphing data...'
	call graph(x,y,n)	
else !For condition: datfilecreate/=1.or.datfile/=0
	print *, 'WARNING: INVALID INPUT! Please try again...'
	goto 14
end if

!END OF THE MAIN PROGRAM
end program quadsolveplot

!!!--------------------------------------------------------------!!!

!SUBROUTINES
subroutine varentrysub(a,b,c)
	implicit none
	
	!DEFINE VARIANLES
	real,intent(inout) :: a,b,c

	!BODY OF SUBROUTINE
	print *, 'This program solves for the roots of a quadratic equation of the form ax^2 + bx + c = 0,'
	print *, 'and will solve for real and complex roots. It will also identify if the equation is'
	print *, 'unsolvable or the roots are all real numbers. The option to save a file with graphable'
	print *, 'data will be presented along with the option to graph the functioned entered.'
	print *, '...'
	print *, 'Please enter the values of the coefficients a, b, and c separated by commas or spaces:'
	read *, a,b,c
	print *, 'You entered the values a = ', a, ', b = ', b, ', c = ', c, '.' !Not sure if this prints variable values inside text
end subroutine varentrysub

!!!--------------------------------------------------------------!!!

subroutine datafilesub(a,b,c,varcheck,n,x,y)
	implicit none
	
	!DEFINE VARIANLES
	real, intent(in) :: a, b, c
	integer, intent(in) :: varcheck, n
	real, dimension(n) intent(inout) :: x,y
	integer :: bitcheck !For internal select case statement
	integer :: i !x,y populator do loop counters

	!BODY OF SUBROUTINE
	if (varcheck==1.or.varcheck==5) checkbit=1
	if (varcheck==2.or.varcheck==3.or.varcheck==6) checkbit=2
	print *, 'Initializing data file creation...'
	do i=-50,50 !Do loop to populate rest of x array
		x(i) = i
	end do
	y(i)=0.0 !Define zero for y values in y array population
	select case (bitcheck)
		case (1) !Linear, data will be constructed accordingly
			do i=-50,50
				y(i)=b*i+c
			end do
			
		case (2) !Quadratic, data will be constructed accordingly
			do i=-50,50
				y(i)=a(i-root1)(i-root2)
			end do
	end select
	open(unit=99,file='quadsolvplot.data',status='replace')
	do i=-50,50
		write(99,*) x(i), ' ', y(i)
	end do
	close(unit=99)
	print *, 'Data arrays generated and saved!'
end subroutine datafilesub

!!!--------------------------------------------------------------!!!

