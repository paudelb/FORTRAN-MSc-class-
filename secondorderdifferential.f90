program secondorderdifferentialequation
implicit none
real :: x, y, func, xval, dy,error
integer :: i,n ,j	
real , dimension(7) :: h
 h = [0.01,0.005,0.05,0.1,0.2,0.5,1.0]
!write(*,*) 'Please provide the value of xo, h and xval ' 
!read(*,*) xval, h,xo
open ( unit = 10 , file = 'eulererror.dat')
do j = 1,7
y = 2.0
x = 1.
xval =2.
