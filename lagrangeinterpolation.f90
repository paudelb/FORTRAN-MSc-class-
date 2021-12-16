program interpolation
implicit none
real :: xi,yi, product1, xvalue, yval
integer :: i, n,j

real , dimension(100) :: x,y
write(*,*) 'please provide the value of n'
read(*,*) n

 
do i = 1, n 
write( *, *) 'please provide the value of x',i, ' and y',i
read(*,*) xi, yi
end do 
!open(unit = 10, file = 'dataa.dat',status = 'old', action = 'read')
!do i = 1,n
!read(10,*) x(i), y(i)
!end do!
write(*,*) 'please provide the value of x for which you want the value of y'
read (*,*) xvalue
yval = 0.
do i = 1,n
product1 = 1.
do j = 1,n
if (i .ne. j) then
product1 = ((xvalue - x(j))/(x(i)- x(j)))*product1
end if	
end do
yval = yval + y(i)*product1
end do
write(*,*) 'the value of y at value of x = ', xvalue, 'is = ', yval
end program

