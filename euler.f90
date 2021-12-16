program Eulerdifferential
implicit none
real :: x, y, func, xval, dy,error
integer :: i,n ,j	
real , dimension(7) :: h
 h = [0.001,0.005,0.05,0.1,0.2,0.5,1.0]
!write(*,*) 'Please provide the value of xo, h and xval ' 
!read(*,*) xval, h,xo
open ( unit = 10 , file = 'eulererror.dat')
do j = 1,7
y = 2.0
x = 1.
xval =2.
!h = 0.1


n = int((xval-x)/h(j)+0.5)
do i = 1,n
 
dy = h(j) *func(x, y )
y= y + dy
x = x +h(j) 
end do 
!write(*,*) 'for x =' , x, 'and h = ', h(j) , ',  y = ', y
error = abs(y-8)
write(10,*) h(j),  error
write(*,*) x, y 
end do

end program

real function func(x,y)
real :: x,y
func = 2.0*y/x
end function
