program Eulerdifferential
implicit none
real :: x, y, func, xval, dy,error,h
integer :: i,n ,j	
open ( unit = 10 , file = 'eulererror.dat')
y = 2.0
x = 1.
xval =2.
!h = 0.1
h = 1.

n = int((xval-x)/h+0.5)
do i = 1,n
 
dy = h *func(x, y )
y= y + dy
x = x +h 
!write(*,*) 'for x =' , x, 'and h = ', h(j) , ',  y = ', y
error = abs(y-8)
write(10,*) i,  error
end do 

end program

real function func(x,y)
real :: x,y
func = 2.0*y/x
end function
