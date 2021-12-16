program differential
implicit none
real :: xo, y, func, xval, yo, h, m1,m2,m3,m4,x, dy, error, dyo
integer :: i,n ,j
real , dimension(7) :: a
 a = [0.01,0.005,0.05,0.1,0.2,0.5,1.0]
!write(*,*) 'Please provide the value of xo, h and xval ' 
!read(*,*) xval, h,xo
open ( unit = 10 , file = 'relativeerror.dat')
do j = 1,7
yo = 2.0
xo = 1.
xval =2.
y = 2.0
x = 1.
!h = 0.1
h = a(j)
n = int(((xval-xo)/h)+0.5)
do i = 1,n
dyo = h *func(x, y )
y= y + dyo
x = x +h 
end do 

do i = 1,n
m1 = func(xo,yo)
m2 = func(xo+(h/2.) , yo + (h*m1/2.))
m3 = func(xo+(h/2.) , yo+ (h*m2/2.))
m4 = func(xo+(h/2.) , yo + (h*m3/2.))
dy = (1/6.)* (m1 + 2.*m2 + 2.*m3 + m4)
yo = yo+ dy*h
xo = xo+h
end do 
error = abs(yo-y)
write(10,*) h , error
end do 

end program

real function func(x,y)
real :: x,y
func = 2.0*y/x
end function
