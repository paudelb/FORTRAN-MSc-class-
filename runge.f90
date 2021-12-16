program differential
implicit none
real :: xo, y, func, xval, yo, h, m1,m2,m3,m4,x, dy, error
integer :: i,n ,j
real , dimension(4) :: a
a = [0.005,0.10,0.15,0.20]
!write(*,*) 'Please provide the value of xo, h and xval ' 
!read(*,*) xval, h,xo
open ( unit = 10 , file = 'kuttaerror.dat')
do j = 1,4
yo = 1.
xo = 0.
xval =4

h = a(j)
n = int(((xval-xo)/h)+0.5)
do i = 1,n
m1 = func(xo,yo)
m2 = func(xo+(h/2.) , yo + (h*m1/2.))
m3 = func(xo+(h/2.) , yo+ (h*m2/2.))
m4 = func(xo+(h/2.) , yo + (h*m3/2.))
dy = (1/6.)* (m1 + 2.*m2 + 2.*m3 + m4)
yo = yo+ dy*h
xo = xo+h
end do 
error = abs(yo-1)
write(*,*) 'for x =' , xo, 'and h = ', h , ',  y = ', yo
write(10,*) h , error
end do 
end program

real function func(x,y)
real :: x,y
func = x
end function
