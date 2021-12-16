program eulerm
implicit none
real :: x , y, xvalue, h,dy,f 
integer :: i,n
read(*,*) h
read(*,*) x , y
read(*,*) xvalue
n = int(((xvalue-x)/h) +0.5)
do i = 1 , n
dy = h * f(x,y)
y = y +dy
x = x + h
end do
write (*,*) 'the yvalue is', y
end program 

real function f(x,y)
real :: x, y
f = 2*y/x
end function
