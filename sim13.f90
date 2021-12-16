program inter
implicit none
real ::  xo,xn,h,sum1,x1,f,integration,x
integer :: i, n
read(*,*) xo, xn, n
h = (xn - xo)/n

sum1 = 0 
do i = 1, n
if ( mod (i,3 ) == 0) then
sum1 = sum1 + 2*f(xo+i*h)
else
sum1 = sum1 +3*f(xo+i*h)
end if 
end do
integration = (3*h/8)*(f(xo) + f(xn) + sum1)
write(*,*) 'result is', integration
end program


real function f(x)
real :: x
f = exp(x)
end function
