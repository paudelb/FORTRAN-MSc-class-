program torpe
implicit none
real :: x,h,sum1,integration, func,a,b
integer :: i,n
write(*,*) 'please provide the limits of integration a and b in the respective order'
read(*,*) a,b,n
sum1 = 0
h = (b-a)/n
do i = 1, n-1
x = a+i*h
sum1 = sum1 + 2.*func(x)
end do
integration = (h/2.)*(func(a)+func(b)+sum1)
write(*,*) 'the integration value is', integration
end program



real function func(x)
real :: x
func = cos(x)
end function
