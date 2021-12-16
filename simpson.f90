program simp
implicit none
real :: x0,xn,func,h, sum1, integration, integration_i
integer :: i,n
write(*,*) 'Please provide the values of x0, xn, and n in the respective order : '
read(*,*) x0,xn,n
h = (xn-x0)/n
sum1 = 0.
open( unit = 10, file = 'error_simp.dat')
do i = 1,n
if (mod(i,2) .eq. 0) then 
sum1 = sum1 + 2.*func(x0+i*h)
else
sum1 = sum1 + 4.*func(x0+i*h)
end if
integration_i = (h/3.)*(func(x0)+func(xn)+sum1)
end do
integration = (h/3.)*(func(x0)+func(xn)+sum1)
write(*,*) 'the result is = ' , integration
!write(*,*) rand()
end program

real function func(x)
real :: x
func = exp(x)
end function

