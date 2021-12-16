program simpson38
implicit none
real :: x0,xn,h,sum1,integ,fx

integer::i,n
external fx
write(*,*)"give the integration limit value"
read(*,*)x0,xn
write(*,*)"give the number of interval"
read(*,*) n
write(*,*)"give the number of interval"
read(*,*)n
h=(xn-X0)/n
sum1=0.
do i=1,n-1
if((mod(i,3).eq.0)) then
sum1=sum1+2.*fx(x0+i*h)
else
sum1=sum1+3.*fx(x0+i*h)
endif
end do
sum1=sum1+fx(x0)+fx(xn)
integ=sum1*(3.*h/8.)
write(*,*)"the value of integration is",integ
end program

real function fx(x)
real :: x
fx= x**(Z-1)*exp(-x)
end function
