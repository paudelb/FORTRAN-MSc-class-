program montepi
implicit none
real :: x , y,  pi, error
integer :: seed, i, n, ninside

n = 100000
call srand(343)
ninside = 0 
open(unit = 12 , file = 'hello.dat')

do i = 1,n

x = rand()
y = rand()
if ((x**2+y**2) .lt. 1) then
ninside = ninside +1
end if
pi = 4*ninside/real(i)
error = abs(pi - 4*atan(1.))
write(12,*) i , error
end do
pi = 4*ninside/real(n)
write(*,*) pi

end program












real function f(x)
real :: x
f = exp(x)
end function
