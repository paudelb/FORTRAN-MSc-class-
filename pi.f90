program pie
implicit none
real ::  pi , x,y, error, pi2
integer :: seed, n , i, ninside
write(*,*) 'how many knives you want to throw ? '
read(*,*) n
ninside = 0 
call srand(98989)
open(unit = 10, file = 'pi.dat')
do i = 1,n
x = rand()
y = rand()
if ((x**2+y**2) .le. 1) then
ninside = ninside +1
pi2 = 4*(ninside/real(i))
error =abs( pi2 - 4*atan(1.))
write(10,*) i, error
end if
end do
pi = 4*(ninside/real(n))
write(*,*) 'value of pi estimated to be ' , pi
end program
