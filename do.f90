program do
implicit none
integer :: i,n
real :: x, x_min, x_max,step
44 format(F4.2)
x_min = 0.
x_max = 2.0
step = 0.2
n = nint((x_max - x_min)/step
do i = 0,n
x = i*step
write(*,44)x
end do
end program
