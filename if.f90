program ifm
implicit none
real :: x,y,z
read(*,*) x,y,z
!x= 30.
!y = 4.
!z = 50.
if (x>y .and. z>x) write(*,*) 'z is greatest fo all'
end program
