program rounde
implicit none
real :: x
integer :: i
call srand(2345)
open( unit = 1, file = 'random.dat')
do i = 1, 100000
x = rand()
write(1,*) i , x 
end do
end program
