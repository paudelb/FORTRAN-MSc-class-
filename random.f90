program random1
implicit none
integer :: seed,i
seed = 429454
call srand(seed)
open(unit = 10, file = 'random.dat', action = 'write')
do i = 1,5000
av = a + 100*rand(), 100*rand()
end do 
end program
