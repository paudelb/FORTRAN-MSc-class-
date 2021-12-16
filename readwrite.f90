program open
implicit none
integer :: i,x,y !declaring variables
open(unit = 10, file = 'cmp.txt', form ='formatted', status = 'old', action = 'readwrite')
! generating data and writing inside the file
read(10,*)  'number = ', x, '	square of the number = ', y
end do
close(unit = 11)
end program

! start program
! open the file
!generate data
! write data
! write squares of numbers from 1 to 10
! end do loop
!end program
