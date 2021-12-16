program open
implicit none
real :: x,xbar,mean !declaring variables
integer :: i
1 format (f4.2)
open(unit = 10, file = 'data.dat', form ='formatted', status = 'old', action = 'read')
xbar = 0.
do i = 1,9
read(10,1)  x
print*,'data= ', x
xbar = xbar + x
end do
mean = xbar /9
print*,'mean is = ', mean
close(unit = 10)
end program

! start program
! open the file
!start do loop
!read the data
!end do loop
! calculate mean
! display mean
!end program
