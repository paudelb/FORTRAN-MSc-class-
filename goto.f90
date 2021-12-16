program gotoex
implicit none
integer :: i,j,k,sum1,sum2
real :: x,y,z
sum1 = 0 
do i = 1,100,2
sum1 = sum1 + i
end do
print*, 'sum1 = ', sum1
sum1 = 0 
i=0 
1 sum2= sum2+i
i =i+2
if (i <100) goto 1
print*, 'sum2 =', sum2
end program

