program multiplication_table
implicit none
integer :: n,i,sum1,factorial
write(*,*) 'give value of n'
read*, n
sum1 = 0
 do i = 1,n
 sum1 = sum1*i
 factorial= sum1*i
 end do
 print*, 'the sum of first',n,'integer is = ', sum1
end program
