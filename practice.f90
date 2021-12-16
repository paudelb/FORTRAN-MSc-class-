program practice
implicit none
!declaring variables
real :: x,y,multiple, result,choice, divide, percentage
integer :: i,j
write(*,*) 'Give the value of x and y'
read(*,*) x,y
!write(*,*) 'Choose any option below'
!write(*,*) 'choose 1 to add'
!write(*,*) 'choose 2 to multiple'
write(*,*) 'give your favorite number'
read(*,*) choice
do i = 1,10
if (choice < 10) then
result = i*(X+Y)
	do j = 1,5
	percentage = result/100
write(*,*) 'the result is ', result,percentage
	end do
else if (choice >10 ) then
multiple = i*(x*y)
write(*,*) 'The result is ', multiple
else 
divide = x/y
write(*,*) ' the result is ', divide
end if
end do
end program
