program inter
implicit none
real :: xvalue, yvalue, product1
integer :: i , j , n
real, dimension(10) :: x , y
read(*,*) n
do i = 1,n
read(*,*) x(i), y(i)
end do
read(*,*) xvalue
yvalue = 0
do i = 1,n
product1 = 1.
do j = 1,n
if (i .ne. j) then
product1 = ((xvalue - x(j))/(x(i)-x(j)) ) * product1
end if
end do
yvalue = yvalue + product1*y(i)
end do
write(*,*) 'your result is ', yvalue
end program
