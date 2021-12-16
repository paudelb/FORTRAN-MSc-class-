program two
implicit none
real :: x, y ,f
write(*,*) 'please provide the values of x and y in the respective order'
read(*,*) x, y
if (x .ge. 0  .and. y .ge. 0) then
f = x+y
else if (x .ge. 0  .and. y .lt. 0) then
f = x +y**2
else if (x .lt. 0  .and. y .ge. 0) then
f = x**2 + y
else if(x .lt. 0  .and. y .lt. 0) then
f = x**2 + y**2
end if
write(*,*) 'the value of function is =', f
end program
