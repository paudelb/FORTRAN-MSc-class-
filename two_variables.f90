program two
implicit none
!declaring variables
real :: x,y,f
write(*,*) 'Give the value of x and y'
read*, x,y
write(*,*) 'The values of x and y are', x, 'and' ,y
!if statements
if (x>= 0 ) then
if (y>=0) then
f = x+y
else if(x>=0 .and. y<0) then
f = x+y**2
else if(x<0 .and. y>=0) then
f = x**2+y
else 
f = x**2+y**2
end if
!writing my result
write(*,*) 'The value of f is', f
end program
