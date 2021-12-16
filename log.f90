program loga
implicit none
real :: x,y
1 write(*,*) 'please provide the value of x'
read(*,*) x
if (x .le. 1) then
y = log(1/(1-x))
else
y = log(1/(1+x))
end if 
print*, 'the answer is ',y
end program
