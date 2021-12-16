program io
implicit none
!declaring variables
real :: X,Y,Z,Result
print*, 'Enter the value of X,Y and Z' !ask for input
read *,  X,Y,Z 
Result = X+Y**2-Z
write(*,*)"The result is ", Result ! displayig output
end program io
