program format
implicit none
integer :: i,j,k
real :: x,y,z
i = 1343
j = 3543
k = 3545
x = 1.22435345
y = 3.34545644
z = 3.64267445
write(*,11)  i,j,k,x,y,z
11 format ('wow thats cool',1x, i4,1x,i4,1x,i4,1x,f6.2,1x,f4.2,1x,f4.2)
end program
