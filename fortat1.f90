program format1
integer :: i,j,k
real :: x,y,z
i = 18934
j = 52819
k = i*j
x = 2435.343
y = 145*34.344
z = x*y
open(unit = 11, file = 'format.csv', action = 'write')
write(*,*) 'unformatted data are', k,k,z
write(*,10) k,k,z
10 format(i4,i8,f9.3)
end program 
