program doloop
implicit none
real :: sumx,sum_x2, x_bar, std,x
integer :: n
n = 0
sumx = 0
sum_x2 = 0
do while(x .gt. 0)
write(*,*)'Give the value of x'
read*, x
if (x<0) exit
n = n+1
sumx = sumx+x
sum_x2 = sum_x2+x**2
end do
x_bar = sumx/real(n)
std = (sum_x2/n)**(1/2)
write(*,*) 'mean is =', x_bar ,'standard deviation is = ', std
end program doloop
