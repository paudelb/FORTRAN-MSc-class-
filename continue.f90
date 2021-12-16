program doloop
implicit none
real :: sumx,sum_x2, x_bar, std,x
integer :: n
n = 0
sumx = 0
sum_x2 = 0
do 10 while(x .gt. 0)
write(*,*)'Give the value of x'
read*, x
if (x<0) exit
n = n+1
sumx = sumx+x
sum_x2 = sum_x2+x**2
10 continue
x_bar = sumx/real(n)
std = sqrt((real(n)*sum_x2 - sumx**2) /(real(n)*real(n-1)))
write(*,*) 'mean is =', x_bar ,'standard deviation is = ', std
end program doloop
