program doloop
implicit none
real :: sumx,sum_x2, x_bar, std,x
integer :: n
n = 0
sumx = 0
sum_x2 = 0
10 write(*,*)'Give the value of x'
read*, x 
if (x<0) goto 11
n = n+1
sumx = sumx+x
sum_x2 = sum_x2+x**2
if ((x<0) .eqv. .false.) goto 10
x_bar = sumx/real(n)
std = (sum_x2/n)**(1/2)
11 write(*,*) '|n = ', n,  '|mean is =', x_bar ,'|standard deviation is = ', std
end program doloop
