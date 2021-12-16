program motemd
implicit none
real :: func, integration, sum1,a , b, c, d, x ,y
integer :: seed,i,n
write(*,*) 'the limit of integration for x is a , b '
read(*,*) a , b
write(*,*) 'the limit of integration for y is a , b '
read(*,*) c,d
write(*,*) 'value of n is = '
read(*,*) n
seed = 429454
call srand(seed)
do i = 1,n
x = a + (b-a)*rand()
y = c + (d-c)*rand()
sum1 = sum1 + func(x,y)
end do 
integration = (b-a) * (d-c)*(sum1/real(n))
write(*,*) 'the solution is = ', integration
end program

real function func(x,y)
real :: x,y 
func = x**2+y**2
end function
