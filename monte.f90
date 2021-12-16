program mote
implicit none
real :: x, func, integration, sum1,a , b, sum2,error
integer :: seed,i,n
write(*,*) 'the limit of integration is a , b '
read(*,*) a , b
write(*,*) 'value of n is = '
read(*,*) n
seed = 429454
call srand(seed)
open( unit = 10, file = 'error.dat')
do i = 1,n
x = a + (b-a)*rand()
sum1 = sum1 + func(x)
sum2 = sum1/real(i)*(b-a)
error = abs (1.0-sum2)
write(10,*) i , error
end do 
integration = (b-a) * (sum1/real(n))
write(*,*) 'the solution is = ', integration
end program

real function func(x)
real :: x 
func = exp(x)
end function
