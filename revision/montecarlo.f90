program montecarlointegration
real :: x , a,b, integration,sum1,f
integer :: i
write(*,*) 'Please provide the value of a,b,n'
read(*,*) a,b,n
call srand(2345)
sum1 = 0 
do i = 1,n 
x = a +(b-a)*rand()
sum1 = sum1 + f(x)
end do
integration = sum1*(b-a)/n
print*, integration
end program

real function f(x)
real:: x
f = cos(x)
end function 
