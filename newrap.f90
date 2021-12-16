program newtrap
implicit none
real :: fun, der, x, a, h, tol
integer :: n, iter
tol = 0.01
iter = 100
write(*,*) 'what is your guess for the solution : '
read(*,*) a
n = 0 
1 h = fun(a)/der(a)
x = a - h
if (abs(fun(x)) .gt. tol) then
a = x 
n = n+1

go to 1
else 
write(*,*) 'the solution is' , x ,  n
end if
end program


real function fun(x)
real :: x
fun = tanh(0.5*x)-x
end function
real function der(x)
real :: x
der = (0.5*(1/cosh(0.5*x))*(1/cosh(0.5*x))) - 1
end function 
