program bisection
implicit none
real :: a,b,c, f, tol,d
integer :: n
external f
1 write(*,*) 'please provide the values of a and b' 
read(*,*) a,b
if (f(a)*f(b) > 0 ) then
go to 1
end if
n = 0 
tol = 0.001
2 c = (a+b)/2.
!if (f(c) .lt. tol) then
!go to 1
!end if
if (f(a)*f(c) < 0 ) then 
b = c
else
a = c
end if
d = (a-b)
if (abs(d)<tol ) then 
 write(*,*) 'the solution is found to be' , c,n
else 
n = n+1
go to 2
end if

end program

real function f(x)
real :: x 
f = tanh(4*x)-x

end function
