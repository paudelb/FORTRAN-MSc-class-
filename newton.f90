program newrap
implicit none
real :: xold, f , tol, h, der, xnew
integer :: n,maxit,i
tol = 0.01
maxit = 1000
write(*,*) ' guess your solution' 
read (*,*) xold
do i = 1, maxit
h = -f(xold)/der(xold)
xnew = xold+h
xold = xnew
if ( abs (f(xnew))<tol) then 
write(*,*) 'the solution is ', xnew, i
exit
end if
end do
end program
real function f(x)
real:: x
f = cos(x)-x
end function
real function der(x)
real:: x
der = -sin(x)-1
end function

