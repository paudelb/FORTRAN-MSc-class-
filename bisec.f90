program bisection1
implicit none
real:: a, b, c, fa, fb, fc, fx, x,tol
! a and b are also left and right values
!c is the middle point of a and b
! fa is the f(a),fb is f(b) and fc is f(c)
external fx
integer::n
tol=0.001
!a=0.0
!b=1.60
read(*,*) a,b
!fx=f(x) !since f(x) is an external variable type so we have to define
!external fx !which we define after the program ends.
n=0
!here giving n is to find after how many times of iteration we got the result
111 c=(a+b)/2.
fa=fx(a)
fb=fx(b)
fc=fx(c)
if(fa*fc<0.) then
b=c
 fb=fc
else
a=c 
fa=fc
endif
if ((abs(a-b)/c)<tol) then
! here we set a tolerance value to give nearest value of root
print*, 'the root is' ,c,n
else
n=n+1
goto 111
endif
end program
!since cos(x)-x is an external function so !it should be defined afterthe program ends !or before the program starts
real function fx(x)
real :: x
 !fx = cos(x) - x
 fx = x**2-4
 end function fx


