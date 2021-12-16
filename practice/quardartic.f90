program roots
implicit none
real :: a,b,c,d,x,y,x1,x2
11 write(*,*) 'give the values of a , b  and c for your quadratic equation'
read(*,*) a , b, c
if (a .eq. 0) then
write(*,*) 'you idiot, there is no quadratic equation with a = 0'
goto 11
end if
d = b**2 - 4.*a*c
if (d .gt. 0) then
x1 = (-b+sqrt(d))/(2.*a)
x2 = (-b-sqrt(d))/(2.*a)
write(*,*) 'there are two real roots:',x1,'and',x2
else if (d .eq. 0) then
x1 = -b/(2.*a)
x2 = -b/(2.*a)
write(*,*) 'there are two real and identical roots:',x1,'and',x2
else if (d .lt. 0) then
x = -b/(2*a)
y = sqrt(abs(d))/(2*a)
write(*,*) 'the roots are', x,'+i',y, 'and', x, '-i',y
end if
end program

