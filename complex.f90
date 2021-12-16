program quadratic
implicit none
!declare variable types.
real:: a,b,c,root2,root1,D,real_part,imag_part
!prompt the user for the coefficient of the user
write(*,*)"We intend to find the roots of the quadratic equation of the form a*x**2+b*x+c=0."
write(*,*)"Give the value of coefficient a,b,c:"
read(*,*)a,b,c
write(*,*) 'the values  of a, b , c given are ' , a, b, c
!calculate discriminant
D = b**2-4 * a * c

!case1

if (D>0)then !there are two real roots
write(*,*)"This equation has two real and unequal roots"
root1=(-b+sqrt(D))/(2*a)
root2=(-b-sqrt(D))/(2*a)
write(*,*)" The real and unequal roots are:",root1,root2
!caseII
write(*,*) " "
else if(D<0)then !there are complex root
real_part=(-b/2*a)
imag_part=(sqrt ( abs ( D ) ) )/ ( 2 * a )
write(*,*)"This equation has complex roots"
write(*,*)"root1=",real_part," + i", imag_part
write(*,*)"root2=",real_part," - ","i",imag_part
!caseIII
else if (D==0)then !equation has two identical roots
write(*,*) "roots are real and equal"
root1=(-b)/(2*a)
write(*,*)"The real and equal root is =",root1
end if
end program


