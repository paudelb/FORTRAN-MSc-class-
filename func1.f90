program factoriall
implicit none
integer :: j
integer(8) :: resul, factorial
write(*,*) 'please provide the number '
read*, j
resul = factorial(j)
print*, 'Factorial of', j , 'is', resul
end program factoriall


INTEGER(8) FUNCTION Factorial(n)
IMPLICIT NONE
!Factorial computation Read and return a positive real number
INTEGER, INTENT(IN) :: n
INTEGER(8) :: i, Ans
Ans = 1
DO i = 1, n
Ans = Ans * i
END DO
Factorial = Ans
END FUNCTION Factorial
