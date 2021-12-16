PROGRAM extremes
IMPLICIT NONE
! Data dictionary: declare constants
INTEGER, PARAMETER :: MAXSIZE = 10 ! Max size of data set
! Data dictionary: declare variable types, definitions, & units
INTEGER, DIMENSION(MAXSIZE) :: input ! Input values
INTEGER :: ilarge ! Pointer to largest value
INTEGER :: ismall ! Pointer to smallest value
INTEGER :: j ! DO loop index
INTEGER :: nvals ! Number of vals in data set
INTEGER :: temp ! Temporary variable
! Get number of values in data set
WRITE (*,*) 'Enter number of values in data set:'
READ (*,*) nvals
! Is the number ¡= MAXSIZE?
size: IF ( nvals == MAXSIZE ) THEN
! Get input values.
in: DO J = 1, nvals
WRITE (*,100) 'Enter value ', j
100 FORMAT (A,I3,': ')
READ (*,*) input(j)
END DO in
! Find the largest value.
temp = input(1)
ilarge = 1
large: DO j = 2, nvals
IF ( input(j) > temp ) THEN
temp = input(j)
small: DO j = 2, nvals
end if
IF ( input(j) < temp ) THEN
temp = input(j)
ismall = j
END IF
END DO small
! Write out list.
WRITE (*,110)
110 FORMAT (’The values are:’)
out: DO j = 1, nvals
IF ( j == ilarge ) THEN
WRITE (*,'(I6,2X,A)') input(j), 'LARGEST'
ELSE IF ( J == ismall ) THEN
WRITE (*,'(I6,2X,A)') input(j), 'SMALLEST'
ELSE
WRITE (*,'(I6)') input(j)
END IF
END DO out
ELSE size
! nvals > maxsize. Tell user and quit.
WRITE (*,120) nvals, MAXSIZE
120 FORMAT ('Too many input values: ', I6, ' > ', I6)
END IF size
END PROGRAM extremes 

