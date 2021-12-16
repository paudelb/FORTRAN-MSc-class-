PROGRAM Average1
! Declarations for main program
implicit none
REAL:: A,B,C, AV, AVSQ1, AVSQ2,AVERAGE
! Enter the data
!OR you can read the data form a file
A=5.0
B=2.0
C=3.0
! you can give all the inputs i.e. arguments of average function from a file using read(unit = number,*)
! Calculate the average of the numbers
AV = AVERAGE(A,B,C)
AVSQ1=AV**2
!PRINT *,”av=”,AV,AVSQ1
AVSQ2 = AVERAGE(A**2,B**2,C**2)
PRINT *,'The average of the numbers is:',AV,AVSQ1,AVSQ2
END PROGRAM Average1
REAL FUNCTION AVERAGE(X,Y,Z)
REAL:: X,Y,Z,SUM1
SUM1 = X + Y + Z
AVERAGE = SUM1/3.0
END FUNCTION
