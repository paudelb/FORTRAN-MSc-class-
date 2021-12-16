PROGRAM CURRNET
IMPLICIT NONE
INTEGER :: I
REAL :: IO, K,Q,t
real, DIMENSION(17) :: VD,ID
VD = [-1.0,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0.0,0.1,0.2,0.3,0.4,0.5,0.6]
T = 75.
IO = 2.0
K = 1.38* 10**(-23)
Q = 1.602*10**(-19)
DO I = 1,17
ID(I) = IO*(EXP(Q*VD(I)/(K*T))-1 ) 
write(*,*) ID(I)
END DO 
END PROGRAM
