program test_evs
implicit none
integer , parameter : : P
= 100 ! P= LDA
integer , parameter : : LWORK = 3* P−1
real ( 8 ) : : A ( P , P ) , W ( P ) , WORK ( LWORK )
integer : : N ! DSYEV d i a g o n a l i z e s A(N, N)
integer : : i , j
integer : : LDA , INFO
c h a r a c t e r ( 1 ) : : JOBZ , UPLO
! D e f i n e t h e * * symmetric * * matrix t o be d i a g o n a l i z e d
! The s u b r o u t i n e us e s t h e upper t r i a n g u l a r p a r t (UPLO= ’U’ )
! t h e r e f o r e t h e lower t r i a n g u l a r p a r t needs not t o be d e f i n e d
N=4
A ( 1 , 1 ) = −7.7;
A ( 1 , 2 )= 2 . 1 ; A (2 ,2)= 8 . 3 ;
A ( 1 , 3 ) = −3.7; A ( 2 , 3 ) = −16.; A ( 3 , 3 ) =−12.
A ( 1 , 4 ) = 4 . 4 ; A ( 2 , 4 ) = 4 . 6 ; A ( 3 , 4 ) = −1.04; A ( 4 , 4 ) =−3.7
!We p r i n t t h e matrix A b e f o r e c a l l i n g DSYEV s i n c e i t i s
! destroyed a f t e r the c a l l .
do i =1 , N
do j=i , N
print * , ’A( ’ , i , ’ , ’ , j , ’ )= ’ , A ( i , j )
enddo
enddo
!We ask f o r e i g e n v a l u e s AND e i g e n v e c t o r s ( JOBZ= ’V’ )
JOBZ= ’V ’ ; UPLO= ’U’
print * , ’COMPUTING WITH DSYEV: ’
LDA=P
! n o t i c e t h a t LDA−> P>N ! !
c a l l DSYEV ( JOBZ , UPLO , N , A , LDA , W , WORK , LWORK , INFO )
print * , ’DSYEV: DONE. CHECKING NOW: ’
! I f INFO i s nonzero , then t h e r e i s an e r r o r :
if ( INFO . ne . 0) then
print * , ’DSYEV FAILED . INF0= ’ , INFO
stop
endif
! P r i n t r e s u l t s : W( I ) has t h e e i g e n v a l u e s :
print * , ’DSYEV: DONE . : ’
print * , ’EIGENVALUES OF MATRIX: ’
do i =1 , N
print * , ’LAMBDA( ’ , i , ’ )= ’ , W ( i )
enddo
! E i g e n v e c t o r s a r e i n s t o r e d i n t h e columns o f A:
print * , ’EIGENVECTORS OF MATRIX’
do J =1 , N
print * , ’EIGENVECTOR ’ , j , ’ FOR EIGENVALUE ’ , W ( j )
do i =1 , N
print * , ’V_ ’ , j , ’ ( ’ , i , ’ )= ’ , A ( i , j )
enddo
enddo
end program test_evs



