program cyc
 implicit none
 real :: sum
 integer :: i,j,n
 print*, 'give n'
 read *, n
 sum = 0 
 i =1
 10 sum = sum + i
 i = 1+i
 if (i<n) goto 10
 write(*,*) 'the sum of first' , n,' natural number is ' , sum
END PROGRAM 
