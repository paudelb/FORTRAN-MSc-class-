program hey
implicit none
integer::i,j,n,k
real :: sum1, averageM, averageE, sum2,specific_heat,ssum1,averageEsq
real, dimension(20):: x,y
real, dimension(13):: beta
character(len=12) :: fn , fm
beta = [0.01,0.1,0.2,0.3,0.4,0.41,0.43,0.44,0.445,0.46,0.6,0.7,0.8]
n=13 !arbitrary number of files 
do i = 1,n
write(fn,10) i
write(fm,11) i
!write(6,*) fn
open(unit = 1,file = fn)
open(unit = 11,file = fm)
write(6,*) fm
sum1 = 0
ssum1 = 0
sum2 = 0
do j = 1, 9
read(1,*)  x(j),y(j)
sum1 = sum1 +x(j)
ssum1 = ssum1 + x(j)**2
sum2 = sum2 +y(j)
end do
averageE = sum1/9
averageEsq = ssum1/9
averageM = sum2/9
do k= 1,13
specific_heat  = ((averageEsq-averageE**2)*beta(k)**2)/81
!write(11,10) beta(k), specific_heat
write(11,*) beta(k), specific_heat
end do
close(1)
end do
10 format('beta',i2.2,'.dat') 
11 format('cv',i2.2,'.dat') 
 stop 
 end program
 
 
