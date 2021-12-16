program sr
implicit none
integer::i,n
real::E,avgE,sum1,sum2,M
real::avgm,avgE2,avgM2,delE,delM
real::Cv,Chi
real,parameter::Kb=1.38e-23,beta=0.01
print*,"Enter the number of datas"
read*,n
open (unit=15,file="Ising.dat",action="read")
sum1=0.
sum2=0.
do i=1,n
read(15,*)E,M
sum1=sum1+E
sum2=sum2+M
end do
avgE=sum1/real(n)
avgM=sum2/real(n)
 close(15)
open (unit=16,file="Ising.dat",action="read")
sum1=0.
sum2=0.
do i=1,n
read(16,*)E,M
sum1=sum1+(E**2)
sum2=sum2+(M**2)
end do
avgE2=(sum1/real(n))
avgM2=(sum2/real(n))
 close(16)
print*,""
print*,"Mean value and Mean of square value of Energy and Magnetization is given as:"
print*,""
print*,avgE,avgM,avgE2,avgM2
print*,""
delE=sqrt(avgE2-((avgE)**2))
delM=sqrt(avgM2-((avgM)**2))
 Cv= (delE)**2
 Chi=(delM)**2
 print"(A50,//,2(f14.3))","Heat capacity and Susceptibility are",Cv,Chi
end program
