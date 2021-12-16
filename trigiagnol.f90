program tridiagnol
implicit none
real :: t
integer :: i , n 
real, dimension(3) ::  a, b , c, r, row, beta, x
write(*,*) 'please provide the value of n.' 
read(*,*) n 
do i = 2, n
write(*,*) 'what is the value of a(',i,')'
read(*,*) t
a(i) = t
end do
do i = 1, n
write(*,*) 'what is the value of a(',i,')'
read(*,*) t
b(i) = t
end do
do i = 1, n-1
write(*,*) 'what is the value of a(',i,')'
read(*,*) t
c(i) = t
end do
do i = 1, n
write(*,*) 'what is the value of a(',i,')'
read(*,*) t
r(i) = t
end do


if 





























end program
