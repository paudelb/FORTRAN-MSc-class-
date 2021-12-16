program aray
implicit none
integer, dimension(4,4) :: a
integer :: i,j
do i = 1,4
do j = 1,4
!read(*,*) a(i,j)
a(i,j) = i
end do
write(*,*) a(i,1),a(i,2),a(i,3),a(i,4)
end do
 
end program 
 

























