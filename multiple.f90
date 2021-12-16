program multi
implicit none
integer ::  i,j
do i = 1,10
do j = 1,10
write(*,*) i ,'x',j,'=',i*j
end do 
end do
end program
