program doloop
integer :: i
i = 1
do 
i =i+ 1
write(*,*) 'this is an infinite do loop'
if (i>10) then 
exit
end if  
end do
end program
