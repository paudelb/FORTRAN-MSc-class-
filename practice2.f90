program multiplication_table
implicit none
integer :: x,i,j,p,q
print*, 'give the values of p and q '
read *, p ,q
outer1 : if (p<q) then
outer2: do j = 1,10
inner2: do i = 1,10
 x = j*i
 write(*,*) j,  '*', i ,'= ', x
end do inner2
end do outer2
else
outer3: do j = 1,10
inner3: do i = 1,10
 x = j*i
 write(*,*) i,  '*', j ,'= ', x
end do inner3
end do outer3
end if outer1
end program
