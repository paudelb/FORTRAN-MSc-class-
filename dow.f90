program trun
implicit none  
real :: x , fun, h, f1,f2,error
integer :: i
x = 1
f2 = atan(x)
do i = 1,10000
h = 0.001*i
f1 = (fun(x+h)-fun(x))/h
error = f2-f1
write(*,*) h,error
end do


end program




real function fun(x)
real :: x
fun = atan(x)
end function

    
