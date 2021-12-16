program format3
implicit none
CHARACTER(len=17) :: subject,name1
real :: marks
integer :: i
write(*,*) 'enter name  '
read(*,*) name1
do i = 1,4
WRITE (*,*) 'please provide the subject'
read(*,*) subject
WRITE (*,*) 'plese provide the marks' 
read *, marks
write(*,10) name1, subject, marks 
end do
10 FORMAT (A20, /, A12, f5.2)

end program
