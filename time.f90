program time1 
implicit none
real :: hour, minutes
real :: tim
write(*,*)'Give time in second '
read(*,*) tim
hour = int(tim/(60*60))
tim = tim - hour*60*60
minutes = int(tim/60)
tim = tim - minutes*60
write(*,*) int(hour), 'Hours, ', int(minutes), 'Minutes, ', int(tim), 'seconds'
end program
