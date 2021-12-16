program scientific_format
implicit none
real ::  a = 1.2346e6, b = 0.001, c = -77.7e10
write(*,20) a,b,c
20 format(2es14.4,1x,es12.3)
end program
