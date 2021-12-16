program one 
implicit none 
real :: radius , area , volume, diameter, pi
write(*,*) 'please provode the value of RADIUS' 
read(*,*) radius
call sphere(radius, area, diameter, volume)
print*, 'area is ', area, 'volume is = ', volume, 'diameter is = ' , diameter
end program

subroutine sphere(radius, area, diameter, volume)
real :: pi
real, intent(in) :: radius
real, intent(out) :: area, volume, diameter
pi = 4*atan(1.0)
area = 4*pi*radius**2
volume = 4*pi*radius**3/3
diameter = 2*radius
end subroutine
