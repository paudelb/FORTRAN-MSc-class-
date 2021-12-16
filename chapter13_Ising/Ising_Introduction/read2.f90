program hey
	implicit none
	integer::i,j,n,k, nsweep
	real :: sum1,averageM, averageE, sum2,ssum1,averageEsq, averageMsq,ssum2
	real, dimension(200000):: x,y
	real, dimension(13):: beta, specific_heat, susceptibility
	character(len=12) :: fn , fm
	beta = [0.01,0.1,0.2,0.3,0.4,0.41,0.43,0.44,0.445,0.46,0.6,0.7,0.8]
	n=13 ! number of files 
	nsweep = 100000
	
	do i = 1,n
		write(fn,10) i
		write(fm,11) i
		open(unit = 1,file = fn)
		open(unit = 11,file = 'cvdata.dat')
		open(unit = 12,file = 'chidata.dat')
		open(unit = 20, file = fm)
		sum1 = 0.
		ssum1 = 0.
		sum2 = 0.
		ssum2 = 0.
		
		do j = 1, nsweep
			read(1,*)  x(j),y(j)
			sum1 = sum1 +x(j)
			ssum1 = ssum1 + x(j)**2
			sum2 = sum2 +y(j)
			ssum2 = ssum2 + y(j)**2
			write(20, *) j , y(j) 	
		end do
		
		averageMsq = ssum2/nsweep
		averageE = (sum1/nsweep)
		averageEsq = (ssum1/nsweep)
		averageM = sum2/nsweep
		susceptibility(i) = ((averageMsq-averageM**2)*beta(i))/(144.**2)
		specific_heat(i)  = ((averageEsq-averageE**2)*beta(i)**2)/(144.**2)
		write(11,*) beta(i), specific_heat(i)
		write(12,*) beta(i), susceptibility(i)
		
		close(1)
	end do
	
	10 format('beta',i2.2,'.dat') 
	11 format('M',i2.2,'.dat') 
	 stop 
 end program
 
 
