program facto
	implicit none
	integer(8) :: factorial,n
	read*, n
	print*, ' factorial of', n, 'is', factorial(n)
end program

recursive function factorial(n) result (fact)
	implicit none
	integer(8) :: n,fact
	if (n==0 .or. n ==1) then
		fact = 1
	else 
		fact = n * factorial(n-1)
	end if
end function 
