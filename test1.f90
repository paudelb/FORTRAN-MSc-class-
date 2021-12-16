program test
implicit none
CHARACTER(LEN=80) :: grade1, grade2, grade3,grade4,grade5, grade6
real :: internal, finale, per_i1, per_f1, per_t1,GPA, GPA2, GPA3,GPA4,per_i2, per_i3,per_i4,per_f2,per_f3
real :: per_f4,per_t2,per_t3,per_t4, per_t5,per_t6,GPA5,GPA6,SGPA,general, electronics
!integer :: 
write(*,*) ' '
write(*,*) 'Welcome to the gradesheet maker for first semester'
write(*,*) ' '
write(*,*)'Please provide the internal and final marks on following compulsary subjects in order (internal,final) '
write(*,*) '1. Quantum Mechanics I'
			write(*,*) ' '
			read(*,*) internal, finale
			per_i1 = (internal/30.)*100.
			per_f1 = (finale/45.)*100.
			if (abs(per_i1 - per_f1)>20) then
			per_i1 = per_f1+20.
			end if 
			per_t1 = (per_i1*30+ per_f1*45)/75
			if (per_t1<50) then
			grade1 = 'F'
			GPA = 2.70
			else if (per_t1 .ge. 50 .and. per_t1<60) then
			grade1 = 'B-'
			GPA = 2.7
			else if (per_t1 .ge. 60 .and. per_t1<70) then
			grade1 = 'B'
			GPA = 3.0
			else if (per_t1 .ge. 70 .and. per_t1<80) then
			grade1 = 'B+'
			GPA = 3.3
			else if (per_t1 .ge. 80 .and. per_t1<90) then
			grade1 = 'A-'
			GPA = 3.7
			else
			grade1 = 'A'
			GPA = 4.0
			end if
write(*,*) '2. Classical Mechanics'
			write(*,*) ' '
			read(*,*) internal, finale
			per_i2 = (internal/30.)*100.
			per_f2 = (finale/45.)*100.
			if (abs(per_i1 - per_f1)>20) then
			per_i2 = per_f2+20.
			end if 
			per_t2 = (per_i2*30+ per_f2*45)/75
			if (per_t2<50) then
			grade2 = 'F'
			GPA2 = 2.70
			else if (per_t2 .ge. 50 .and. per_t2<60) then
			grade2 = 'B-'
			GPA2 = 2.7
			else if (per_t2 .ge. 60 .and. per_t2<70) then
			grade2 = 'B'
			GPA2 = 3.0
			else if (per_t2 .ge. 70 .and. per_t2<80) then
			grade2 = 'B+'
			GPA2 = 3.3
			else if (per_t2 .ge. 80 .and. per_t2<90) then
			grade2 = 'A-'
			GPA2 = 3.7
			else
			grade2 = 'A'
			GPA2 = 4.0
			end if
			write(*,*) ' '
write(*,*) '3. Mathematical Physics I'
		write(*,*) ' '
		read(*,*) internal, finale
		per_i3 = (internal/30.)*100.
		per_f3 = (finale/45.)*100.
		if (abs(per_i3 - per_f3)>20) then
		per_i3 = per_f3+20.
		end if 
		per_t3 = (per_i3*30+ per_f3*45)/75
		if (per_t3<50) then
		grade3 = 'F'
		GPA3 = 2.70
		else if (per_t3 .ge. 50 .and. per_t3<60) then
		grade3 = 'B-'
		GPA3 = 2.7
		else if (per_t3 .ge. 60 .and. per_t3<70) then
		grade3 = 'B'
		GPA3 = 3.0
		else if (per_t3 .ge. 70 .and. per_t3<80) then
		grade3 = 'B+'
		GPA3 = 3.3
		else if (per_t3 .ge. 80 .and. per_t3<90) then
		grade3 = 'A-'
		GPA3 = 3.7
		else
		grade3 = 'A'
		GPA3 = 4.0
		end if
		write(*,*) ' '
write(*,*) '4. Electronics'
write(*,*) ' '
read(*,*) internal, finale
per_i4 = (internal/30.)*100.
per_f4 = (finale/45.)*100.
if (abs(per_i4 - per_f4)>20) then
per_i4 = per_f4+20.
end if 
per_t4 = (per_i4*30+ per_f4*45)/75
if (per_t4<50) then
grade4 = 'F'
GPA4 = 2.70
else if (per_t4 .ge. 50 .and. per_t4<60) then
grade4 = 'B-'
GPA4 = 2.7
else if (per_t4 .ge. 60 .and. per_t4<70) then
grade4 = 'B'
GPA4 = 3.0
else if (per_t4 .ge. 70 .and. per_t4<80) then
grade4 = 'B+'
GPA4 = 3.3
else if (per_t4 .ge. 80 .and. per_t4<90) then
grade4 = 'A-'
GPA4 = 3.7
else
grade4 = 'A'
GPA4 = 4.0
end if

	write(*,*)' '
	write(*,*)'Please provide the marks obtained on following practical subjects out of 50'
	write(*,*) ' '
	write(*,*)'5. General Experiments'
	read *, general
	per_t5 = (general/50)*100
	if (per_t5<50) then
	grade5 = 'F'
	GPA5 = 2.70
	else if (per_t5 .ge. 50 .and. per_t5<60) then
	grade5 = 'B-'
	GPA5 = 2.7
	else if (per_t5 .ge. 60 .and. per_t5<70) then
	grade5 = 'B'
	GPA5 = 3.0
	else if (per_t5 .ge. 70 .and. per_t5<80) then
	grade5 = 'B+'
	GPA5 = 3.3
	else if (per_t5 .ge. 80 .and. per_t5<90) then
	grade5 = 'A-'
	GPA5 = 3.7
	else
	grade5 = 'A'
	GPA5 = 4.0
	end if
write(*,*) ' '
write(*,*)'6. Electronics Experiments'
read *, electronics
per_t6 = (electronics/50)*100
if (per_t6<50) then
grade6 = 'F'
GPA6 = 2.70
else if (per_t6 .ge. 50 .and. per_t6<60) then
grade6 = 'B-'
GPA6 = 2.7
else if (per_t6 .ge. 60 .and. per_t6<70) then
grade6 = 'B'
GPA6 = 3.0
else if (per_t6 .ge. 70 .and. per_t6<80) then
grade6 = 'B+'
GPA6 = 3.3
else if (per_t6 .ge. 80 .and. per_t6<90) then
grade6 = 'A-'
GPA6 = 3.7
else
grade6 = 'A'
GPA6 = 4.0
end if
SGPA = (gpa*3+gpa2*3+gpa3*3+gpa4*3+gpa5*2+gpa6*2)/16

		write(*,*) ' '
		write(*,*) ' '
		write(*,*) ' '
		write(*,*) 'Your Gradesheet is ready '
		write(*,*)' '
		write(*,*) 'S.N.	', 'Subjects   	        ',   	'grade											',    		'GPA'
		write(*,*) ' '
		write(*,*) '1. Quantum Mechanics I	 	    '   ,    	grade1,'', GPA
		write(*,*) '2. Classical Mechanics		    '   ,    	grade2,'',GPA2
		write(*,*) '3. Mathematical Physics	    '   ,    	grade3,'',GPA3
		write(*,*) '4. Electronics			    '   ,        grade4,'',GPA4
		write(*,*) '5. General Experiments	    	    '   ,    	grade5,'',GPA5
		write(*,*) '6. Electronics Experiments         '   ,  	grade6,'',GPA6 
		write(*,*) ' '
		write(*,*) 'SGPA = ', 						sgpa
		
		









end program
