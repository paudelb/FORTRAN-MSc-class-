program test
!Purpose:
!this program will ask user to enter the marks obtained by student in different subjects and prints the gradesheet according to the rule of Tribhuvan University.
implicit none
!declaring variables
CHARACTER(LEN=20) :: grade1, grade2, grade3,grade4,grade5, grade6,remark
real :: internal, finale, per_i1, per_f1, per_t1,GPA, GPA2, GPA3,GPA4,per_i2, per_i3,per_i4,per_f2,per_f3
real :: per_f4,per_t2,per_t3,per_t4, per_t5,per_t6,GPA5,GPA6,SGPA,general, electronics
logical :: a
10 Format(F4.2)
!integer :: 
write(*,*) ' '
write(*,*) 'Welcome to the gradesheet maker for first semester'
write(*,*) ' '
!prompt the user asking for input in predefined order
write(*,*)'Please provide the internal and final marks on following compulsary courses in order (internal,final) '
		    11 write(* , *) '1. Quantum Mechanics I' ! Quantum mechanics will be understood as subject 1 and variables are defined on the same basis.

			write(*,*) ' '
			read(*,*) internal, finale ! internal and finale are the marks obtained on internal assessement and final examination.
			
			if ((internal > 30) .or. (finale>45) .or. (internal<0) .or. (finale<0)) then
				write(*,*) 'Obtained mark cannot be greater than full mark or negative '
				write(*,*) 'Please Provide the valid marks'
				go to 11
			end if
			
			per_i1 = (internal/30.)*100. ! changing internal marks to percentage, per_i1 means percentage on subject 1 on internal.
			per_f1 = (finale/45.)*100. !changing final marks to percentage,  per_f1 means percentage on subject 1 on final.
			if (abs(per_i1 - per_f1)>20) then !applying ceiling condition
				per_i1 = per_f1+20.
			end if 
			per_t1 = (per_i1*30+ per_f1*45)/75 ! per_t1 means total percentage on 1.
			if (per_t1<50) then ! calculating grade and gpa on individual subject as per the grading system of university.
				grade1 = 'F' 
				GPA = 0.00
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
			
2 write(*,*) '2. Classical Mechanics'
		write(*,*) ' '
		read(*,*) internal, finale
		if ((internal > 30) .or. (finale>45) .or. (internal<0) .or. (finale<0)) then
			write(*,*) 'Obtained mark cannot be cannot be greater than full mark or negative '
			write(*,*) 'Please Provide the valid marks'
			go to 2
		end if
		per_i2 = (internal/30.)*100.
		per_f2 = (finale/45.)*100.
		if (abs(per_i1 - per_f1)>20) then
			per_i2 = per_f2+20.
		end if 
		per_t2 = (per_i2*30+ per_f2*45)/75
		if (per_t2<50) then
			grade2 = 'F'
			GPA2 = 0.00
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
 3 write(*,*) '3. Mathematical Physics I'
		write(*,*) ' '
		read(*,*) internal, finale
		if ((internal > 30) .or. (finale>45) .or. (internal<0) .or. (finale<0)) then
			write(*,*) 'Obtained mark cannot be cannot be greater than full mark or negative '
			write(*,*) 'Please Provide the valid marks'
			go to 3
		end if
		per_i3 = (internal/30.)*100.
		per_f3 = (finale/45.)*100.
		if (abs(per_i3 - per_f3)>20) then
			per_i3 = per_f3+20.
		end if 
		per_t3 = (per_i3*30+ per_f3*45)/75
		if (per_t3<50) then
			grade3 = 'F'
			GPA3 = 0.0
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
4 write(*,*) '4. Electronics'
write(*,*) ' '
read(*,*) internal, finale
		if ((internal > 30) .or. (finale>45) .or. (internal<0) .or. (finale<0)) then
			write(*,*) 'Obtained mark cannot be cannot be greater than full mark or negative '
			write(*,*) 'Please Provide the valid marks'
			go to 4
		end if
per_i4 = (internal/30.)*100.
per_f4 = (finale/45.)*100.
if (abs(per_i4 - per_f4)>20) then
	per_i4 = per_f4+20.
end if 
per_t4 = (per_i4*30+ per_f4*45)/75
if (per_t4<50) then
	grade4 = 'F'
	GPA4 = 0.00
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
!now asking user for marks on practical subjects, each out of 50.
	write(*,*)' '
	write(*,*)'Please provide the marks obtained on following practical subjects out of 50'
	write(*,*) ' '
5	write(*,*)'5. General Experiments'
	read *, general
	if (general > 50) then
		write(*,*) 'Obtained mark cannot be cannot be greater than full mark '
		write(*,*) 'Please Provide the valid marks'
		go to 5
	end if
	per_t5 = (general/50)*100 !calculating percentage
	if (per_t5<50) then !calculating grade and gpa
		grade5 = 'F'
		GPA5 = 0.
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
6 write(*,*)'6. Electronics Experiments'
read *, electronics
if (electronics > 50) then
	write(*,*) 'Obtained mark cannot be cannot be greater than full mark '
	write(*,*) 'Please Provide the valid marks'
	go to 6
end if
per_t6 = (electronics/50)*100
if (per_t6<50) then
	grade6 = 'F'
	GPA6 = 0.
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
!calculating SGPA which is weighted average of gpa on individual subjects with credit hours as their weights. 
SGPA = (gpa*3+gpa2*3+gpa3*3+gpa4*3+gpa5*2+gpa6*2)/16
a = (grade1 == 'F') .or. (grade2 == 'F' ) .or. (grade3 == 'F') .or. (grade4 == 'F') .or. (grade5 == 'F') .or. (grade6 == 'F')
if (a .eqv. .true. ) then 
	Remark = 'Fail'
	sgpa = 0.00
else
	Remark = 'Pass'
end if
! printing the gradesheet 
write(*,*) ' '
write(*,*) ' '
write(*,*) ' '
write(*,*) 'Your Gradesheet is ready '
write(*,*) '	Institute of Science and Technology'
write(*,*) ' Central Department of Physics'
write(*,*)' 	Tribhuvan University'
write(*,*) ' 			   GRADESHEET'
write(*,*) 'Name - Bipin Paudel'
write(*,*) 'Roll no. 21 '
write(*,*) 'S.N.	', 'Courses   	        ',   	'grade	',    		'GPA'
write(*,*) '____________________________________________________ '
write(*,*) '1. Quantum Mechanics I	 	    ',	grade1,	GPA
write(*,*) '2. Classical Mechanics		    ',	grade2,	GPA2
write(*,*) '3. Mathematical Physics	    ',	grade3,	GPA3
write(*,*) '4. Electronics			    ',	grade4,	GPA4
write(*,*) '5. General Experiments	    	    ',	grade5,	GPA5
write(*,*) '6. Electronics Experiments         ',	grade6,	GPA6 
write(*,*) ' '
write(*,*) '__________________________________________________ '
write(*,*) 'SGPA = ', 						sgpa, 'Result = ' , Remark
		
		
end program
