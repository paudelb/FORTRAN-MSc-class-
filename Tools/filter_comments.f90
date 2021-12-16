!===============================================================================
! Program that reads lines of a file disregarding the ones that contain
! the character '#'
!===============================================================================
program filter_hash
 implicit none
 integer :: E,M,eof
 character(10000) :: line

 open(unit=11,file='test')
 do
  read(11,'(A10000)',IOSTAT=eof) line   !read a line as a string
  if(eof /= 0) exit                     !reached end of file
  if(INDEX(line,'#') == 0)then          !if there are no # in line, then process
   read(line,*) E,M
   print *,E,M
  endif
 enddo


end program filter_hash
