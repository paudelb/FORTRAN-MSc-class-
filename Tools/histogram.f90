!======================================================
!file: histogram.f90
!
!Compile with:
!gfortran -O2 getopt.f90 histogram.f90 -o hist
!======================================================
!histogram function: you can use this module in
!any of your programs.
!------------------------------------------------------
!
MODULE hist_function
 implicit none
 SAVE
 integer                 :: MAXDAT
 real(8)                 :: BINW
 character(200)          :: prog
 CONTAINS
!------------------------------------------------------
!histogram function:
  subroutine histogram(ndat,BINW,nbins,x,xh,h,xmin,xmax)
   integer               :: ndat,nbins
   real(8),dimension(0:) :: x             !careful, array index starts from 0
   real(8),allocatable   :: xh(:)         !histogram and corresponding x-values. arrays start from 0
   integer,allocatable   :: h(:)
   real(8)               :: BINW
   real(8)               :: xmin,xmax
   integer               :: i,ih
!--------------------------------------
! Determine min and max values:
   xmin = HUGE(xmin);xmax = -HUGE(x)
   do i = 0,ndat-1
    if( xmin > x(i) ) xmin = x(i)
    if( xmax < x(i) ) xmax = x(i)
   end do
!--------------------------------------
! Histogram parameters:
   nbins   = (xmax-xmin)/binw+1
   ALLOCATE(h(0:nbins-1));ALLOCATE(xh(0:nbins-1))
   do ih   = 0,nbins-1
    xh(ih) = xmin + ih * BINW + 0.5D0 * BINW  ! place x in the middle of the bin
   end do
! Build Histogram:
   h       = 0;
   do i    = 0, ndat -1
    ih     = (x(i) - xmin )/BINW
    h(ih)  = h(ih) + 1
   end do
  end subroutine histogram
!------------------------------------------------------
  subroutine locerr(errmes)
   implicit none
   character(*) :: errmes
   write(0,'(A,A)')TRIM(prog),':',TRIM(errmes),' Exiting....'
   stop 1
  end subroutine locerr
 END MODULE hist_function
!======================================================
program histogram_program
 use hist_function
 implicit none
 integer             :: ndat,chk,nbins
 real(8),allocatable :: x(:),xh(:)
 integer,allocatable :: h(:)
 real(8)             :: xmin,xmax
 integer             :: ih
 MAXDAT=1000000;BINW=1.0D0
 call get_the_options
 ALLOCATE(x(0:MAXDAT-1),STAT=chk)
 if(chk > 0) call locerr('Not enough memory for x')
 ndat=0
 do while ( ndat < MAXDAT)
  read(*,*,END=101)x(ndat)
  ndat = ndat+1
 enddo
101 continue
 if(ndat >= MAXDAT) write(0,'(3A,I14,A,I14)') &
      '# ',TRIM(prog),                        &
      ': Warning: read ndat=', ndat,          &
      ' and reached the limit: ',MAXDAT
 call histogram(ndat,BINW,nbins,x,xh,h,xmin,xmax)
 print '(A)',"# ########################################################################"
 print '(A)',"# Histogram:"
 print '(A,I20,A,I10,A,G10.3)',"# ndat= ",ndat," nbins= ",nbins," binw= ",BINW
 print '(A)',"# x:   data value"
 print '(A)',"# H:   no .data in [x-BINW/2,x+BINW/2]"
 print '(A)',"# h:   relative frequency H/ndat"
 print '(A)',"# rho: normalized data s.t. int_0^infty rho(x) dx = 1"
 print '(A)',"# x     Η       h        rho"
 print '(A)',"# ------------------------------------------------------------------------"
 do ih = 0 , nbins - 1
  if(h(ih) > 0 )then
   print '(4G28.16)',xh(ih),h(ih),DBLE(h(ih))/ndat,DBLE(h(ih))/ndat/BINW
  end if
 end do
end program histogram_program
!======================================================
subroutine get_the_options
 use hist_function
 use getopt_m    !from getopt.f90
 implicit none
 call getarg(0,prog)

 do
  select case( getopt( "-hb:d:" ))
  case( 'b' )
   read(optarg,*)BINW
  case( 'd' )
   read(optarg,*)MAXDAT
  case( 'h' )
   call usage
  case( '?' )
   print *, 'unknown option   ', optopt
   stop
  case( char(0)) ! done with options
   exit
  case( '-' )    ! use -- to exit from options
   exit
  case default
   print *, 'unhandled option ', optopt, ' (this is a bug)'
  end select
 enddo

end subroutine get_the_options
!=============================================
subroutine usage
 use hist_function
 implicit none
 print '(3A    )','Usage: ',TRIM(prog),'  [options]'
 print '( A    )','       -b  : Bin width Def. 1'
 print '( A,I14)','       -d  : Give the maximum number of data points read.Def. ',MAXDAT
 print '( A)','Computes histogram of data. Output is x,h(x),h(x)/ndat,f(x)=h(x)/ndat/BINW'
 print '( A)','Data is in one column from stdin.'
 stop
end subroutine usage

!  ---------------------------------------------------------------------
!  Copyright by Konstantinos N. Anagnostopoulos (2004-2014)
!  Physics Dept., National Technical University,
!  konstant@mail.ntua.gr, www.physics.ntua.gr/~konstant
!  
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, version 3 of the License.
!  
!  This program is distributed in the hope that it will be useful, but
!  WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  General Public License for more details.
!  
!  You should have received a copy of the GNU General Public Liense along
!  with this program.  If not, see <http://www.gnu.org/licenses/>.
!  -----------------------------------------------------------------------
