!======================================================
!file: jack.f90
!
!Compile with:
!gfortran -O2 getopt.f90 jack.f90 -o jack
!======================================================
!jackknife function: you can use this module in
!any of your programs.
!------------------------------------------------------
!Note: If ndat % jack != 0, then the algorithm works fine.
!      Each bin has ndat-binw data, bins never cross
!      remainder of data (i >= (jack*binw))
MODULE jack_function
 implicit none
 SAVE
 integer                 :: JACK,MAXDAT
 character(200)          :: prog
 CONTAINS
!------------------------------------------------------
!jackknife function:
  subroutine jackknife(ndat,jack,x,&
       avO,erO,avchi,erchi)
   integer               :: ndat,jack !local jack...
   real(8),dimension(0:) :: x
   real(8)               :: avO,erO,avchi,erchi
   integer               :: i,j,binw,bin
   real(8),allocatable   :: O(:),chi(:)
!--------------------------------------
   ALLOCATE(O(0:jack-1));ALLOCATE(chi(0:jack-1))
   O=0.0D0;chi=0.0D0;
   binw=ndat/jack
   if(binw<1)call locerr('jackknife: binw < 1')
!Average value:
   do i=0,ndat-1
    do j=0,jack-1
     if((i/binw) /= j) &
          O  (j) = O  (j) + x(i)
    enddo
   enddo
   O   = O  /(ndat-binw) !normalize
!Susceptibility:
   do i=0,ndat-1
    do j=0,jack-1
     if((i/binw) /= j) &
          chi(j) = chi(j) + (x(i)-O(j))*(x(i)-O(j)) 
    enddo
   enddo
   chi   = chi/(ndat-binw) !normalize
!---------------
   avO   = SUM(O)/jack;avchi=SUM(chi)/jack
   erO   = sqrt(SUM((O  -avO  )*(O  -avO  )))
   erchi = sqrt(SUM((chi-avchi)*(chi-avchi)))
!---------------
   DEALLOCATE(O);DEALLOCATE(chi)
  end subroutine jackknife
!------------------------------------------------------
  subroutine locerr(errmes)
   implicit none
   character(*) :: errmes
   write(0,'(A,A)')TRIM(prog),':',TRIM(errmes),' Exiting....'
   stop 1
  end subroutine locerr
END MODULE jack_function
!======================================================
program jackknife_errors
 use jack_function
 implicit none
 integer             :: ndat,chk
 real(8)             :: O,dO,chi,dchi
 real(8),allocatable :: x(:)
 MAXDAT=1000000;JACK=10
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
 call jackknife(ndat,JACK,x,O,dO,chi,dchi)
 print '(A,I14,A,I12,A)','#  NDAT = ',ndat,' data. JACK = ',JACK,' groups'
 print '(A)','# <o>, chi= (<o^2>-<o>^2)'
 print '(A)','# <o> +/- err                             chi +/- err'
 print '(4G28.17)',O,dO,chi,dchi
end program jackknife_errors
!======================================================
subroutine get_the_options
 use jack_function
 use getopt_m    !from getopt.f90
 implicit none
 call getarg(0,prog)

 do
  select case( getopt( "-hj:d:" ))
  case( 'j' )
   read(optarg,*)JACK
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
 use jack_function
 implicit none
 print '(3A)','Usage: ',TRIM(prog),'  [options]'
 print '( A)','       -j  : No. jack groups Def. 10'
 print '( A,I14)','       -d  : Give the maximum number of data points read.Def. ',MAXDAT
 print '( A)','Computes <o>, chi= (<o^2>-<o>^2)'
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
