!======================================================
!file: boot.f90
!
!Compile with:
!gfortran -O2 getopt.f90 boot.f90 -o boot
!======================================================
!bootstrap function: you can use this module in
!any of your programs.
MODULE boot_function
 implicit none
 SAVE
 integer                 :: SAMPLES,MAXDAT
 character(200)          :: prog
 integer                 :: seed
 CONTAINS
!------------------------------------------------------
!jackknife function:
  subroutine bootstrap(ndat,samples,x,&
       avO,erO,avchi,erchi)
   integer               :: ndat,samples !local samples...
   real(8),dimension(0:) :: x
   real(8)               :: avO,erO,avchi,erchi
   integer               :: i,j,k
   real(8),allocatable   :: O(:),O2(:),chi(:)
!--------------------------------------
   ALLOCATE(O(0:samples-1));ALLOCATE(O2(0:samples-1));ALLOCATE(chi(0:samples-1))
   O=0.0D0;O2=0.0D0;chi=0.0D0;
   do  j=0,samples-1
    do i=0,ndat   -1
     k  = INT(ndat*drandom()) !0,...,ndat-1
     O (j) = O (j) + x(k)
     O2(j) = O2(j) + x(k)*x(k)
    enddo
    O  (j) = O(j)/ndat; O2(j) = O2(j)/ndat
    chi(j) = O2(j)-O(j)*O(j)
   enddo
!---------------
   avO   = SUM(O)/samples;avchi=SUM(chi)/samples
   erO   = sqrt(SUM((O  -avO  )*(O  -avO  ))/samples)
   erchi = sqrt(SUM((chi-avchi)*(chi-avchi))/samples)
!compute the real avO:
   avO   = SUM(x(0:ndat-1))/ndat
!---------------
   DEALLOCATE(O);DEALLOCATE(chi)
  end subroutine bootstrap
!------------------------------------------------------
  real(8) function drandom()
   implicit none
   integer,parameter :: a = 16807      ! a = 7**5
   integer,parameter :: m = 2147483647 ! m = a*q+r   = 2**31-1
   integer,parameter :: q = 127773     ! q = [m/a]
   integer,parameter :: r = 2836       ! r = MOD(m,a)
   real(8),parameter :: f = (1.0D0/m)
   integer           :: p
   real(8)           :: dr
101 continue
   p       = seed/q              !  = [seed/q]
   seed    = a*(seed- q*p) - r*p !  = a*MOD(seed,q)-r*[seed/q] = MOD(a*seed,m)
   if(seed .lt. 0) seed = seed + m
   dr      = f*seed
   if( dr .le. 0.0D0 .or. dr .ge. 1.0D0) goto 101
   drandom = dr
  end function drandom
!------------------------------------------------------
  subroutine locerr(errmes)
   implicit none
   character(*) :: errmes
   write(0,'(A,A)')TRIM(prog),':',TRIM(errmes),' Exiting....'
   stop 1
  end subroutine locerr
END MODULE boot_function
!======================================================
program bootstrap_errors
 use boot_function
 implicit none
 integer             :: ndat,chk
 real(8)             :: O,dO,chi,dchi
 real(8),allocatable :: x(:)
 MAXDAT=1000000;SAMPLES=1000
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
 open (28, file="/dev/urandom", access="stream", form="unformatted")
 read (28) seed
 seed = ABS(seed)
 close(28)
 call bootstrap(ndat,SAMPLES,x,O,dO,chi,dchi)
 print '(A,I14,A,I12,A)','#  NDAT = ',ndat,' data. SAMPLES = ',SAMPLES,' groups'
 print '(A)','# <o>, chi= (<o^2>-<o>^2)'
 print '(A)','# <o> +/- err                             chi +/- err'
 print '(4G28.17)',O,dO,chi,dchi
end program bootstrap_errors
!======================================================
subroutine get_the_options
 use boot_function
 use getopt_m    !from getopt.f90
 implicit none
 call getarg(0,prog)

 do
  select case( getopt( "-hs:d:" ))
  case( 's' )
   read(optarg,*)SAMPLES
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
 use boot_function
 implicit none
 print '(3A)','Usage: ',TRIM(prog),'  [options]'
 print '( A)','       -s  : No. samples Def. 1000'
 print '( A,I14)','       -d  : Give the maximum number of data points read.Def. ',MAXDAT
 print '( A)','Computes <o>, chi= (<o^2>-<o>^2)'
 print '( A)','Data is in one column from stdin.'
 stop
end subroutine usage
!=============================================




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
