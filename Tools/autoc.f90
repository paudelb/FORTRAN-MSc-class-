!======================================================
!file: autoc.f90
!
!Compile with:
!gfortran -O2 getopt.f90 autoc.f90 -o autoc
!======================================================
!Autocorrelation function: you can use this module in
!any of your programs.
MODULE rho_function 
 implicit none
 SAVE
 integer                 :: NMAX,tmax
 character(200)          :: prog
 CONTAINS 
!------------------------------------------------------
!rho is the unnormalized autocorrelation function at t:
  real(8) function rho(x,ndat,t)
   implicit none
   integer               :: ndat,t
   real(8),dimension(0:) :: x
   integer               :: n,t0
   real(8)               :: xav0,xavt,r
!--------------------------------------
   n=ndat-t
   if(n<1) call locerr('rho: n<1')
!Caclulate the two averages: xav0=<x>_0, xavt=<x>_t
   xav0 = SUM( x(0:n-1  )) / n
   xavt = SUM( x(t:n-1+t)) / n
   rho  = SUM((x(0:n-1)-xav0)*(x(t:n-1+t)-xavt))/n
  end function rho
!------------------------------------------------------
  subroutine locerr(errmes)
   implicit none
   character(*) :: errmes
   write(0,'(A,A)')TRIM(prog),':',TRIM(errmes),' Exiting....'
   stop 1
  end subroutine locerr
END MODULE rho_function
!======================================================
program autocorrelations
 USE rho_function
 implicit none
 real(8),allocatable,dimension(:) :: r,tau,x
 real(8)                          :: norm
 integer                          :: i,ndat,t,tcut,chk
!------------------------------------------------------
!Default values for max number of data and max time for
!rho and tau:
 NMAX=2000000;tmax=1000 !NMAX=2e6 requires ~ 2e6*8=16MB 
 call get_the_options
 ALLOCATE(x(0:NMAX-1),STAT=chk)
 if(chk > 0) call locerr('Not enough memory for x')
 ndat=0
 do while ( ndat < NMAX)
  read(*,*,END=101)x(ndat)
  ndat = ndat+1
 enddo ! 
101 continue
 if(ndat >= NMAX) write(0,'(3A,I14,A,I14)')  &
      '# ',TRIM(prog),                       &
      ': Warning: read ndat=', ndat,         &
      ' and reached the limit: ',NMAX
!We decrease tmax if it is comparable or large of ndat
 if(tmax > (ndat/10) ) tmax = ndat/10
!r(t) stores the values of the autocorrelatin function rho(t)
 ALLOCATE(r(0:tmax-1))
 do t=0,tmax-1
  r(t) = rho(x,ndat,t)
 enddo
 norm  = 1.0D0/r(0); r = norm*r
!tau(t) stores integrated autocorrelation times with tcht=t
 ALLOCATE(tau(0:tmax-1))
 do tcut=0,tmax-1
  tau(tcut)=0.0D0
  do t=0,tcut
   tau(tcut) = tau(tcut)+r(t)
  enddo
 enddo
!Output:
 print '(A)','# =========================================================='
 print '(A)','#      Autoc function rho and integrated autoc time tau     '
 print '(A,I12,A,I8)','# ndat= ',ndat,'  tmax= ',tmax
 print '(A)','# t         rho(t)              tau(tcut=t)                 '
 print '(A)','# =========================================================='
 do t=0,tmax-1
  print '(I8,2G28.17)',t,r(t),tau(t)
 enddo
end program autocorrelations
!======================================================
subroutine get_the_options
 use rho_function
 use getopt_m    !from getopt.f90
 implicit none
 call getarg(0,prog)

 do
  select case( getopt( "-ht:n:" ))
  case( 't' )
   read(optarg,*)tmax
  case( 'n' )
   read(optarg,*)NMAX
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
!======================================================
subroutine usage
 use rho_function
 implicit none
 print '(3A)','Usage: ',TRIM(prog),'  [-t <maxtime>] [-n <ndata>]'
 print '( A)','      Reads data from stdin (one column) and computes autocorrelation'
 print '( A)','      function and integrated autocorrelation time.'
 stop
end subroutine usage
!======================================================
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
