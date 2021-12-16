!======================================================
!file: boot_bin.f90
!
!Compile with:
!gfortran -O2 getopt.f90 boot_bin.f90 -o boot_bin
!======================================================
!bootstrap function: you can use this module in
!any of your programs.
!------------------------------------------------------
!                Jackknife binning
!                -----------------
!. Separates the data in BINS jackknife bins
!. For each bin collects SAMPLES bootstrap measurements:
!     + Picks ndat-binw random values from each bin
!     + The average O,O^2, and computed chi is the value of one sample
!. The samples of each bin are averaged to give the value of each jackknife bin
!. Bins are averaged and jackknife errors calculated
!Gives correct results. For 100000 data for |M|, error is
!independent of number of bins for BINS=5, ..., 1000
!(Ising 2d data, L=12, beta=0.36, tau_autoc ~ 8)
!------------------------------------------------------
MODULE bootbin_function
 implicit none
 SAVE
 integer                 :: SAMPLES,MAXDAT,BINS
 character(200)          :: prog
 integer                 :: seed
 CONTAINS
!------------------------------------------------------
!jackknife function:
  subroutine bootstrap(ndat,samples,bins,x,&
       avO,erO,avchi,erchi)
   integer               :: ndat,samples,bins
   real(8),dimension(0:) :: x
   real(8)               :: avO,erO,avchi,erchi,av0all
   integer               :: i,j,k,m,n,b,binw,ndat_in_bin
   real(8),allocatable   :: O(:),O2(:),chi(:),Ob(:),chib(:)
!--------------------------------------
!compute the real average:
   av0all       = SUM(x(0:ndat-1))/ndat
!Number of data in each bin
   binw         = ndat/bins;
   ndat_in_bin  = ndat - binw;
   ALLOCATE(O (0:samples-1));ALLOCATE(O2 (0:samples-1));ALLOCATE(chi(0:samples-1));
   ALLOCATE(Ob(0:bins   -1));ALLOCATE(chib(0:bins   -1));
   Ob     =   0.0D0; chib = 0.0D0;
   O=0.0D0;O2=0.0D0; chi  = 0.0D0;
   do   i = 0,bins-1
    !For each jackknife bin we collect samples samples
    do  j = 0,samples-1
     O(j) = 0.0D0;O2(j) = 0.0D0;
     !Each samples consist of ndat_in_bin "measurements"
     m    = 0
     do while(m<ndat_in_bin)
      k=INT(ndat*drandom())
      if((k/binw) /= i)then   !if k not in the i_th bin, consider the data: 
       O (j) = O (j)   + x(k) !(lazy, improve with computing without wasting)
       O2(j) = O2(j)   + x(k)*x(k)
       m     = m       + 1
      end if
     end do !do while(m<ndat_in_bin)
     !O,O2,chi are the result of each sample
     O   (j)= O   (j) / ndat_in_bin
     O2  (j)= O2  (j) / ndat_in_bin
     chi (j)= O2  (j) - O(j)*O(j)
     !Ob,chib are the jackknife measurements:
     Ob  (i)= Ob  (i) + O(j)
     chib(i)= chib(i) + chi(j)
    end do !j=0,samples-1
    Ob   (i)= Ob  (i) / samples
    chib (i)= chib(i) / samples
   end do  !i=0,bins-1
   !Compute jackknif averages:
   avO    = SUM(Ob  ) / bins
   avchi  = SUM(chib) / bins
   !Compute jackknif errors:
   erO    = sqrt(SUM((Ob  -avO  )*(Ob  -avO  )))
   erchi  = sqrt(SUM((chib-avchi)*(chib-avchi)))
!compute the real avO:
   avO    = av0all
!---------------
   DEALLOCATE(O );DEALLOCATE(O2  );DEALLOCATE(chi)
   DEALLOCATE(Ob);DEALLOCATE(chib)
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
   write(0,'(A,A)') TRIM(prog),':',TRIM(errmes),' Exiting....'
   stop 1
  end subroutine locerr
END MODULE bootbin_function
!======================================================
program bootstrap_errors
 use bootbin_function
 implicit none
 integer             :: ndat,chk
 real(8)             :: O,dO,chi,dchi
 real(8),allocatable :: x(:)
 MAXDAT=1000000;SAMPLES=1000;BINS=20
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
 call bootstrap(ndat,SAMPLES,BINS,x,O,dO,chi,dchi)
 print '(A,I14,A,I12,A)','#  NDAT = ',ndat,' data. SAMPLES = ',SAMPLES,' groups'
 print '(A,I14        )','#  BINS = ',BINS
 print '(A)','# <o>, chi= (<o^2>-<o>^2)'
 print '(A)','# <o> +/- err                             chi +/- err'
 print '(4G28.17)',O,dO,chi,dchi
end program bootstrap_errors
!======================================================
subroutine get_the_options
 use bootbin_function
 use getopt_m    !from getopt.f90
 implicit none
 call getarg(0,prog)

 do
  select case( getopt( "-hs:d:b:" ))
  case( 's' )
   read(optarg,*)SAMPLES
  case( 'b' )
   read(optarg,*)BINS
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
 use bootbin_function
 implicit none
 print '(3A)','Usage: ',TRIM(prog),'  [options]'
 print '( A)','       -s  : No. samples Def. 1000'
 print '( A)','       -b  : No. bins. Samples have size ndata/BINS. Def. 20'
 print '( A,I14)','       -d  : Give the maximum number of data points read.Def. ',MAXDAT
 print '( A)','Computes <o>, chi= (<o^2>-<o>^2)'
 print '( A)','Data is in one column from stdin.'
 print '( A)','---------------------------------'
 print '( A)','Jackknife binning: Separates the data in BINS bins of binw=ndat/BINS.' 
 print '( A)','BINS Jackknife bins with ndat-binw data are constructed.'
 print '( A)','For each bin collects SAMPLES bootstrap measurements. Each measurement consists of ndat-binw random values'
 print '( A)','from each jackknife bin.In the end, the SAMPLES measurements are averaged giving the result for each bin.'
 print '( A)','Then this is jackknife-averaged and jackknife errors are computed.'
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
