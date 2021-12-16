!========================================================
!Program that computes scattering cross-section of a central
!force on the plane. The user should first check that the
!parameters used, lead to a free state in the end.
!  **  X20 is the impact parameter b  **
!A 4 ODE system is solved using Runge-Kutta Method
!User must supply derivatives 
!dx1/dt=f1(t,x1,x2,x3,x4) dx2/dt=f2(t,x1,x2,x3,x4) 
!dx3/dt=f3(t,x1,x2,x3,x4) dx4/dt=f4(t,x1,x2,x3,x4) 
!as real(8) functions 
!Output is written in file scatter.dat
!========================================================
program scatter_cross_section
 implicit none
 integer,parameter   :: P=1010000
 real(8),dimension(P):: T,X1,X2,V1,V2
 real(8) :: Ti,Tf,X10,X20,V10,V20
 real(8) :: X20F,dX20 !max impact parameter and step
 integer :: Nt
 integer :: i
 real(8) ::        k1,k2
 common /couplings/k1,k2
 integer, parameter :: Nbins=20
 integer :: index
 real(8) :: angle,bins(Nbins),Npart
 real(8),parameter :: PI     =3.14159265358979324D0
 real(8),parameter :: rad2deg=180.0D0/PI
 real(8),parameter :: dangle =PI/Nbins
 real(8) R,density,dOmega,sigma,sigmatot
!Input:
 print *,'Runge-Kutta Method for 4-ODEs Integration'
 print *,'Enter coupling constants:'
 read  *, k1,k2
 print *,'k1= ',k1,' k2= ',k2
 print *,'Enter Nt,Ti,Tf,X10,X20,V10,V20:'
 read  *,       Nt,Ti,TF,X10,X20,V10,V20
 print *,'Enter final impact parameter X20F and step dX20:'
 read  *, X20F,dX20
 print *,'Nt = ',Nt
 print *,'Time: Initial Ti =',Ti,  ' Final Tf=',Tf
 print *,'           X1(Ti)=',X10, ' X2(Ti)=',X20
 print *,'           V1(Ti)=',V10, ' V2(Ti)=',V20
 print *,'Impact par X20F  =',X20F,' dX20  =',dX20

 open(unit=11,file='scatter.dat')
 bins   = 0.0d0
!The Calculation:
 Npart  = 0.0D0
 X20    = X20 + dX20/2.0D0 !starts in middle of first interval
 do while (X20 .lt. X20F )
  call RK(T,X1,X2,V1,V2,Ti,Tf,X10,X20,V10,V20,Nt)
! Take absolute value due to symmetry:
  angle = DABS(atan2(V2(Nt),V1(Nt))) 
!Output: The final angle. Check if almost constant
  write(11,*) '@ ', X20, angle,&
       DABS(atan2(V2(Nt-50),V1(Nt-50))),&
       k1/V10**2/tan(angle/2.0D0)
!Update histogram:
  index       = int(angle/dangle)+1
!Number of incoming particles per unit time 
!is proportional to radius of ring
!of radius X20, the impact parameter:
  bins(index) = bins(index) +  X20 !db is cancelled from density
  Npart       = Npart       +  X20 !<-- i.e. from here 
  X20         = X20         + dX20
 enddo
!Print scattering cross section:
 R         = X20             !beam radius
 density   = Npart/(PI*R*R)  !beam flux density J
 sigmatot  = 0.0D0           !total cross section
 do i=1,Nbins
  angle    = (i-0.5D0)*dangle
  dOmega   = 2.0D0*PI*sin(angle)*dangle !d(Solid Angle)
  sigma    = bins(i)/(density*dOmega)
  if(sigma.gt.0.0D0) write(11,*) 'ds= ',angle,angle*rad2deg,sigma
  sigmatot = sigmatot + sigma*dOmega
 enddo
 write(11,*) 'sigmatot= ',sigmatot
 close(11)
end program scatter_cross_section
!========================================================
!The velocity functions f1,f2(t,x1,x2,v1,v2)
!========================================================
real(8) function f1(t,x1,x2,v1,v2)
 implicit none
 real(8) :: t,x1,x2,v1,v2
 f1=v1           !dx1/dt= v1
end function f1
!--------------------------------------------------------
real(8) function f2(t,x1,x2,v1,v2)
 implicit none
 real(8) :: t,x1,x2,v1,v2
 f2=v2           !dx2/dt= v2
end function f2
!========================================================
!RK(T,X1,X2,V1,V2,Ti,Tf,X10,X20,V10,V20,Nt) is the driver 
!for the Runge-Kutta integration routine RKSTEP
!Input: Initial and final times Ti,Tf
!       Initial values at t=Ti  X10,X20,V10,V20
!       Number of steps of integration: Nt-1
!       Size of arrays T,X1,X2,V1,V2
!Output: real arrays T(Nt),X1(Nt),X2(Nt),
!                          V1(Nt),V2(Nt) where
!T(1) = Ti X1(1) = X10 X2(1) = X20 V1(1) = V10 V2(1) = V20
!          X1(k) = X1(at t=T(k)) X2(k) = X2(at t=T(k))
!          V1(k) = V1(at t=T(k)) V2(k) = V2(at t=T(k))
!T(Nt)= Tf
!========================================================
subroutine RK(T,X1,X2,V1,V2,Ti,Tf,X10,X20,V10,V20,Nt)
 implicit none
 integer :: Nt
 real(8),dimension(Nt)::T,X1,X2,V1,V2
 real(8) :: Ti ,Tf
 real(8) :: X10,X20
 real(8) :: V10,V20
 real(8) :: dt         
 real(8) :: TS,X1S,X2S !values of time and X1,X2 at given step
 real(8) ::    V1S,V2S 
 integer :: i
!Initialize variables:
 dt     = (Tf-Ti)/(Nt-1)
 T (1)  = Ti
 X1(1)  = X10; X2(1) = X20
 V1(1)  = V10; V2(1) = V20
 TS     = Ti
 X1S    = X10; X2S   = X20
 V1S    = V10; V2S   = V20
!Make RK steps: The arguments of RKSTEP are 
!replaced with the new ones
 do i=2,Nt
  call RKSTEP(TS,X1S,X2S,V1S,V2S,dt)
  T(i)  = TS
  X1(i) = X1S; X2(i) = X2S
  V1(i) = V1S; V2(i) = V2S
 enddo 
end subroutine RK
!========================================================
!Subroutine RKSTEP(t,x1,x2,dt)
!Runge-Kutta Integration routine of ODE
!dx1/dt=f1(t,x1,x2,x3,x4) dx2/dt=f2(t,x1,x2,x3,x4)
!dx3/dt=f3(t,x1,x2,x3,x4) dx4/dt=f4(t,x1,x2,x3,x4)
!User must supply derivative functions:
!real function f1(t,x1,x2,x3,x4)
!real function f2(t,x1,x2,x3,x4) 
!real function f3(t,x1,x2,x3,x4)
!real function f4(t,x1,x2,x3,x4) 
!Given initial point (t,x1,x2) the routine advances it
!by time dt.
!Input : Inital time t    and function values x1,x2,x3,x4
!Output: Final  time t+dt and function values x1,x2,x3,x4
!Careful!: values of t,x1,x2,x3,x4 are overwritten...
!========================================================
subroutine RKSTEP(t,x1,x2,x3,x4,dt)
 implicit none
 real(8) :: t,x1,x2,x3,x4,dt
 real(8) :: f1,f2,f3,f4
 real(8) :: k11,k12,k13,k14,k21,k22,k23,k24
 real(8) :: k31,k32,k33,k34,k41,k42,k43,k44
 real(8) :: h,h2,h6

 h =dt       !h =dt, integration step
 h2=0.5D0*h  !h2=h/2
 h6=h/6.0D0  !h6=h/6
      
 k11=f1(t,x1,x2,x3,x4)
 k21=f2(t,x1,x2,x3,x4)      
 k31=f3(t,x1,x2,x3,x4)
 k41=f4(t,x1,x2,x3,x4)

 k12=f1(t+h2,x1+h2*k11,x2+h2*k21,x3+h2*k31,x4+h2*k41)
 k22=f2(t+h2,x1+h2*k11,x2+h2*k21,x3+h2*k31,x4+h2*k41)
 k32=f3(t+h2,x1+h2*k11,x2+h2*k21,x3+h2*k31,x4+h2*k41)
 k42=f4(t+h2,x1+h2*k11,x2+h2*k21,x3+h2*k31,x4+h2*k41)

 k13=f1(t+h2,x1+h2*k12,x2+h2*k22,x3+h2*k32,x4+h2*k42)
 k23=f2(t+h2,x1+h2*k12,x2+h2*k22,x3+h2*k32,x4+h2*k42)
 k33=f3(t+h2,x1+h2*k12,x2+h2*k22,x3+h2*k32,x4+h2*k42)
 k43=f4(t+h2,x1+h2*k12,x2+h2*k22,x3+h2*k32,x4+h2*k42)

 k14=f1(t+h ,x1+h *k13,x2+h *k23,x3+h *k33,x4+h *k43)
 k24=f2(t+h ,x1+h *k13,x2+h *k23,x3+h *k33,x4+h *k43)
 k34=f3(t+h ,x1+h *k13,x2+h *k23,x3+h *k33,x4+h *k43)
 k44=f4(t+h ,x1+h *k13,x2+h *k23,x3+h *k33,x4+h *k43)

 t =t+h
 x1=x1+h6*(k11+2.0D0*(k12+k13)+k14)
 x2=x2+h6*(k21+2.0D0*(k22+k23)+k24)
 x3=x3+h6*(k31+2.0D0*(k32+k33)+k34)
 x4=x4+h6*(k41+2.0D0*(k42+k43)+k44)

end subroutine RKSTEP
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
