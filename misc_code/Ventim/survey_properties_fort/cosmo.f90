! Module to contain all the integrands
module integrands
  use cosmo_def
  type(params) :: theta_dummy
  real :: r_m_dummy
  public :: f, f_r_integrand, f_v_integrand, f_dn_dv_int
contains
  function f_r_integrand(numfun, x) result(val)
    use cosmo_def
    use precision_model
    implicit none
    integer, intent(in) :: numfun
    real, dimension(:), intent(in) :: x
    real, dimension(numfun) :: val
    type(params) :: theta
    real :: f_E, z
    theta = theta_dummy
    z = x(1)
    val(1) = 1./f_E(theta, z)
  end function f_r_integrand

  function f_v_integrand(numfun, x) result(val)
    use cosmo_def
    use precision_model
    implicit none
    integer, intent(in) :: numfun
    real, dimension(:), intent(in) :: x
    real, dimension(numfun) :: val
    type(params) :: theta
    real :: D, E, f_D, f_E, z, f_r
    theta = theta_dummy
    z = x(1)
    D = f_D(theta, z)
    E = f_E(theta, z)
    val = D**2/E
  end function f_v_integrand

!   function f_dn_dv_int(numfun, x) result(val)
!     use cosmo_def
!     use precision_model
!     implicit none
!     integer, intent(in) :: numfun
!     real, dimension(:), intent(in) :: x
!     real, dimension(numfun) :: val
!     type(params) :: theta
!     real :: D, E, f_D, f_E, z, f_r
!     theta = theta_dummy
!     z = x(1)
!     D = f_D(theta, z)
!     E = f_E(theta, z)
!     val = D**2/E
!   end function f_dn_dv_int

!   function f_sigma_int(numfun, x) result(val)
!     use cosmo_def
!     use precision_model
!     implicit none
!     integer, intent(in) :: numfun
!     real, dimension(:), intent(in) :: x
!     real, dimension(numfun) :: val
!     type(params) :: theta
!     real :: D, E, f_D, f_E, k, f_r, r_m, f_W, f_P
!     theta = theta_dummy
!     r_m = r_m_dummy
!     k = x(1)
!     val = f_P(theta, k)*f_W(k,r_m)**2*k**2
!   end function f_sigma_int
end module integrands

! Functions that involve computing integrals
function f_r(theta, z) result(r)
  use cosmo_def
  use precision_model
  use cui                      ! cubpack user interface
  use integrands
  implicit none
  integer, parameter :: n=1, & ! the dimension
       finite_interval = 1, cube = 2
  integer ::   rgtype, neval,  j, key
  type(params), intent(in) :: theta
  real, intent(in) :: z
  real :: r
  real, dimension(1:n,0:n) :: vertices
  real :: integralvalue, abserr, epsrel
  epsrel = 0.1
  RgType = Finite_interval
  Vertices(1,:) = (/ 0. , z /)
  theta_dummy = theta
  call cubatr(n,f_r_integrand,vertices,rgtype,integralvalue,abserr, &
       maxpts=50000,epsrel=epsrel,key=key,neval=neval,job=1)
  r = c/theta%h/H_0*integralvalue/cm_per_Mpc
end function f_r

function f_v(theta, z1, z2) result(v)
  use cosmo_def
  use precision_model
  use cui
  use integrands
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: z1, z2
  real :: v
  integer, parameter :: n = 1, finite_interval = 1, cube = 2
  integer :: rgtype, neval, key
  real, dimension(1:n, 0:n) :: vertices
  real :: integralvalue, abserr, epsrel
  epsrel = 0.1
  RgType = finite_interval
  vertices(1, :) = (/z1, z2/)
  theta_dummy = theta
  call cubatr(n,f_v_integrand,vertices,rgtype,integralvalue,abserr, &
       maxpts=50000,epsrel=epsrel,key=key,neval=neval,job=1)
  v = (c/cm_per_Mpc)/(theta%h*H_0)*integralvalue*4.*4*atan(1.0)
end function f_v

! Functions that do not involve computing integrals
function f_E(theta, z) result(E)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: z
  real :: E
  E = sqrt(theta%Omega_R_0*(1.+z)**4 + &
       &theta%Omega_M_0*(1.+z)**3 + &
       &theta%Omega_lambda_0*(1.+z)**(3*(1.+theta%w)))
end function f_E

function f_D_growth(theta, z) result(D_growth)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: z
  real :: D_growth, f_Omega_M, f_Omega_lambda
  D_growth = &
       &5.*f_Omega_M(theta, z)/(2.*(1.+z))/(f_Omega_M(theta, z)**(4./7.) - &
       &f_Omega_lambda(theta, z) + &
       &(1. + f_Omega_M(theta, z)/2.)*(1. + f_Omega_lambda(theta, z)/70.))
end function f_D_growth

function f_Omega_M(theta, z) result(Omega_M)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: z
  real :: f_E, Omega_M
  Omega_M = theta%Omega_M_0*(1.+z)**3/f_E(theta, z)**2
end function f_Omega_M

function f_Omega_lambda(theta, z) result(Omega_lambda)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: z
  real :: f_E, Omega_lambda
  Omega_lambda = theta%Omega_lambda_0/f_E(theta,z)**2
end function f_Omega_lambda

function f_D_L(theta, z) result(D_L)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: z
  real :: f_D, D_L
  D_L = (1.+z)*f_D(theta, z)
end function f_D_L

function f_D(theta, z) result(D)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: z
  real :: D, Omega_K_0, r, f_r, R_c, f_R_c
  Omega_K_0 = 1. - (theta%Omega_M_0 + theta%Omega_lambda_0 + theta%Omega_R_0)
  r = f_r(theta, z)
  if (Omega_K_0 .lt. -1E-3) then
     R_c = f_R_c(theta)
     D = R_c*sinh(r/R_c)
  else if (Omega_K_0 .gt. 1E3) then
     R_c = f_R_c(theta)
     D = R_c*sin(r/R_c)
  else
     D = r
  end if
end function f_D

function f_R_c(theta) result(R_c)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real :: R_c, Omega_K_0
  Omega_K_0 = 1. - (theta%Omega_M_0 + theta%Omega_lambda_0 + theta%Omega_R_0)
  R_c = c/(H_0*theta%h)/sqrt(abs(Omega_K_0))/cm_per_Mpc
end function f_R_c

function f_P(theta, k) result(P)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: k
  real :: f_T, P
  P = k**1. * f_T(theta, k)**2
end function f_P

function f_T(theta, k) result(T)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: k
  real :: q, f_q, T
  q = f_q(theta, k)
  T = log(1.+2.34*q)/sqrt(sqrt((2.34*q)/(1.+3.89*q+(16.1*q)**2+(5.46*q)**3+(6.71*q)**4)))
end function f_T

function f_q(theta, k) result(q)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: k
  real :: q, Gamma
  Gamma = theta%Omega_M_0*theta%h*exp(-1*theta%Omega_B_0/theta%h**2*(1+sqrt(2.*theta%h)/theta%Omega_M_0))
  q = k/(Gamma*theta%h)
end function f_q

function f_W(k, r_m) result(W)
  use cosmo_def
  implicit none
  real, intent(in) :: k, r_m
  real :: W
  W = 3.*(sin(k*r_m) - k*r_m*cos(k*r_m))/(k*r_m)**3
end function f_W

function f_sigma(theta, M, z) result(sigma)
  use cosmo_def
  use precision_model
  use cui
  use integrands
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: M, z
  real :: sigma
  integer, parameter :: n = 1, finite_interval = 1, cube = 2
  integer :: rgtype, neval, key
  real, dimension(1:n, 0:n) :: vertices
  real :: integralvalue, abserr, epsrel
  real :: k1, k2, r_m, norm, M_8, f_r_m, f_D_growth
  epsrel = 0.1
  RgType = finite_interval
  theta_dummy = theta
  M_8 = 6.D14*theta%Omega_M_0/theta%h
  r_m = f_r_m(theta, M_8)
  k1 = 100./r_m
  k2 = 1./100./r_m
  vertices(1, :) = (/k1, k2/)
!   call cubatr(n,f_sigma_int,vertices,rgtype,integralvalue,abserr, &
!        maxpts=50000,epsrel=epsrel,key=key,neval=neval,job=1)
  norm = 1./integralvalue
  r_m = f_r_m(theta, M)
  k1 = 100./r_m
  k2 = 1./100./r_m
  vertices(1, :) = (/k1, k2/)
!   call cubatr(n,f_sigma_int,vertices,rgtype,integralvalue,abserr, &
!        maxpts=50000,epsrel=epsrel,key=key,neval=neval,job=1)
  integralvalue = norm*integralvalue
  sigma = theta%sigma_8*f_D_growth(theta, z)*integralvalue
end function f_sigma

function f_rho_c(theta) result(rho_c)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real :: rho_c
  rho_c = 3.*(H_0*theta%h)**2/(8.*pi*G)
end function f_rho_c

function f_r_M(theta, M) result(r_M)
  use cosmo_def
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: M
  real :: r_M, f_rho_c
  r_M = (3.*M/(4.*pi*theta%Omega_M_0*f_rho_c(theta)))**(1./3.)
end function f_r_M

function f_lcdm() result(theta)
  use cosmo_def
  implicit none
  type(params) :: theta
  theta%h = 0.7
  theta%w = -1.0
  theta%Omega_R_0 = 0.0
  theta%Omega_M_0 = 0.3
  theta%Omega_lambda_0 = 0.7
  theta%Omega_B_0 = 0.02
  theta%sigma_8 = 0.90
  theta%Temp_naught = 8.2
  theta%L_naught = 1D45
  theta%alpha = 1.5
  theta%beta = 1.8
end function f_lcdm

function f_dn_dtdz(theta, t, z) result(dn_dtdz)
  use cosmo_def
  implicit none
  type(params) :: theta
  integer :: i, j, M, N
  real, dimension(:) :: t, z
  real, dimension(size(t)-1,size(z)-1) :: dn_dtdz, dn_dv
  interface
     pure function f_dn_dv(theta, t1, t2, z1, z2) result(dn)
       use cosmo_def
       implicit none
       type(params), intent(in) :: theta
       real, intent(in) :: t1, t2, z1, z2
       real :: dn
     end function f_dn_dv
  end interface
  M = size(t)
  N = size(z)
  forall (i=1:M-1,j=1:N-1) dn_dtdz(i,j) = f_dn_dv(theta, t(i), t(i+1), z(j), z(j+1))
  forall (i=1:M-1,j=1:N-1) dn_dv(i,j) = f_dn_dv(theta, t(i), t(i+1), z(j), z(j+1))
end function f_dn_dtdz

function f_dn_dv(theta, t1, t2, z1, z2) result(dn)
  use cosmo_def
  use precision_model
  use cui
  use integrands
  implicit none
  type(params), intent(in) :: theta
  real, intent(in) :: t1, t2, z1, z2
  real :: dn, midt, midz, midTemp
  integer, parameter :: n = 2, simplex = 1, cube = 2
  integer :: rgtype, neval, key
  real, dimension(1:n, 0:n) :: vertices
  real :: integralvalue, abserr, epsrel
  epsrel = 0.1
  RgType = cube
  vertices(:, 0) = (/t1, z1/)
  vertices(:, 1) = (/t1, z2/)
  vertices(:, 2) = (/t2, z2/)
  midt = 0.5*(t1+t2)
  midz = 0.5*(z1+z2)
  midTemp = theta%Temp_naught*10.**midt
!   call cubatr(n,f_dn_dv_integrand,vertices,rgtype,integralvalue,abserr, &
!        maxpts=50000,epsrel=epsrel,key=key,neval=neval,job=1)
  dn = 1.
end function f_dn_dv

program main
  use cosmo_def
  use cui
  implicit none
  interface
     function f_dn_dtdz(theta, t, z) result(dn_dtdz)
       use cosmo_def
       implicit none
       type(params) :: theta
       real, dimension(:) :: t, z
       real, dimension(size(t),size(z)) :: dn_dtdz
     end function f_dn_dtdz
  end interface
  type(params) :: theta, f_lcdm
  integer :: i
  integer, parameter :: M=4, N=4
  real, dimension(M,N) :: dn_dtdz
  real, dimension(M) :: t
  real, dimension(N) :: z
  real :: V, r, f_v, f_r
!   forall (i = 1:4) 
!      t(i) = real(i)/M*(15. - 2.) + 2. 
!      z(i) = real(i)/N*(2. - 0.) + 0.
!   end forall
  theta = f_lcdm()
!   print *, f_dn_dtdz(theta, t, z)
  V = f_v(theta, 0.000, 2.)/1E9
  r = f_r(theta, 2.)
  call cubatr()
  print *, 'z = ', z
  print *, 'h = ', theta%h
  print *, 'Omega_M_0 = ', theta%Omega_M_0
  print *, 'Omega_lambda_0 = ', theta%Omega_lambda_0
  print *, 'Comoving Radial Distance (Mpc) r = ', r
  print *, 'Comoving Volume (Gpc^3) v = ', v
end program main
