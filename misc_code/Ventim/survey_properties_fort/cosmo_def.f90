module cosmo_def
  implicit none
  type params
     real :: h
     real :: w
     real :: Omega_M_0
     real :: Omega_lambda_0
     real :: Omega_R_0
     real :: Omega_B_0
     real :: sigma_8
     real :: Temp_naught
     real :: L_naught
     real :: alpha
     real :: beta
  end type params
  
  real, parameter :: gm_per_Msun = 2D30*1E3
  real, parameter :: cm_per_Mpc = 3.08D18*1D6
  real, parameter :: G = 6.67259D-8*gm_per_Msun/cm_per_Mpc**3
  real, parameter :: c = 2.99792E10
  real, parameter :: H_0 = 100.*1000.*100./cm_per_Mpc
  real, parameter :: delta_c = 1.686
  real, parameter :: pi = 4.*atan(1.0)
end module cosmo_def
