module probdata_module

  use amrex_fort_module, only: rt => amrex_real

  implicit none

  real(rt), public :: p_ambient, dens_ambient, exp_energy, e_ambient
  real(rt), public :: r_init
  integer,  public :: nsub

#ifdef CUDA
  attributes(managed) :: p_ambient, dens_ambient, exp_energy, &
                         r_init, nsub, e_ambient
#endif
  
end module probdata_module
