module timestep_module

  use amrex_fort_module, only: rt => amrex_real

  implicit none

contains

  ! Courant-condition limited timestep

#ifdef CUDA
  attributes(global) &
#endif
  subroutine estdt(lo,hi,u,u_lo,u_hi,dx,dt)

    use eos_module, only: eos
    use eos_type_module, only: eos_t, eos_input_re
    use amrex_fort_module, only: rt => amrex_real, get_loop_bounds
    use meth_params_module, only: NVAR

    implicit none

    integer,  intent(in   ) :: lo(3), hi(3)
    integer,  intent(in   ) :: u_lo(3), u_hi(3)
    real(rt), intent(in   ) :: u(u_lo(1):u_hi(1),u_lo(2):u_hi(2),u_lo(3):u_hi(3),NVAR)
    real(rt), intent(in   ) :: dx(3)
    real(rt), intent(inout) :: dt

    integer  :: i, j
    integer  :: blo(3), bhi(3)

    type (eos_t) :: eos_state

    call get_loop_bounds(blo, bhi, lo, hi)

    ! Call EOS for the purpose of computing sound speed

    blo(:) = 0
    bhi(:) = 1

    do j = blo(2), bhi(2)
       do i = blo(1), bhi(1)

          eos_state % rho = 1.0d5
          eos_state % T   = 1.0d8
          eos_state % e   = 1.0d23
          eos_state % xn  = 1.0d0

          call eos(eos_input_re, eos_state)

       enddo
    enddo

  end subroutine estdt

end module timestep_module
