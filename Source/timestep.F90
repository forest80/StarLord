module timestep_module

  implicit none

contains

  ! Courant-condition limited timestep

  subroutine ca_estdt() bind(c,name='ca_estdt')

    use eos_module, only: eos
    use eos_type_module, only: eos_t, eos_input_re
    use amrex_fort_module, only: get_loop_bounds

    implicit none

    integer :: i, j
    integer :: lo(3), hi(3)
    integer :: blo(3), bhi(3)

    type (eos_t) :: eos_state

    lo(:) = 0
    hi(:) = 1

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

  end subroutine ca_estdt

end module timestep_module
