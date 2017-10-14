module eos_module

  implicit none

  public eos_init, eos

contains

  ! EOS initialization routine: read in general EOS parameters, then 
  ! call any specific initialization used by the EOS.

  subroutine eos_init()

    use amrex_fort_module, only: rt => amrex_real
    use eos_type_module, only: mintemp, maxtemp, mindens, maxdens, minx, maxx, &
                               minye, maxye, mine, maxe, minp, maxp, minh, maxh, mins, maxs
    use actual_eos_module, only: actual_eos_init

    implicit none

    ! Allocate and set default values

    allocate(mintemp)
    allocate(maxtemp)
    allocate(mindens)
    allocate(maxdens)
    allocate(minx)
    allocate(maxx)
    allocate(minye)
    allocate(maxye)
    allocate(mine)
    allocate(maxe)
    allocate(minp)
    allocate(maxp)
    allocate(mins)
    allocate(maxs)
    allocate(minh)
    allocate(maxh)

    mintemp = 1.d-200
    maxtemp = 1.d200
    mindens = 1.d-200
    maxdens = 1.d200
    minx    = 1.d-200
    maxx    = 1.d0 + 1.d-12
    minye   = 1.d-200
    maxye   = 1.d0 + 1.d-12
    mine    = 1.d-200
    maxe    = 1.d200
    minp    = 1.d-200
    maxp    = 1.d200
    mins    = 1.d-200
    maxs    = 1.d200
    minh    = 1.d-200
    maxh    = 1.d200

    call actual_eos_init

  end subroutine eos_init



  subroutine eos(input, state)

    !$acc routine seq

    use eos_type_module, only: eos_t, composition, composition_derivatives
    use actual_eos_module, only: actual_eos

    implicit none

    ! Input arguments

    integer,      intent(in   ) :: input
    type (eos_t), intent(inout) :: state

    ! Get abar, zbar, etc.

    call composition(state)

    ! Call the EOS.

    call actual_eos(input, state)

  end subroutine eos

end module eos_module
