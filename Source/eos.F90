module eos_module

  implicit none

  public eos_init, eos

  logical, save :: initialized = .false.

  interface eos
     module procedure eos_doit
#ifdef CUDA
     module procedure eos_host
#endif
  end interface eos

contains

  ! EOS initialization routine: read in general EOS parameters, then 
  ! call any specific initialization used by the EOS.

  subroutine eos_init(small_temp, small_dens)

    use amrex_fort_module, only: rt => amrex_real
    use parallel, only: parallel_IOProcessor
    use bl_error_module, only: bl_warn
    use eos_type_module, only: mintemp, maxtemp, mindens, maxdens, minx, maxx, &
                               minye, maxye, mine, maxe, minp, maxp, minh, maxh, mins, maxs
    use actual_eos_module, only: actual_eos_init
#if (defined(CUDA) && !defined(NO_CUDA_8))
    use cudafor, only: cudaMemAdvise, cudaMemAdviseSetPreferredLocation
    use cuda_module, only: cuda_device_id
#endif

    implicit none

#ifdef CUDA
    integer :: cuda_result
#endif

    real(rt), optional :: small_temp
    real(rt), optional :: small_dens

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

    ! Set up any specific parameters or initialization steps required by the EOS we are using.

    call actual_eos_init

    ! If they exist, save the minimum permitted user temperature and density.
    ! These are only relevant to this module if they are larger than the minimum
    ! possible EOS quantities. We will reset them to be equal to the EOS minimum
    ! if they are smaller than that.

    ! Note that in this routine we use the Fortran-based parallel_IOProcessor()
    ! command rather than the C++-based version used elsewhere in Castro; this
    ! ensures compatibility with Fortran-based test programs.

    if (present(small_temp)) then
       if (small_temp < mintemp) then
          if (parallel_IOProcessor()) then
             call bl_warn('EOS: small_temp cannot be less than the mintemp allowed by the EOS. Resetting small_temp to mintemp.')
          endif
          small_temp = mintemp
       else
          mintemp = small_temp
       endif
    endif

    if (present(small_dens)) then
       if (small_dens < mindens) then
          if (parallel_IOProcessor()) then
             call bl_warn('EOS: small_dens cannot be less than the mindens allowed by the EOS. Resetting small_dens to mindens.')
          endif
          small_dens = mindens
       else
          mindens = small_dens
       endif
    endif

    initialized = .true.

    !$acc update &
    !$acc device(mintemp, maxtemp, mindens, maxdens, minx, maxx, minye, maxye) &
    !$acc device(mine, maxe, minp, maxp, mins, maxs, minh, maxh)

#if (defined(CUDA) && !defined(NO_CUDA_8))
    cuda_result = cudaMemAdvise(mintemp, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(maxtemp, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(mindens, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(maxdens, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(minx, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(maxx, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(minye, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(maxye, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(mine, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(maxe, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(minp, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(maxp, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(mins, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(maxs, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(minh, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
    cuda_result = cudaMemAdvise(maxh, 1, cudaMemAdviseSetPreferredLocation, cuda_device_id)
#endif

  end subroutine eos_init



  subroutine eos_doit(input, state)

    !$acc routine seq

    use eos_type_module, only: eos_t, composition, composition_derivatives
    use actual_eos_module, only: actual_eos
#if !(defined(ACC) || defined(CUDA))
    use bl_error_module, only: bl_error
#endif

    implicit none

    ! Input arguments

    integer,      intent(in   ) :: input
    type (eos_t), intent(inout) :: state

    ! Get abar, zbar, etc.

    call composition(state)

    ! Call the EOS.

    call actual_eos(input, state)

  end subroutine eos_doit

  subroutine eos_finalize() bind(c, name='eos_finalize')

    use eos_type_module, only: mintemp, maxtemp, mindens, maxdens, &
                               minx, maxx, minye, maxye, &
                               mine, maxe, minp, maxp, &
                               mins, maxs, minh, maxh
    use actual_eos_module, only: actual_eos_finalize

    implicit none

    deallocate(mintemp)
    deallocate(maxtemp)
    deallocate(mindens)
    deallocate(maxdens)
    deallocate(minx)
    deallocate(maxx)
    deallocate(minye)
    deallocate(maxye)
    deallocate(mine)
    deallocate(maxe)
    deallocate(minp)
    deallocate(maxp)
    deallocate(mins)
    deallocate(maxs)
    deallocate(minh)
    deallocate(maxh)

    call actual_eos_finalize()

  end subroutine eos_finalize

end module eos_module
