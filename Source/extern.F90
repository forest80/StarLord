module extern_probin_module

  use bl_types
  use bl_space

  implicit none

  private

  logical, allocatable, public :: eos_assume_neutral
  real (kind=dp_t), allocatable, public :: eos_gamma
  real (kind=dp_t), allocatable, public :: small_x
#ifdef CUDA
  attributes(managed) :: small_x
  attributes(managed) :: eos_gamma
  attributes(managed) :: eos_assume_neutral
#endif

  !$acc declare create(eos_assume_neutral, eos_gamma, small_x)

end module extern_probin_module

subroutine runtime_init(name,namlen)

  use extern_probin_module

  implicit none

#ifdef CUDA
  integer :: cuda_result
#endif

  integer :: namlen
  integer :: name(namlen)

  integer :: un, i, status

  integer, parameter :: maxlen = 256
  character (len=maxlen) :: probin

  namelist /extern/ eos_assume_neutral
  namelist /extern/ eos_gamma
  namelist /extern/ small_x

  allocate(eos_assume_neutral)
  allocate(eos_gamma)
  allocate(small_x)

  eos_assume_neutral = .true.
  eos_gamma = 5.d0/3.d0
  small_x = 1.d-30


  ! create the filename
  if (namlen > maxlen) then
     print *, 'probin file name too long'
     stop
  endif

  do i = 1, namlen
     probin(i:i) = char(name(i))
  end do


  ! read in the namelist
  un = 9
  open (unit=un, file=probin(1:namlen), form='formatted', status='old')
  read (unit=un, nml=extern, iostat=status)

  if (status < 0) then
     ! the namelist does not exist, so we just go with the defaults
     continue

  else if (status > 0) then
     ! some problem in the namelist
     print *, 'ERROR: problem in the extern namelist'
     stop
  endif

  close (unit=un)

  !$acc update &
  !$acc device(eos_assume_neutral, eos_gamma, small_x)

end subroutine runtime_init


subroutine ca_extern_finalize() bind(c, name='ca_extern_finalize')

  use extern_probin_module

  implicit none

  deallocate(eos_assume_neutral)
  deallocate(eos_gamma)
  deallocate(small_x)

end subroutine ca_extern_finalize
