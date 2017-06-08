module simple_log_module

#ifndef CUDA
  use bl_error_module
#endif

  implicit none
  
  integer, allocatable ::errors
#ifdef CUDA
  attributes(managed) errors
#endif
  contains

#ifdef CUDA
  attributes(device) &
#endif
  subroutine log_error(err)
  integer, intent(in) :: err
    errors = err 

  end subroutine log_error


end module simple_log_module
