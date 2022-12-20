module m_kinds
  !! Import kind constants from iso_fortran_env module
  use iso_fortran_env, only: i4 => int32, d4 => real32, i8 => int64
  implicit none
  
  integer(i4), parameter :: i16 = selected_int_kind(32)
contains
  
end module m_kinds