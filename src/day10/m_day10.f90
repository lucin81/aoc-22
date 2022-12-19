module m_day10
  use m_kinds
  implicit none
  private

  public :: print_crt
contains

  subroutine print_crt(crt_in, nr, nc)
    character(len=1), intent(in) :: crt_in(:)
    integer(i4), intent(in) :: nr, nc

    character(len=1) :: crt(nr,nc)
    character(len=1024) :: fmt
    integer(i4) :: i, j


    crt = reshape(crt_in, [nr, nc], order=[2, 1])
    write (fmt, '(a,i3,a)') '(', size(crt, 2), '(a1))'
    do i = 1, size(crt, 1)
      write (*, fmt) (crt(i, j), j=1, size(crt, 2))
    end do
  end subroutine print_crt
end module m_day10
