module m_io
  use m_kinds
  implicit none
  private

  public :: nrows, read_txt_1c
contains

  integer(kind=i4) function nrows(fname)
    character(len=*), intent(in) :: fname
    integer(kind=i4) :: iu

    nrows = 0
    open (newunit=iu, file=trim(fname), action='read')
    do
      read (iu, *, END=200)
      nrows = nrows + 1
    end do
    200 close (iu)
  end function nrows

  function read_txt_1c(fname) result(res)
    character(len=*), intent(in) :: fname
    integer(kind=i4), allocatable :: res(:)
    integer(kind=i4) :: iu, i
    
    allocate(res(nrows(fname)))
    open (newunit=iu, file=trim(fname), action='read')
      read (iu, '(I10)') res
    close(iu)
  end function read_txt_1c
end module m_io
