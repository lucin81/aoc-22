module m_io
  use m_kinds
  implicit none
  private

  public :: nrows, read_txt_1c, read_txt_2c
contains

  integer(kind=i4) function nrows(fname)
    !! Returns the number of rows in a formatted file

    !> The file name for which we need the row count
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
    !! Reads a 4 bytes integer from a single column text file

    !> File name to read 
    character(len=*), intent(in) :: fname
    !> Resulting vector
    integer(kind=i4), allocatable :: res(:)
    integer(kind=i4) :: iu, i
    
    allocate(res(nrows(fname)))
    open (newunit=iu, file=trim(fname), action='read')
      read (iu, '(I10)') res
    close(iu)
  end function read_txt_1c

  function read_txt_2c(fname) result(res)
    !! Reads a 2 single character columns from a text file

    !> File name to read 
    character(len=*), intent(in) :: fname
    !> Resulting vector
    character(len=1), allocatable :: res(:,:), tmp(:,:)
    integer(kind=i4) :: iu, i, nr

    nr = nrows(fname)
    allocate(tmp(2, nr))
    open (newunit=iu, file=trim(fname), action='read')
      read (iu, *) tmp
    close(iu)
    res = transpose(tmp)
  end function read_txt_2c
end module m_io
