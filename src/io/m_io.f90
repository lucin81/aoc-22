module m_io
  use m_kinds
  implicit none
  private

  public :: nrows, read_txt_1c, read_txt_2c, max_string_len, read_txt_char_array

  interface read_txt_1c
    module procedure :: read_txt_1c_int32
  end interface
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

  function max_string_len(fname) result(res)
    !! Returns the length of the longest string in a file
    character(len=*), intent(in) :: fname
    ! character(len=1), intent(in) :: separator
    integer(kind=i4) :: res
    character(len=1024) :: string
    integer(kind=i4) :: iu

    res = 0_i4
    open (newunit=iu, file=trim(fname), action='read')
    do
      read (iu, '(a)', end=200) string
      res = max(res, len_trim(string))
    end do
200 close (iu)
  end function max_string_len

  ! integer function ncols(fname, separator)
  !   character(len=*), intent(in) :: fname
  !   character(len=1), intent(in) :: separator
  !   character(len=1000) :: string
  !   integer(kind=i4) :: iu

  !   open (newunit=iu, file=trim(fname), action='read')
  !   read (iu, '(a)') string
  !   close (iu)
  !   ncols = 1
  !   do while (string(ncols:ncols) /= separator)
  !     ncols = ncols + 1
  !   end do
  !   ncols = ncols - 1
  ! end function ncols

  function read_txt_char_array(fname, max_len, nr) result(res)
    !! Returns the length of the longest string in a file
    character(len=*), intent(in) :: fname
    integer(kind=i4), intent(in) :: max_len, nr
    ! character(len=1), intent(in) :: separator
    character(len=max_len) :: res(nr)
    integer(kind=i4) :: iu, i

    open (newunit=iu, file=trim(fname), action='read')
    do i = 1, nr
      read (iu, '(a)') res(i)
    end do
    close (iu)
  end function read_txt_char_array

  function read_txt_1c_int32(fname) result(res)
    !! Reads a 4 bytes integer from a single column text file

    !> File name to read
    character(len=*), intent(in) :: fname
    !> Resulting vector
    integer(kind=i4), allocatable :: res(:)
    integer(kind=i4) :: iu, i

    allocate (res(nrows(fname)))
    open (newunit=iu, file=trim(fname), action='read')
    read (iu, '(I10)') res
    close (iu)
  end function read_txt_1c_int32

  function read_txt_2c(fname) result(res)
    !! Reads a 2 single character columns from a text file

    !> File name to read
    character(len=*), intent(in) :: fname
    !> Resulting vector
    character(len=1), allocatable :: res(:, :), tmp(:, :)
    integer(kind=i4) :: iu, i, nr

    nr = nrows(fname)
    allocate (tmp(2, nr))
    open (newunit=iu, file=trim(fname), action='read')
    read (iu, *) tmp
    close (iu)
    res = transpose(tmp)
  end function read_txt_2c
end module m_io
