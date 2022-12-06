module m_io
  use m_kinds
  use m_strings, only: split, parse_crates
  use m_sets, only: set_t
  implicit none
  private

  public :: nrows, read_txt_1c, read_txt_2c, max_string_len, read_txt_char_array, &
            read_assignemnt_pairs, find_first_blank_line, read_nth_line, init_stacks, &
            read_moves

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

  function find_first_blank_line(fname, start_line_opt) result(res)
    !! Returns the line number of the first blank line starting from start_line_opt
    !! if start_line_opt is not present start from line 1
    !! if there are no blank lines in the file return 0
    character(len=*), intent(in) :: fname
    integer(kind=i4), intent(in), optional :: start_line_opt
    integer(kind=i4) :: res
    integer(kind=i4) :: start_line, i, nr, iu
    character(len=1024) :: buffer


    start_line = 1
    if (present(start_line_opt)) start_line = start_line_opt

    nr = nrows(fname)

    if (start_line >= nr) error stop "Start line >= number of rows in file"
    
    open (newunit=iu, file=trim(fname), action='read')
      if (start_line > 1 ) then 
        do i = 1, start_line - 1
          read(iu, *) ! Skip lines before start line 
        enddo
      endif 

      do i = start_line, nr
        read(iu, '(a)') buffer
        if (buffer == '') then 
          res = i
          return
        endif
      enddo
    close(iu)

    res = 0
  end function find_first_blank_line

  function read_nth_line(fname, n) result(res)
    !! Returns a character array with the nth line in fname
    character(len=*), intent(in) :: fname
    integer(kind=i4) :: n, i, nr, iu
    character(len=1024) :: res

    nr = nrows(fname)

    if (n >= nr) error stop "n >= number of rows in file"
    
    open (newunit=iu, file=trim(fname), action='read')
      do i = 1, n - 1
        read(iu, *) ! Skip until n-1
      enddo
      read(iu, '(a)') res
    close(iu)
  end function read_nth_line

  function read_assignemnt_pairs(fname, sep) result(res)
    !! Returns the assignments for the camp cleanup
    character(len=*), intent(in) :: fname
    type(set_t), allocatable :: res(:, :)
    character(len=:), allocatable :: assignment1, assignment2
    character(len=1), optional, intent(in) :: sep
    character(len=1) :: separator
    character(len=1024) :: buffer
    integer(kind=i4) :: iu, i, nr

    separator = ','
    if (present(sep)) separator = sep

    nr = nrows(fname)
    allocate (res(2, nr))
    open (newunit=iu, file=trim(fname), action='read')
    do i = 1, nr
      read (iu, '(a)') buffer
      call split(trim(buffer), separator, assignment1, assignment2)
      res(:, i) = [set_t(assignment1), set_t(assignment2)]
    end do
    close (iu)
  end function read_assignemnt_pairs

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

  subroutine init_stacks(stacks, start_row, end_row, fname, crates)
    !! Read data structure for aoc day5
    integer(kind=i4), intent(in) :: stacks(:)
    integer(kind=i4), intent(in) :: start_row, end_row
    character(len=*), intent(in) :: fname
    character(len=1), intent(out) :: crates(:,:)
    
    integer(kind=i4) :: iu, i, level_index
    character(len=1024) :: buffer

    open (newunit=iu, file=trim(fname), action='read')
      if (start_row > 1) then 
        do i = 1, start_row - 1
          read(iu, *) ! Skip lines
        enddo
      endif

      do i = start_row, end_row
        read(iu, '(a)') buffer
        level_index = size(crates,2) - end_row + i
        call parse_crates(buffer, '[', crates(:, level_index))
      enddo
    close(iu)
  end subroutine init_stacks

  subroutine read_moves(fname, skip, moves)
    character(len=*), intent(in) :: fname
    integer(kind=i4), intent(in) :: skip
    integer(kind=i4), allocatable :: moves(:, :)
    character(len=1024) :: buffer
    integer(kind=i4) :: stat, i, index, iu, k, nmoves, idx2

    open (newunit=iu, file=trim(fname), action='read')
      do i = 1, skip
        read(iu, *) ! Skip lines
      enddo

      nmoves = nrows(fname) - skip
      allocate(moves(nmoves, 3))
      do i = 1, nmoves
        read(iu, '(a)') buffer
        index = scan(trim(buffer), ' ')
        k = 1
        do while (index /= 0)
          buffer(index:index) = '.'
          idx2 = index + 1
          do while (buffer(idx2:idx2) == ' ')
            buffer(idx2:idx2) = '.'
            idx2 = idx2 + 1
          enddo
          ! idx2 = scan(trim(buffer), ' ')

          read(buffer((index+1):), *, iostat=stat) moves(i, k)
          if (stat == 0) k = k + 1
          index = scan(trim(buffer), ' ')
        enddo
      enddo
  end subroutine read_moves
end module m_io
