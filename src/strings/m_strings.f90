module m_strings
  use m_kinds
  use stdlib_string_type, only: string_type, scan
  implicit none
  private

  public :: split_string, string_to_int, split, to_int, string_to_int_array, &
            parse_crates
contains

  subroutine split(str, separator, string1, string2)
    !! Split a string in two parts at the first occurrence of a separator character
    !! If the separator is not present in str the last string is empty
    !! and the first string is equal to str
    character(len=*), intent(in) :: str
    character(len=1), intent(in) :: separator
    character(len=:), allocatable, intent(out) :: string1, string2
    integer(kind=i4) :: index

    index = scan(trim(str), separator)
    if (index == 0) then
      ! separator not found
      string1 = str
      string2 = ''
    else if (index == 1) then
      ! separator is the first character of the string
      string1 = ''
      string2 = str(index + 1:)
    else if (index == len_trim(str)) then
      ! separator is the last character of the string
      string1 = str(1:index - 1)
      string2 = ''
    else
      ! regular case
      string1 = str(1:index - 1)
      string2 = str(index + 1:)
    end if
  end subroutine split

  subroutine parse_crates(str, sep, res)
    !! Parse string into stack of crates
    character(len=*), intent(in) :: str
    character(len=1), intent(in) :: sep
    character(len=1), intent(inout) :: res(:)

    character(len=:), allocatable :: tmp
    integer(kind=i4) :: index, stack_id

    tmp = trim(str)
    index = scan(trim(tmp), sep)
    do while (index /= 0)
      stack_id = index/4 + 1
      res(stack_id) = str((index + 1):(index + 1))
      tmp(index:index) = '|'

      index = scan(trim(tmp), sep)
    end do
  end subroutine parse_crates

  elemental function to_int(str) result(int)
    !! Convert a string to an integer
    character(len=*), intent(in) :: str
    integer(kind=i4) :: int
    integer(kind=i4) :: stat

    read (str, *, iostat=stat) int

    if (stat /= 0) error stop 'Error in string conversion to int'
  end function to_int

  function string_to_int_array(str, sep) result(res)
    !! Return an array of integer by parsing str
    !> The string to parse
    character(len=*), intent(in) :: str
    !> The number of characters per value in the string (including white space)
    character(len=1), intent(in) :: sep
    integer(kind=i4), allocatable :: res(:)

    character(len=:), allocatable :: tmp
    integer(kind=i4) :: n, idx, k, val, idx2, stat

    allocate(res(0))
    tmp = trim(adjustl(str))
    ! Try to read from the beginning 
    read (tmp, *, iostat=stat) val
    if (stat == 0) res = [res, val] 

    idx = scan(tmp, ' ')
    do while (idx /= 0)
      tmp = trim(adjustl(tmp(idx+1:)))
      read (tmp, *, iostat=stat) val

      if (stat == 0) res = [res, val] 
      idx = scan(tmp, ' ')
    end do
  end function string_to_int_array

  pure subroutine split_string(string, s1, s2)
    !! Takes a string and returns the two substrings obtained by
    !! dividing the string in half. If the number of characters in the
    !! string is odd gives an error.

    character(len=*), intent(in) :: string
    character(len=:), allocatable, intent(out) :: s1, s2

    integer(kind=i4) :: str_len

    str_len = len_trim(string)

    if (modulo(str_len, 2) /= 0) error stop "split_string: odd number of characters"

    s1 = string(1:(str_len/2))
    s2 = string((str_len/2 + 1):str_len)
  end subroutine split_string

  pure function string_to_int(s) result(res)
    !! Convert a string of characters into a sequence of scores.
    !! Characters between 'a' and 'z' map to numbers  1:26
    !! Characters between 'A' and 'Z' map to numbers 27:52
    character(len=*), intent(in) :: s
    integer(kind=i4), allocatable :: res(:)
    integer(kind=i4) :: i, v

    allocate (res(len_trim(s)))
    do i = 1, len_trim(s)
      v = ichar(s(i:i))
      if (v > 90) then
        ! lowercase letters get a score between 1 and 26
        res(i) = v - ichar('a') + 1
      else
        ! uppercase letter get a score between 27 and 52
        res(i) = v - ichar('A') + 27
      end if
    end do
  end function string_to_int
end module m_strings
