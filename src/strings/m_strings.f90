module m_strings
  use m_kinds
  use stdlib_string_type, only: string_type, scan
  implicit none
  private
  
  public :: split_string, string_to_int
contains
  
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

    allocate(res(len_trim(s)))
    do i = 1, len_trim(s)
      v = ichar(s(i:i))
      if (v > 90) then 
        ! lowercase letters get a score between 1 and 26
        res(i) = v - ichar('a') + 1
      else 
        ! uppercase letter get a score between 27 and 52
        res(i) = v - ichar('A') + 27
      endif
    enddo
  end function string_to_int

  pure function find_common_characters(c1, c2) result(res)
    !! Returns the common characters in the two strings

    character(len=*), intent(in) :: c1, c2
    character(len=:), allocatable :: res

    

  end function find_common_characters
end module m_strings