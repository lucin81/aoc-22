program test_strings
  use m_kinds
  use m_strings, only: split_string, string_to_int
  implicit none

  logical :: res, overall_res
  character(len=:), allocatable :: test_name

  overall_res = .true.

  block 
    character(len=:), allocatable :: s1, s2

    call split_string('vJrwpWtwJgWrhcsFMMfFFhFp', s1, s2)
    res = s1 == 'vJrwpWtwJgWr' .and. s2 == 'hcsFMMfFFhFp'
    if (.not. res) then 
      print*, "Expected=", 'vJrwpWtwJgWr', 'hcsFMMfFFhFp'
      print*, "Actual=", s1, s2
    endif 
  end block

  test_name = 'Can split a string in two equal parts'
  print*, test_name, res
  overall_res = overall_res .and. res

  res = all(string_to_int('pLPvts') == [16, 38, 42, 22, 20, 19])
  test_name = 'Can convert "pLPvts" in array of scores [16, 38, 42, 22, 20, 19]'
  print*, test_name, res
  overall_res = overall_res .and. res

  if (overall_res) then 
    print*, "test_strings: All tests succeded"
  else 
    error stop "test_strings: Tests failed"
  endif 
end program