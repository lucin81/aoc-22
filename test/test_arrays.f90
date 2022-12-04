program test_arrays
  use m_kinds
  use m_arrays, only: find_matches, unique
  implicit none

  logical :: res, overall_res
  character(len=:), allocatable :: test_name

  overall_res = .true.

  block 
    integer(kind=i4), allocatable :: actual(:), expected(:)

    actual = unique([73, 12, 34, 12])
    expected = [12, 34, 73]
    res = all(actual == expected)
    if (.not. res) then 
      print*, "Expected=", expected
      print*, "Actual=", actual
    endif 
  end block

  test_name = 'unique([73, 12, 34, 12]) == [12, 34, 73]'
  print*, test_name, res
  overall_res = overall_res .and. res

  block 
    integer(kind=i4), allocatable :: actual(:), expected(:)

    expected = [12, 34]
    actual = find_matches([34, 73, 12, 12], [12, 11, 12, 34, 44, 22])
    res = all(actual == expected)
    if (.not. res) then 
      print*, "Expected=", expected
      print*, "Actual=", actual
    endif 
  end block

  test_name = 'find_matches([34, 73, 12, 12], [12, 11, 12, 34, 44, 22]) == [12, 34]'
  print*, test_name, res
  overall_res = overall_res .and. res

  block 
    integer(kind=i4), allocatable :: actual(:)

    actual = find_matches([12, 34, 73, 29], [16, 11])
    res = size(actual) == 0
    if (.not. res) then 
      print*, "Expected= unallocated array"
      print*, "Actual=", actual
      print*, allocated(actual)
      print*, size(actual)
    endif 
  end block

  test_name = 'find_matches([12, 34, 73, 29], [16, 11]) == zero size array'
  print*, test_name, res
  overall_res = overall_res .and. res

  if (overall_res) then 
    print*, "test_arrays: All tests succeded"
  else 
    error stop "test_arrays: Tests failed"
  endif 
end program