program test_io
  use m_kinds
  use m_io, only: nrows, read_txt_1c, read_txt_2c, max_string_len, read_txt_char_array, &
    read_assignemnt_pairs
  use m_sets, only: set_t
  implicit none

  logical :: res, overall_res
  character(len=:), allocatable :: test_name
  integer(kind=i4) :: nr

  overall_res = .true.

  test_name = 'Can get number of rows in file with nrows'
  res = nrows('data/test/d1p1.txt') == 14
  print*, test_name, res
  overall_res = overall_res .and. res

  block 
    integer(kind=i4), allocatable :: tmp(:), baseline(:)
    
    baseline = [1000, 2000, 3000, 0, 4000, 0, 5000, 6000, 0, &
      7000, 8000, 9000, 0, 10000]
    tmp = read_txt_1c('data/test/d1p1.txt')
    res = all(tmp == baseline) 
    if (.not. res) then 
      print*, "Baseline=",baseline 
      print*, "Values=",tmp
    endif
  end block 
  test_name = 'Can read single column integer with read_txt_1c'
  print*, test_name, res
  overall_res = overall_res .and. res

  block 
    integer(kind=i4) :: expected, actual
    
    expected = 32

    actual = max_string_len('data/test/d3p1.txt')
    res = expected == actual
    if (.not. res) then 
      print*, "Expected=",expected 
      print*, "Actual=",actual
    endif
  end block 
  test_name = 'Can compute the length of the longest string in a text file'
  print*, test_name, res
  overall_res = overall_res .and. res


  block 
    character(len=:), allocatable :: expected, actual
    character(len=:), allocatable :: tmp(:)
    
    allocate(character(len=32) :: tmp(6))
    tmp = read_txt_char_array('data/test/d3p1.txt', 32, 6)
    actual = tmp(3)
    expected = 'PmmdzqPrVvPwwTWBwg'

    res = expected == actual
    if (.not. res) then 
      print*, "Expected=",expected 
      print*, "Actual=",actual
    endif
  end block 
  test_name = 'Can read an array of character strings from file'
  print*, test_name, res
  overall_res = overall_res .and. res

  block 
    character(len=1), allocatable :: actual(:, :)
    character(len=1), allocatable :: expected1(:), expected2(:)

    expected1 = ['A', 'B', 'C']
    expected2 = ['Y', 'X', 'Z']

    actual = read_txt_2c('data/test/d2p1.txt')
    res = all(actual(:,1) == expected1) .and. all(actual(:,2) == expected2)
    if (.not.res) then 
      print*, "Expected1=",expected1
      print*, "Actual1=",actual(:,1)
      print*, "Expected2=",expected2
      print*, "Actual2=",actual(:,2)
    endif 
  end block 
  test_name = 'Can read 2 single character columns with read_txt_2c'
  print*, test_name, res
  overall_res = overall_res .and. res

  block 
    type(set_t), allocatable :: actual(:, :)
    type(set_t), allocatable :: expected(:, :)
    integer(kind=i4) :: i

    allocate(expected(6, 2))
    expected(1, :) = [set_t("2-4"), set_t("6-8")]
    expected(2, :) = [set_t("2-3"), set_t("4-5")]
    expected(3, :) = [set_t("5-7"), set_t("7-9")]
    expected(4, :) = [set_t("2-8"), set_t("3-7")]
    expected(5, :) = [set_t("6-6"), set_t("4-6")]
    expected(6, :) = [set_t("2-6"), set_t("4-8")]
    actual = transpose(read_assignemnt_pairs('data/test/d4p1.txt'))

    res = .true. 
    do i = 1, size(expected,1)
      res = res .and. &
            all(actual(i, 1)%get_bounds() == expected(i, 1)%get_bounds()) .and. &
            all(actual(i, 2)%get_bounds() == expected(i, 2)%get_bounds())
    enddo
    if (.not.res) then 
      print*, "Printing only the first set..."
      print*, "Expected:"
      call expected(1,1)%print()
      print*, "Actual:"
      call actual(1,1)%print()
    endif 
  end block 
  test_name = "transpose(read_assignemnt_pairs('data/test/d4p1.txt')) is correct"
  print*, test_name, res
  overall_res = overall_res .and. res

  if (overall_res) then 
    print*, "test_io: All tests succeded"
  else 
    error stop "test_io: Tests failed"
  endif 
end program test_io
