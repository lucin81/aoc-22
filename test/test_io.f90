program test_io
  use m_kinds
  use m_io, only: nrows, read_txt_1c, read_txt_2c
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

  if (overall_res) then 
    print*, "test_io: All tests succeded"
  else 
    error stop "test_io: Tests failed"
  endif 
end program test_io
