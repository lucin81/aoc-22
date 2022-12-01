program test_d1p1
  use m_kinds
  use m_io, only: nrows, read_txt_1c
  use stdlib_sorting, only: sort
  implicit none

  logical :: res 
  character(len=:), allocatable :: test_name
  integer(kind=i4) :: nr

  test_name = 'Read correctly number of rows in file'
  res = nrows('data/test/d1p1.txt') == 14
  print*, test_name, res
  if (.not. res) error stop

  block 
    integer(kind=i4), allocatable :: tmp(:), baseline(:)
    
    baseline = [1000, 2000, 3000, 0, 4000, 0, 5000, 6000, 0, &
      7000, 8000, 9000, 0, 10000]
    tmp = read_txt_1c('data/test/d1p1.txt')
    res = all(tmp == baseline) 
    if (.not. res) then 
      print*, "Baseline=",baseline 
      print*, "Values=",tmp
      error stop  
    endif
  end block 
  test_name = 'Read correctly values in file'
  print*, test_name, res

end program test_d1p1
