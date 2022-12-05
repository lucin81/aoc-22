program test_sets
  use m_kinds
  use m_sets, only: set_t
  implicit none

  logical :: res, overall_res
  character(len=:), allocatable :: test_name

  overall_res = .true.

  block 
    integer(kind=i4) :: expected(2), actual(2)
    type(set_t) :: s1, s2, s3

    expected = [2, 10]
    s1 = set_t(2, ub = 10, name = "2-10")
    s2 = set_t([2, 10, 7], name = "2-10")
    s3 = set_t(name = "2-10")
    
    res = all(expected == s1%get_bounds()) .and. &
          all(expected == s2%get_bounds()) .and. &
          all(expected == s3%get_bounds())
    if (.not. res) then 
      print*, "Expected=", expected
      print*, "Actual (s1)=", s1%get_bounds()
      print*, "Actual (s2)=", s2%get_bounds()
      print*, "Actual (s3)=", s3%get_bounds()
    endif 
  end block

  test_name = 'Set initialized correctly'
  print*, test_name, res
  overall_res = overall_res .and. res

  block 
    type(set_t) :: s1, s2
    s1 = set_t("2-8")
    s2 = set_t("3-6")

    res = s2%is_subset(s1) 
  end block 
  test_name = '"3-6" is a subset of "2-8"'
  print*, test_name, res
  overall_res = overall_res .and. res

  block 
    type(set_t) :: s1(2), s2(2)
    s1 = [set_t("2-8"), set_t("25-34")]
    s2 = [set_t("3-6"), set_t("01-15")]

    res = all(s2%is_subset(s1) .eqv. [.true., .false.])
  end block 
  test_name = '"3-6" is a subset of "2-8" and "25-34" is not a subset of "1-15"'
  print*, test_name, res
  overall_res = overall_res .and. res

  if (overall_res) then 
    print*, "test_sets: All tests succeded"
  else 
    error stop "test_sets: Tests failed"
  endif 
end program