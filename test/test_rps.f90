program test_rps
  use m_kinds
  use m_rps, only: shape_to_int, calculate_p1_score, calculate_p1_score_from_outcome, &
    outcome_to_int
  implicit none

  logical :: res, overall_res
  character(len=:), allocatable :: test_name

  overall_res = .true.

  test_name = 'shape_to_int returns 1 with input A or X'
  res = shape_to_int('A') == 1 .and. shape_to_int('X') == 1
  print*, test_name, res
  overall_res = overall_res .and. res

  test_name = 'shape_to_int returns [2, 3] with input [B, C] or [Y, Z]'
  res = all(shape_to_int(['B', 'C']) == [2, 3]) .and. &
        all(shape_to_int(['Y', 'Z']) == [2, 3])
  print*, test_name, res
  overall_res = overall_res .and. res

  test_name = 'calculate_p1_score returns 4 with input rock/rock'
  res = calculate_p1_score(1, 1) == 4
  print*, test_name, res
  overall_res = overall_res .and. res

  test_name = 'calculate_p1_score returns [9, 2] with input &
    &[scissors, paper]/[paper, scissors]'
  res = all(calculate_p1_score([3, 2], [2, 3]) == [9, 2])
  print*, test_name, res
  overall_res = overall_res .and. res

  test_name = 'calculate_p1_score_from_outcome returns &
    &[4, 1, 7, 8, 5, 2, 3, 9, 6] with input &
    &[draw, lose, win, win, draw, lose, lose, win, draw]/&
    &[rock, paper, scissors, rock, paper, scissors, rock, paper, scissors]'
  res = all(calculate_p1_score_from_outcome([3, 0, 6, 6, 3, 0, 0, 6, 3], &
    [1, 2, 3, 1, 2, 3, 1, 2, 3]) == [4, 1, 7, 8, 5, 2, 3, 9, 6])
  print*, test_name, res
  if (.not. res) then 
    print*, calculate_p1_score_from_outcome([3, 0, 6, 6, 3, 0, 0, 6, 3], &
    [1, 2, 3, 1, 2, 3, 1, 2, 3])
  endif 
  overall_res = overall_res .and. res
  
  test_name = 'outcome_to_int returns 0 with input X'
  res = outcome_to_int('X') == 0
  print*, test_name, res
  overall_res = overall_res .and. res

  test_name = 'outcome_to_int returns [3, 6] with input [Y, Z]'
  res = all(outcome_to_int(['Y', 'Z']) == [3, 6])
  print*, test_name, res
  overall_res = overall_res .and. res

  if (overall_res) then 
    print*, "test_rps: All tests succeded"
  else 
    error stop "test_rps: Tests failed"
  endif 
end program test_rps
