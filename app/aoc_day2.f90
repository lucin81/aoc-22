program main
  !! Solve the day 2 puzzles of AOC-22 
  use m_kinds
  use m_io, only: read_txt_2c
  use m_rps, only: shape_to_int, calculate_p1_score, calculate_p1_score_from_outcome, &
    outcome_to_int
  implicit none

  character(len=1), allocatable :: shape_inputs(:, :)
  integer(kind=i4), allocatable :: opponent_strategy(:), my_strategy(:)
  integer(kind=i4), allocatable :: my_round_scores(:), outcomes(:)
  
  ! shape_inputs = read_txt_2c('data/test/d2p1.txt')
  shape_inputs = read_txt_2c('data/d2p1_input.txt')

  opponent_strategy = shape_to_int(shape_inputs(:, 1))
  my_strategy = shape_to_int(shape_inputs(:, 2))

  my_round_scores = calculate_p1_score(p1 = my_strategy, p2 = opponent_strategy)

  print*, "Answer day 2 part 1 =", sum(my_round_scores)

  outcomes = outcome_to_int(shape_inputs(:, 2))
  my_round_scores = calculate_p1_score_from_outcome(outcome = outcomes, p2 = opponent_strategy)
  print*, "Answer day 2 part 2 =", sum(my_round_scores)
end program main
