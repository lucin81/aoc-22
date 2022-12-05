program main
  !! Solve the day 4 puzzles of AOC-22 
  use m_kinds
  use m_io, only: read_assignemnt_pairs
  use m_sets, only: set_t
  implicit none
  integer(kind=i4) :: most_urgent, overlaps
  character(len=:), allocatable :: fname
  type(set_t), allocatable :: assignments(:, :)


  ! fname = 'data/test/d4p1.txt'
  fname = 'data/d4p1_input.txt'

  assignments = transpose(read_assignemnt_pairs(fname))
  most_urgent = count(assignments(:, 1)%is_subset(assignments(:, 2)) .or. &
                      assignments(:, 2)%is_subset(assignments(:, 1)))
  
  print*, "Answer day 4 part 1 =", most_urgent

  ! Intersection is symmetric so no need to test both sides
  overlaps = count(assignments(:, 1)%intersects(assignments(:, 2))) 

  print*, "Answer day 4 part 2 =", overlaps
end program main
