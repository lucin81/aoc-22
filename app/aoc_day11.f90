program aoc_day11
  use m_kinds
  use m_day11, only: parse_input, monkey_t
  use stdlib_sorting, only: sort
  implicit none

  character(len=:), allocatable :: fname
  type(monkey_t), allocatable :: monkeys(:), original_monkeys(:)
  integer(i4) :: i, j
  integer(i4), allocatable :: counts(:)
  integer(i4), parameter :: n_rounds = 20, n_rounds_part2 = 10000

  ! fname = 'data/test/d11p1.txt'
  fname = 'data/d11p1_input.txt'

  call parse_input(fname, monkeys)

  original_monkeys = monkeys 
  do i = 1, n_rounds
    do j = 1, size(monkeys, 1)
      call monkeys(j)%play_turn(monkeys, reduce_worry=.true.)
    enddo
  enddo 

  counts = monkeys(:)%inspection_counter
  call sort(counts, reverse = .true.)

  print*, "Answer day 11 part 1 =", counts(1)*counts(2)

  ! Reset for the second part 
  monkeys = original_monkeys
  do i = 1, n_rounds_part2
    do j = 1, size(monkeys, 1)
      call monkeys(j)%play_turn(monkeys, reduce_worry=.false.)
    enddo
    ! if (any([1, 20, 1000, 10000]==i)) print*, i,":",monkeys(:)%inspection_counter
  enddo 

  counts = monkeys(:)%inspection_counter
  call sort(counts, reverse = .true.)
  print*, "Answer day 11 part 2 =", int(counts(1), i16) * int(counts(2), i16)

end program aoc_day11
