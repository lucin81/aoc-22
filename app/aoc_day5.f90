program aoc_day5
  !! Solve the day 5 puzzles of AOC-22 
  use m_kinds
  use m_io, only: find_first_blank_line, read_nth_line, init_stacks, nrows, &
    read_moves
  use m_strings, only: string_to_int_array
  implicit none
  character(len=:), allocatable :: fname, str
  integer(kind=i4), allocatable :: stacks(:)
  character(len=1), allocatable :: crates(:, :)
  integer(kind=i4) :: n, max_stack_height, i, j
  integer(kind=i4), allocatable :: moves(:,:)

  ! fname = 'data/test/d5p1.txt'
  fname = 'data/d5p1_input.txt'
  
  n = find_first_blank_line(fname)
  str = read_nth_line(fname, n-1)
  stacks = string_to_int_array(str, ' ')
  max_stack_height = size(stacks) * (n-2)

  allocate(crates(size(stacks), max_stack_height))
  crates = '.'
  call init_stacks(stacks, 1, n-2, fname, crates)

  call read_moves(fname, n, moves)

  ! call print_stack(crates)
  do i = 1, size(moves, 1)
    ! print*, moves(i, :)
    call move(n = moves(i, 1), f = crates(moves(i, 2), :), &
      t = crates(moves(i, 3), :))
    ! call print_stack(crates)
  enddo 

  call print_stack(crates)

  block 
    character(len=1) :: answer(size(crates,1))

    do i=1, size(crates,1)
      answer(i) = crates(i, findloc(crates(i,:),'.', dim=1, back=.true.)+1)
    enddo
    print*, "Answer day 5 part 1 =", answer
  end block 

  crates = '.'
  call init_stacks(stacks, 1, n-2, fname, crates)
  ! call print_stack(crates)
  do i = 1, size(moves, 1)
    ! print*, moves(i, :)
    call move9001(n = moves(i, 1), f = crates(moves(i, 2), :), &
      t = crates(moves(i, 3), :))
    ! call print_stack(crates)
  enddo 

  block 
    character(len=1) :: answer(size(crates,1))

    do i=1, size(crates,1)
      answer(i) = crates(i, findloc(crates(i,:),'.', dim=1, back=.true.)+1)
    enddo
    print*, "Answer day 5 part 2 =", answer
  end block 

  contains 
  subroutine move(n, f, t)
    integer, intent(in) :: n
    character(len=1), intent(inout) :: f(:), t(:)
    integer :: ii, ind_t, ind_f
  
    
    do ii = 1, n 
      ind_f = findloc(f, '.', dim=1, back=.true.) + 1
      ind_t = findloc(t, '.', dim=1, back=.true.)
      t(ind_t) = f(ind_f)
      f(ind_f) = '.'
    enddo
  end subroutine move

  subroutine move9001(n, f, t)
    integer, intent(in) :: n
    character(len=1), intent(inout) :: f(:), t(:)
    integer :: ii, ind_t1, ind_t2, ind_f1, ind_f2
  
    ind_f1 = findloc(f, '.', dim=1, back=.true.) + 1
    ind_f2 = ind_f1 + n - 1
    ind_t1 = findloc(t, '.', dim=1, back=.true.)
    ind_t2 = ind_t1 - n + 1
    t(ind_t2:ind_t1) = f(ind_f1:ind_f2)
    f(ind_f1:ind_f2) = '.'
  end subroutine move9001

  subroutine print_stack(ss)
    character(len=1), intent(in) :: ss(:,:)
    integer :: ii, jj
    do ii=1, size(ss, 2)
      write(*, *) (ss(jj, ii), jj=1, size(ss,1))
    enddo  
  end subroutine print_stack
end program aoc_day5

