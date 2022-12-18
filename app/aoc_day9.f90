program aoc_day9
  use m_kinds
  use m_io, only: nrows
  use m_day9, only: compute_grid_size, compute_visited, rope_dynamics, print_visited
  implicit none
  character(len=:), allocatable :: fname
  integer(i4) :: nr_in, i, iu, j
  character(len=1), allocatable :: dir(:)
  integer(i4), allocatable :: nsteps(:)
  integer(i4) :: min_row, max_row, min_col, max_col
  logical, allocatable :: visited(:, :)

  ! fname = 'data/test/d9test.txt'
  ! fname = 'data/test/d9p1.txt'
  ! fname = 'data/test/d9p2.txt'
  fname = 'data/d9p1_input.txt'

  nr_in = nrows(fname)
  ! nr_in = 50
  allocate(dir(nr_in), nsteps(nr_in))
  open (newunit=iu, file=trim(fname), action='read', form='formatted')
  do i = 1, nr_in
    read(iu, '(a1,1x,i4)') dir(i), nsteps(i)
    ! print*, dir(i), nsteps(i)
  enddo

  call compute_grid_size(dir, nsteps, min_row, max_row, min_col, max_col)

  allocate(visited(min_row:max_row, min_col:max_col), source = .false.)

  call compute_visited(dir, nsteps, min_row, min_col, visited)

  print*, "Answer day 9 part 1 =", count(visited)

  visited = .false. 
  call rope_dynamics(dir, nsteps, min_row, min_col, 10, visited, with_print=.false.)

  print*, "Answer day 9 part 2 =", count(visited)

  ! call print_visited(visited, min_row, min_col, max_row, max_col)
end program aoc_day9
