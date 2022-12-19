program aoc_day10
  use m_kinds
  use m_io, only: nrows
  use m_day10, only: print_crt
  implicit none
  
  character(len=:), allocatable :: fname
  character(len=4), allocatable :: instruction(:)
  integer(i4), allocatable :: val(:), x(:), cycle_index(:), signal_strenghts(:)
  integer(i4) :: i, nr, iu, n_cycles, cycle_counter, sprite_pos

  character(len=1) :: crt(0:(40*6-1))

  ! fname = 'data/test/d10p1.txt'
  ! fname = 'data/test/d10test.txt'
  fname = 'data/d10p1_input.txt'

  nr = nrows(fname)
  allocate(instruction(nr), val(nr))
  val = 0 
  open (newunit=iu, file=trim(fname), action='read', form='formatted')
  do i = 1, nr
    read(iu, '(a4,1x,i6)') instruction(i), val(i)
  enddo

  n_cycles = count(instruction == 'noop') + 2 * count(instruction == 'addx')
  allocate(x(1:(n_cycles+1)))
  x = 0 
  cycle_counter = 1
  x(1) = 1
  do i = 1, nr
    select case (instruction(i))
    case('noop')
      cycle_counter = cycle_counter + 1
      x(cycle_counter) = x(cycle_counter - 1)
    case('addx')
      cycle_counter = cycle_counter + 2
      x((cycle_counter - 1):cycle_counter) = x(cycle_counter - 2)
      x(cycle_counter) = x(cycle_counter) + val(i)
    case default 
      error stop 'Unknown instruction'
    end select
  enddo

  cycle_index = [(i, i = 1, (n_cycles+1))]
  signal_strenghts = pack(cycle_index, mod(cycle_index - 20, 40) == 0) * pack(x, mod(cycle_index - 20, 40) == 0)
  
  print*, "Answer day 10 part 1 =", sum(signal_strenghts)

  crt = ' '
  do i = 1, n_cycles
    sprite_pos = x(i)
    if (any([(i, i=sprite_pos-1,sprite_pos+1)] == mod(i-1, 40))) crt(i-1:i-1) = ';'
  enddo
  
  call print_crt(crt, 6, 40)
end program aoc_day10