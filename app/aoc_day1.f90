program main
  !! Solve the part 1 and part 2 puzzles of AOC-22 
  use m_kinds
  use m_io, only: read_txt_1c
  use stdlib_sorting, only: sort
  implicit none

  integer(kind=i4), allocatable :: calories(:), calories_per_elf(:)
  integer(kind=i4) :: n_elves, i_elf, i
  
  calories = read_txt_1c('data/d1p1_input.txt')
  ! calories = read_txt_1c('data/test/d1p1.txt')
  n_elves = count(calories == 0) + 1

  allocate(calories_per_elf(n_elves), source = 0)
  i_elf = 1
  do i = 1, size(calories,1)
    if (calories(i) == 0) then 
      !go to the next elf
      i_elf = i_elf + 1
    else 
      calories_per_elf(i_elf) = calories_per_elf(i_elf) + calories(i)
    endif
  enddo 

  print*, "Anser part 1 =", maxval(calories_per_elf)

  call sort(calories_per_elf, reverse=.true.)

  print*, "Answer part 2 =", sum(calories_per_elf(1:3))
end program main
