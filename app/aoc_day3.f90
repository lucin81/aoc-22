program main
  !! Solve the day 3 puzzles of AOC-22 
  use m_kinds
  use m_io, only: nrows, max_string_len, read_txt_char_array
  use m_strings, only: split_string, string_to_int
  use m_arrays, only: find_matches
  implicit none
  integer(kind=i4) :: nr, max_len, i
  character(len=:), allocatable :: fname, contents(:)
  character(len=:), allocatable :: s1, s2
  integer(kind=i4), allocatable :: v1(:), v2(:), tmp_shared_items(:), shared_items(:)

  ! fname = 'data/test/d3p1.txt'
  fname = 'data/d3p1_input.txt'
  nr = nrows(fname)
  max_len = max_string_len(fname)

  allocate(character(len = max_len) :: contents(nr))
  contents = read_txt_char_array(fname, max_len, nr)

  allocate(shared_items(0))
  do i = 1, nr
    call split_string(contents(i), s1, s2)
    v1 = string_to_int(s1)
    v2 = string_to_int(s2)

    tmp_shared_items = find_matches(v1, v2)
    if (size(tmp_shared_items) > 0) shared_items = [shared_items, tmp_shared_items]
  enddo
    
  print*, "Number of rucksacks=", nr
  print*, "Longest list of contents=", max_len
  print*, "Number of shared items =", size(shared_items)

  print*, "Answer day 2 part 1 =", sum(shared_items)

  ! print*, "Answer day 2 part 2 =", sum(my_round_scores)
end program main
