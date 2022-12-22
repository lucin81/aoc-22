program aoc_day12
  use m_kinds
  use m_day12, only: parse_input, print_map, dijkstra
  implicit none

  character(len=:), allocatable :: fname

  integer(i4) :: start_loc(2), best_loc(2)
  integer(i4), allocatable :: map(:,:), all_distances(:)
  integer(i4) :: distance, nn, j, i



  ! fname = 'data/test/d12p1.txt'
  fname = 'data/d12p1_input.txt'

  call parse_input(fname, map, start_loc, best_loc)
  ! call print_map(map, start_loc, best_loc)

  call dijkstra(map, start_loc, best_loc, distance)
  
  print*, "Answer day 12 part 1 =", distance

  nn=0
  do i = 1, size(map,1)
    do j = 1, size(map,2)
      if (map(i,j) == 97) then 
        nn=nn+1
        if (mod(nn,100)==0) print*, nn, 'of', count(map==97)
        call dijkstra(map, [i, j], best_loc, distance)
        all_distances = [all_distances, distance]
      endif
    enddo
  enddo

  ! print*, all_distances
  print*, "Answer day 12 part 1 =", minval(all_distances,dim=1, mask=all_distances>-1)


end program aoc_day12