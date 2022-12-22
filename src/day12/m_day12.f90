module m_day12
  use m_kinds
  use m_io, only: nrows
  implicit none
  private

  ! row/col: up, right, down, left
  integer(i4), parameter :: delta(4,2) = reshape([-1,0,1,0,0,1,0,-1], [4,2])

  public :: parse_input, print_map, dijkstra
contains

  subroutine print_map(map, start_loc, best_loc)
    integer(i4), intent(in) :: map(:, :)
    integer(i4), intent(in) :: start_loc(2), best_loc(2)
    character(len=1024) :: fmt
    integer(i4) :: i, j

    print *, "Start location:", start_loc
    print *, "Best signal at:", best_loc
    do i = 1, size(map, 1)
      write (*, *) (map(i, j), j=1, size(map, 2))
    end do
  end subroutine print_map

  subroutine parse_input(fname, map, start_loc, best_loc)
    character(*), intent(in) :: fname
    character(len=1024) :: buffer
    integer(i4) :: iu, i, j, ios, nr, nc

    integer(i4), allocatable, intent(out) :: map(:, :)
    integer(i4), intent(out) :: best_loc(2), start_loc(2)

    nr = nrows(fname)

    open (newunit=iu, file=trim(fname), action='read', form='formatted')
    read (iu, '(a)', iostat=ios) buffer
    rewind (iu)

    nc = len_trim(buffer)
    allocate (map(nr, nc))

    do i = 1, nr
      read (iu, '(a)', iostat=ios) buffer
      if (ios /= 0) error stop 'Error reading input.'

      do j = 1, nc
        select case (buffer(j:j))
        case ('S')
          start_loc = [i, j]
          map(i, j) = ichar('a')
        case ('E')
          best_loc = [i, j]
          map(i, j) = ichar('z')
        case default
          map(i, j) = ichar(buffer(j:j))
        end select
      end do
    end do
  end subroutine parse_input

  subroutine dijkstra(map, source, target, shortest_path_length)
    integer(i4), intent(in) :: map(:, :)
    integer(i4), intent(in) :: source(2), target(2)
    integer(i4), intent(out) :: shortest_path_length

    integer(i4), allocatable :: dist(:), prev(:), q(:), path(:)
    logical, allocatable :: processed(:)
    integer(i4) :: nr, nc, j, i, nn, alt, target_loc, source_loc, &
      ij(2), loc, n, nloc, diff

    nr = size(map, 1)
    nc = size(map, 2)
    allocate(dist(nr*nc), prev(nr*nc), q(nr*nc), processed(nr*nc))

    prev = -99
    ! Initialize dist to infinity and 0 for the start pixel
    dist = 999
    source_loc = source(1)+nr*(source(2)-1)
    dist(source_loc) = 0
    processed = .false.

    target_loc = target(1) + (target(2)-1) * nr
    do while (count(processed) < nr*nc)
      ! if (mod(count(processed), 500) == 0) print*, count(processed), "of", size(processed,1)
      loc = minloc(dist,dim=1,mask = .not.processed)

      if (loc == target_loc) exit

      processed(loc) = .true.

      i = mod(loc-1, nr) + 1
      j = (loc-1)/nr + 1

      ! print*, loc, nr, i, j, source

      do n = 1, 4
        ij = [i, j] + delta(n, :)
        ! print*, i, j, ij, n
        if (ij(1)<1 .or. ij(1)>nr .or. ij(2)<1 .or. ij(2)>nc) cycle 
        diff = map(ij(1), ij(2)) - map(i, j) 
        if (diff > 1) cycle  
        nloc = ij(1) + nr * (ij(2)-1)
        alt = dist(loc) + 1
        ! print*, nloc, alt, dist(nloc)
        if (alt < dist(nloc)) then 
          dist(nloc) = alt
          prev(nloc) = loc
        endif
        ! print*, nloc, alt, dist(nloc)
      enddo
    enddo

    allocate(path(0))
    loc = target_loc
    if (prev(loc) /= -99 .or. loc == source_loc ) then 
      do while (loc  /= -99)
        path = [loc, path]
        loc = prev(loc)
      enddo
    endif

    shortest_path_length = size(path,1)-1
  end subroutine dijkstra
end module m_day12
