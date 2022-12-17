program aoc_day8
  use m_kinds
  use m_io, only: nrows
  implicit none
  character(len=:), allocatable :: fname
  integer(i4) :: nr, iu, ios, i, nc, j
  character(len=1) :: a
  character(len=100) :: iom
  character(len=1024) :: buffer
  integer(i4),allocatable :: t(:,:)
  integer(i4) :: n_visible, this_height, scenic_score, max_scenic_score, tmp
  logical :: is_visible


  ! fname = 'data/test/d8p1.txt'
  fname = 'data/d8p1_input.txt'

  nr = nrows(fname)
  ! Read data 
  open (newunit=iu, file=trim(fname), action='read', form='formatted')
  do i = 1, nr
    read(iu, '(a)') buffer
    nc = len_trim(buffer)
    if (.not.allocated(t)) allocate(t(nc,nr))
    do j = 1, nc 
      read(buffer(j:j), '(i1)') t(j, i)
    enddo
  enddo
  close(iu)

  n_visible = 2 * (nr - 1) + 2 * (nc - 1) 
  max_scenic_score = 0
  do i = 2, nr-1
    do j = 2, nc-1
      this_height = t(j, i)

      is_visible = &
        count(t(1:(j-1), i) >= this_height) == 0 .or. & ! Visible from the left
        count(t(j, 1:(i-1)) >= this_height) == 0 .or. & ! Visible from the top 
        count(t((j+1):nc, i) >= this_height) == 0 .or. & ! Visible from the right 
        count(t(j, (i+1):nr) >= this_height) == 0 ! Visible from the right 
      
      if (is_visible) n_visible = n_visible + 1

      ! Calculate scenic score
      scenic_score = 1
      tmp = findloc(t(j, (i-1):1:-1) >= this_height,.true., 1)
      if (tmp == 0) tmp = i-1 
      scenic_score = scenic_score * tmp
      print*, "(j, 1:(i-1)):",tmp

      tmp = findloc(t(j, (i+1):nr) >= this_height,.true., 1)
      if (tmp == 0) tmp = (nr - i)
      scenic_score = scenic_score * tmp
      print*, "(j, (i+1):nr):", tmp

      tmp = findloc(t((j-1):1:-1, i) >= this_height,.true., 1)
      if (tmp == 0) tmp = j - 1
      scenic_score = scenic_score * tmp
      print*, "(1:(j-1), i):", tmp

      tmp = findloc(t((j+1):nc, i) >= this_height,.true., 1)
      if (tmp == 0) tmp = nc - j
      scenic_score = scenic_score * tmp
      print*, "((j+1):nc, i):", tmp

      max_scenic_score = max(max_scenic_score, scenic_score)
      print *, "Pixel row and col:",i,j, this_height, scenic_score
      ! print*, "(j, 1:(i-1)):",findloc(t(j, 1:(i-1)) >= this_height,.true., 1)
      ! print*, "(j, (i+1):nr):",findloc(t(j, (i+1):nr) >= this_height,.true., 1)
      ! print*, "(1:(j-1), i):",findloc(t(1:(j-1), i) >= this_height,.true., 1)
      ! print*, "((j+1):nc, i):",findloc(t((j+1):nc, i) >= this_height,.true., 1)
      
    enddo
  enddo

  print*, "Answer day 8 part 1 =", n_visible
  print*, "Answer day 8 part 2 =", max_scenic_score
end program aoc_day8