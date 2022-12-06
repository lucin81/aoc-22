program aoc_day6
  use m_kinds
  use m_arrays, only: unique
  implicit none

  character(len=:), allocatable :: fname
  character(len=:), allocatable :: buffer
  character(len=100) :: iom
  integer(kind=i4) :: iu, ios, i, marker_pos

  ! fname = 'data/test/d6p1.txt'
  fname = 'data/d6p1_input.txt'

  open (newunit=iu, file=trim(fname), action='read', form='formatted', &
    access='direct', recl=1)
    i = 0
    do 
      i=i+1
      read(iu, '(a1)', rec=i, iostat=ios)
      if (ios /= 0) then 
        i = i - 1
        exit
      endif 
    enddo 
  close(iu)

  ! print*, i
  allocate(character(len=i) :: buffer)

  open (newunit=iu, file=trim(fname), action='read')
    read(iu, '(a)', iostat=ios) buffer
  close(iu)
  
  call find_first_marker(buffer, marker_pos, marker_size=4)

  print*, "Answer day 6 part 1 =", marker_pos

  call find_first_marker(buffer, marker_pos, marker_size=14)

  print*, "Answer day 6 part 2 =", marker_pos

  contains

  subroutine find_first_marker(buffer, marker_pos, marker_size)
    character(len=*), intent(in) :: buffer
    integer(kind=i4), intent(in) :: marker_size
    integer(kind=i4), intent(out) :: marker_pos
    character(len=marker_size) :: sub_buf
    integer(kind=i4) :: k, j, n_distinct_characters

    do k = 1, len_trim(buffer)
      sub_buf = buffer(k:(k+marker_size-1))
      n_distinct_characters = size(unique([(ichar(sub_buf(j:j)), j=1,marker_size)]))
      ! print*, sub_buf, n_distinct_characters, k+marker_size-1
      if (n_distinct_characters == marker_size) then 
        ! Found the first marker the first time that marker_size characters are distinct
        marker_pos = k + marker_size - 1
        ! print*, sub_buf, n_distinct_characters, marker_pos
        return
      endif
    enddo
  end subroutine find_first_marker
end program aoc_day6