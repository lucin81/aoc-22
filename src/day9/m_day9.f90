module m_day9
  use m_kinds
  implicit none
  private

  public :: compute_grid_size, compute_visited, rope_dynamics, print_visited
contains

  subroutine compute_grid_size(dir, nsteps, min_row, max_row, min_col, max_col)
    character(len=1), allocatable, intent(in) :: dir(:)
    integer(i4), allocatable, intent(in) :: nsteps(:)
    integer(i4), intent(out) :: min_row, max_row, min_col, max_col
    integer(i4) :: k, row, col

    min_row = 1; max_row = 1
    min_col = 1; max_col = 1
    row = 1; col = 1
    do k = 1, size(dir, 1)
      select case (dir(k))
      case ('R')
        col = col + nsteps(k)
        if (col > max_col) max_col = col
      case ('L')
        col = col - nsteps(k)
        if (col < min_col) min_col = col
      case ('U')
        row = row - nsteps(k)
        if (row < min_row) min_row = row
      case ('D')
        row = row + nsteps(k)
        if (row > max_row) max_row = row
      case default
        error stop 'Error in parsing instructions'
      end select
      ! print*, dir(k), nsteps(k), n, ii, jj, nr, nc, si, sj
    end do
  end subroutine compute_grid_size

  subroutine head_movement(dir, di, dj)
    character(len=1), intent(in) :: dir
    integer(i4), intent(out) :: di, dj

    di = 0; dj = 0
    select case (dir)
    case ('R')
      dj = 1
    case ('L') 
      dj = -1
    case ('U')
      di = -1
    case ('D')
      di = 1
    case default 
      error stop 'unknown direction'
    end select 
  end subroutine head_movement

  subroutine move_node(Hi, Hj, Ti, Tj, is_tail, visited, min_row, min_col)
    integer(i4), intent(in) :: Hi, Hj
    integer(i4), intent(inout) :: Ti, Tj
    integer(i4), intent(in) :: min_row, min_col
    logical, intent(inout) :: visited(min_row:, min_col:)
    logical, intent(in) :: is_tail

    do while ( (abs(Hi-Ti)>1) .or. abs(Hj-Tj)>1 )
      if (Ti /= Hi) Ti = Ti + sign(1, Hi - Ti) 
      if (Tj /= Hj) Tj = Tj + sign(1, Hj - Tj)
      if (is_tail) visited(Ti, Tj) = .true. 
      ! print*, "Head: ", Hi, Hj
      ! print*, "Tail: ", Ti, Tj 
    enddo 
  end subroutine move_node

  subroutine move_tail(Hi, Hj, Ti, Tj, min_row, min_col, visited)
    integer(i4), intent(in) :: Hi, Hj
    integer(i4), intent(inout) :: Ti , Tj
    integer(i4), intent(in) :: min_row, min_col
    logical, intent(inout) :: visited(min_row:, min_col:)

    if (Hi == Ti) then
      ! They are on the same row
      if (Hj > Tj + 1) then 
        ! Head is moving right 
        visited(Ti, (Tj + 1):(Hj - 1)) = .true.
        Tj = Hj - 1
      else if (Hj < Tj - 1) then 
        ! Head is moving left 
        visited(Ti, (Hj + 1):(Tj - 1)) = .true.
        Tj = Hj + 1
      endif 
    else if (Hj == Tj) then 
      ! They are on the same column 
      if (Hi > Ti + 1) then 
        ! Head is moving down
        visited((Ti + 1):(Hi - 1), Tj) = .true.
        Ti = Hi - 1
      else if (Hi < Ti - 1) then 
        ! Head is moving up 
        visited((Hi + 1):(Ti - 1), Tj) = .true.
        Ti = Hi + 1
      endif 
    else 
      ! Tail is juping on the diagonal 
      ! It changes row if Hj > Tj + 1 or Hj < Tj - 1 or column if Hi > Ti + 1 or Hi < Ti - 1
      if (Hj > Tj + 1) then
        Ti = Hi
        visited(Ti, (Tj + 1):(Hj - 1)) = .true.
        Tj = Hj - 1
      else if (Hj < Tj - 1) then 
        Ti = Hi 
        visited(Ti, (Hj + 1):(Tj - 1)) = .true.
        Tj = Hj + 1
      else if (Hi > Ti + 1) then 
        Tj = Hj 
        visited((Ti + 1):(Hi - 1), Tj) = .true.
        Ti = Hi - 1
      else if (Hi < Ti - 1) then 
        Tj = Hj 
        visited((Hi + 1):(Ti - 1), Tj) = .true.
        Ti = Hi + 1
      end if
    end if
  end subroutine move_tail

  subroutine rope_dynamics(dir, nsteps, min_row, min_col, n_nodes, visited, with_print)
    character(len=1), intent(in) :: dir(:)
    integer(i4), intent(in) :: nsteps(:)
    integer(i4), intent(in) :: min_row, min_col, n_nodes
    logical, intent(inout) :: visited(min_row:, min_col:)

    integer(i4) :: rope_i(n_nodes), rope_j(n_nodes), ii, k, di, dj, n
    logical :: is_tail, with_print

    visited(1, 1) = .true.
    rope_i = 1; rope_j = 1

    do k = 1, size(dir,1)
      call head_movement(dir(k), di, dj)
      do n = 1, nsteps(k)
        ! Move the head 
        rope_i(1) = rope_i(1) + di
        rope_j(1) = rope_j(1) + dj
        is_tail = .false. 
        do ii = 2, n_nodes
          ! Move the nodes 
          ! print*, "Move ",k, ", Node ", ii
          if (ii==n_nodes) is_tail = .true.
          call move_node(rope_i(ii-1), rope_j(ii-1), rope_i(ii), rope_j(ii), &
            is_tail, visited, min_row, min_col)
        enddo 
      enddo 
      if (with_print) then 
        print*, "----- Step ", k, " ---------- "
        call print_rope(rope_i, rope_j, lbound(visited, 1), &
          lbound(visited, 2), ubound(visited, 1), ubound(visited, 2))
      endif
    enddo
  end subroutine rope_dynamics

  subroutine print_rope(irow, icol, min_row, min_col, max_row, max_col)
    integer(i4), intent(in) :: irow(:), icol(:), min_row, min_col, max_row, max_col
    character(len=1) :: rope_config(min_row:max_row, min_col:max_col)

    character(len=1024) :: fmt
    character(len=1) :: node_str
    integer(i4) :: i, j

    write(fmt, '(a,i3,a)') '(', size(rope_config,2), '(a1))'
    rope_config = '.'
    rope_config(irow(1):irow(1), icol(1):icol(1)) = 'H'
    do i = 1, size(irow, 1)-1
      write(node_str, '(i1)') i
      rope_config(irow(i+1):irow(i+1), icol(i+1):icol(i+1)) = node_str
    enddo

    do i = min_row, max_row
      write(*, fmt) (rope_config(i, j), j=min_col, max_col)
    enddo
  end subroutine print_rope

  subroutine print_visited(visited, min_row, min_col, max_row, max_col)
    integer(i4), intent(in) :: min_row, min_col, max_row, max_col
    logical, intent(in) :: visited(min_row:, min_col:)
    character(len=1) :: rope_config(min_row:max_row, min_col:max_col)

    character(len=1024) :: fmt
    character(len=1) :: node_str
    integer(i4) :: i, j

    write(fmt, '(a,i3,a)') '(', size(visited,2), '(a1))'
    rope_config = '.'

    do i = min_row, max_row
      do j = min_col, max_col
        if (visited(i, j)) rope_config(i,j) = '#'
      enddo
      write(*, fmt) (rope_config(i, j), j=min_col, max_col)
    enddo
  end subroutine print_visited

  subroutine compute_visited(dir, nsteps, min_row, min_col, visited)
    character(len=1), intent(in) :: dir(:)
    integer(i4), intent(in) :: nsteps(:)
    integer(i4), intent(in) :: min_row, min_col
    logical, intent(inout) :: visited(min_row:, min_col:)
    integer(i4) :: nr, nc
    integer(i4) :: k, ii, jj, n, Hi, Hj, Ti, Tj

    visited(1, 1) = .true.
    Hi = 1; Hj = 1
    Ti = 1; Tj = 1
    do k = 1, size(dir, 1)
      select case (dir(k))
      case ('R')
        Hj = Hj + nsteps(k)
        if (Hi == Ti) then
          ! They are on the same row
          if (Hj > Tj + 1) then 
            visited(Ti, Tj:(Hj - 1)) = .true.
            Tj = Hj - 1
          end if
        else
          ! Moves diagonally, so T changes row if Hj>Tj+1
          if (Hj > Tj + 1) then
            Ti = Hi
            visited(Ti, (Tj + 1):(Hj - 1)) = .true.
            Tj = Hj - 1
          end if
        end if
      case ('L')
        Hj = Hj - nsteps(k)
        if (Hi == Ti) then
          ! They are on the same row
          if (Hj < Tj - 1) then
            visited(Ti, (Hj + 1):Tj) = .true.
            Tj = Hj + 1
          end if
        else
          ! Moves diagonally, so T changes row if Hj<Tj-1
          if (Hj < Tj - 1) then
            Ti = Hi
            visited(Ti, (Hj + 1):(Tj - 1)) = .true.
            Tj = Hj + 1
          end if
        end if
      case ('U')
        Hi = Hi - nsteps(k)
        if (Hj == Tj) then
          ! They are on the same column
          if (Hi < Ti - 1) then
            visited((Hi + 1):(Ti - 1), Tj) = .true.
            Ti = Hi + 1
          end if
        else
          ! Moves diagonally, so T changes col if Hi<Ti-1
          if (Hi < Ti - 1) then
            Tj = Hj
            visited((Hi + 1):(Ti - 1), Tj) = .true.
            Ti = Hi + 1
          end if
        end if
      case ('D')
        Hi = Hi + nsteps(k)
        if (Hj == Tj) then
          ! They are on the same column
          if (Hi > Ti + 1) then
            visited((Ti + 1):(Hi - 1), Tj) = .true.
            Ti = Hi - 1
          end if
        else
          ! Moves diagonally, so T changes col if Hi>Ti+1
          if (Hi > Ti + 1) then
            Tj = Hj
            visited((Ti + 1):(Hi - 1), Tj) = .true.
            Ti = Hi - 1
          end if
        end if
      case default
        error stop 'Error in parsing instructions'
      end select
      ! print*, dir(k), nsteps(k), Hi, Hj, Ti, Tj, count(visited)
      ! if (k == 1) print *, 1, 1, 1, 1, dir(k), nsteps(k)
      ! if (k < size(dir, 1)) print *, Hi, Hj, Ti, Tj, dir(k + 1), nsteps(k + 1), count(visited)
    end do
  end subroutine compute_visited

end module m_day9
