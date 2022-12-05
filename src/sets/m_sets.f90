module m_sets
  use m_kinds
  use m_strings, only: split, to_int
  implicit none
  private

  type :: set_t
    !! Define an integer complete set (an interval)
    private
    integer(kind=i4) :: lb
    integer(kind=i4) :: ub
    character(len=:), allocatable :: name
    character(len=1) :: sep = '-'
  contains
    procedure, public :: get_bounds => bounds
    procedure, public :: print => print_set
    procedure, public :: is_subset
    procedure, public :: intersects
  end type

  interface set_t
    module procedure set_constructor
    module procedure set_constructor_from_array
    module procedure set_constructor_from_string
  end interface

  public :: set_t
contains

  function set_constructor(lb, ub, name) result(new_set)
    !! Returns a new set with a given lower and upper bound and name
    integer(kind=i4), intent(in) :: lb, ub
    character(len=*), intent(in) :: name
    type(set_t) :: new_set

    new_set%name = trim(name)
    new_set%lb = lb
    new_set%ub = ub
  end function set_constructor

  function set_constructor_from_string(name, sep) result(new_set)
    !! Returns a new set with a given lower and upper bound and name
    character(len=*), intent(in) :: name
    character(len=1), optional :: sep
    type(set_t) :: new_set

    character(len=:), allocatable :: s_lb, s_ub
    integer(kind=i4) :: lb, ub

    new_set%name = trim(name)

    if (present(sep)) then
      new_set%sep = sep
    end if

    call split(name, new_set%sep, s_lb, s_ub)

    new_set%lb = to_int(s_lb)
    new_set%ub = to_int(s_ub)
  end function set_constructor_from_string

  function set_constructor_from_array(arr, name) result(new_set)
    !! Returns a new set from an array
    integer(kind=i4), intent(in) :: arr(:)
    character(len=*), intent(in) :: name
    type(set_t) :: new_set

    new_set%name = trim(name)
    new_set%lb = minval(arr)
    new_set%ub = maxval(arr)
  end function set_constructor_from_array

  elemental function is_subset(this, other) result(res)
    !! Return true if this is subset of other and false otherwise
    class(set_t), intent(in) :: this
    type(set_t), intent(in) :: other
    logical :: res

    res = this%lb >= other%lb .and. this%ub <= other%ub
  end function is_subset

  elemental function intersects(this, other) result(res)
  !! Return true if this intersects other and false otherwise
    class(set_t), intent(in) :: this
    type(set_t), intent(in) :: other
    logical :: res

    res = this%is_subset(other) .or. &
          (this%lb <= other%ub .and. this%ub >= other%lb) .or. &
          (this%lb >= other%ub .and. this%ub <= other%lb)
  end function intersects

  pure function bounds(self) result(res)
    !! Returns the bounds of the set
    class(set_t), intent(in) :: self
    integer(kind=i4) :: res(2)

    res(1) = self%lb
    res(2) = self%ub
  end function bounds

  subroutine print_set(self)
    !! Prints a set
    class(set_t), intent(in) :: self

    write (*, 1001) trim(self%name), self%lb, self%ub
1001 format('Set ', a, ' = {n | n is integer, and ', g0, '<= n <=', g0, '}')
  end subroutine print_set
end module m_sets
