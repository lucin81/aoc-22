module m_arrays
  use m_kinds
  use stdlib_sorting, only: sort
  implicit none
  private

  public :: find_matches, unique
contains

  pure function find_matches(values, array) result(res)
    !! Extract the values in [values] that are contained in [array]
    !! values and array might be of different size
    !! returns an array of size equal to the number of matching values
    !! or an unallocated array if there is no match

    integer(kind=i4), intent(in) :: values(:), array(:)
    integer(kind=i4), allocatable :: res(:)
    integer(kind=i4), allocatable :: unique_values(:)
    integer(kind=i4) :: i

    unique_values = unique(values)
    allocate (res(0))
    do i = 1, size(unique_values)
      if (any(array == unique_values(i))) res = [res, unique_values(i)]
    end do
  end function find_matches

  pure function unique(vec) result(vec_unique)
    !! Return only the unique values from vec.
    !! Taken from https://degenerateconic.com/unique.html

    integer(kind=i4), intent(in) :: vec(:)
    integer(kind=i4), allocatable :: vec_unique(:)

    integer(kind=i4) :: i, num
    logical :: mask(size(vec))

    mask = .false.

    do i = 1, size(vec)
      !count the number of occurrences of this element:
      num = count(vec(i) == vec)

      if (num == 1) then
        !there is only one, flag it:
        mask(i) = .true.
      else
        !flag this value only if it hasn't already been flagged:
        if (.not. any(vec(i) == vec .and. mask)) mask(i) = .true.
      end if

    end do

    !return only flagged elements:
    allocate (vec_unique(count(mask)))
    vec_unique = pack(vec, mask)
    call sort(vec_unique)
  end function unique
end module m_arrays
