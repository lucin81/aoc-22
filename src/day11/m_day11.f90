module m_day11
  use m_kinds
  use m_strings, only: split, string_to_int_array
  implicit none
  private
  
  type :: monkey_t
    integer(i4) :: id
    integer(i16), allocatable :: worry_level(:)
    character(len=1) :: operator
    integer(i16) :: operand
    integer(i4) :: test
    integer(i4) :: if_true
    integer(i4) :: if_false
    integer(i4) :: inspection_counter = 0
  contains
    procedure :: play_turn
    procedure :: print_notes
  end type

  public :: parse_input, monkey_t
contains

  subroutine print_notes(this)
    class(monkey_t), intent(in) :: this

    integer(i4) :: n_items

    if (.not.allocated(this%worry_level)) then 
      n_items = 0 
    else 
      n_items = size(this%worry_level, 1)
    endif

    print*, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    print*, "Monkey ", this%id
    print*, "Number of Items: ", n_items
    print*, "Items: ", this%worry_level
    print*, "Number of items inspected: ", this%inspection_counter
    print*, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

  end subroutine 

  subroutine parse_input(fname, monkeys)
    character(*), intent(in) :: fname 
    character(len=:), allocatable :: s1, s2, tmp, tmp2
    character(len=1024) :: buffer
    integer(i4) :: iu, i, ios, val, id

    integer(i4), allocatable :: worry_level(:)
    integer(i4) :: operand, test_value, true_value, false_value
    character(len=1) :: operator
    type(monkey_t), intent(out), allocatable :: monkeys(:)

    open (newunit=iu, file=trim(fname), action='read', form='formatted')
    id = 0
    if (allocated(monkeys)) deallocate(monkeys)
    allocate(monkeys(0))
    do
      read(iu, '(a)', iostat=ios) buffer
      if (ios /= 0) exit
  
      if (buffer(1:6) == "Monkey") then 
        ! Add a new monkey
        ! Next five lines contain notes
        do i = 1, 5
          read(iu, '(a)', iostat=ios) buffer
          if (ios /= 0) error stop 'Error parsing monkey'

          call split(buffer, ":", s1, s2)

          select case (trim(adjustl(s1)))
          case("Starting items")
            worry_level = string_to_int_array(s2,' ')
          case("Operation")
            tmp=trim(adjustl(s2))
            operator = tmp(11:11)
            tmp2 = trim(adjustl(tmp(12:)))
            if (tmp2 == 'old') then 
              if (operator == '*') then 
                operator = '^'
                operand = 2
              else if (operator == '+') then 
                operator = '*'
                operand = 2
              else 
                error stop 'Error parsing operation'
              endif
            else 
              read(tmp2, '(i3)', iostat=ios) operand
              if (ios/=0) error stop 'Error parsing operation'
            endif
          case("Test")
            tmp = trim(adjustl(s2))
            read(tmp(13:), '(i4)', iostat=ios) test_value
            if (ios/=0) error stop 'Error parsing test'
          case("If true")
            tmp = trim(adjustl(s2))
            read(tmp(16:), '(i4)', iostat=ios) true_value
            if (ios/=0) error stop 'Error parsing If true'
          case("If false")
            tmp = trim(adjustl(s2))
            read(tmp(16:), '(i4)', iostat=ios) false_value
            if (ios/=0) error stop 'Error parsing If false'
          end select
        enddo

        monkeys = [monkeys, monkey_t(id, worry_level, operator, operand, test_value, &
          true_value, false_value)]
        id = id + 1
      endif
    enddo
  end subroutine parse_input

  subroutine play_turn(this, monkeys, reduce_worry)
    class(monkey_t), intent(inout) :: this
    type(monkey_t), intent(inout) :: monkeys(:)
    logical, intent(in) :: reduce_worry
    integer(i16) :: calm_worry 
    
    integer(i4) :: n_items, throws_to
    integer(i16) :: item_worry
    integer(i4) :: i 

    if (.not.allocated(this%worry_level)) return 

    calm_worry = 1
    do i = 1, size(monkeys, 1)    
      calm_worry = calm_worry * monkeys(i)%test
    enddo


    n_items = size(this%worry_level)
    do while (n_items > 0) 
      ! Initial worry level
      item_worry = this%worry_level(1)

      ! Inspection increases worry level 
      select case (this%operator)
      case ('+')
        item_worry = item_worry + this%operand
      case ('*')
        item_worry = item_worry * this%operand
      case ('^')
        item_worry = item_worry ** this%operand
      case default 
        error stop this%operator//" is not a valid operation."
      end select 

      ! Finish inspection: increase counter and reduce worry level.
      this%inspection_counter = this%inspection_counter + 1
      item_worry = mod(item_worry, calm_worry)
      if (reduce_worry) then 
        item_worry = item_worry / 3
      endif

      ! Test
      if (mod(item_worry, this%test) == 0) then 
        throws_to = this%if_true + 1
      else 
        throws_to = this%if_false + 1
      endif

      ! Throw the item and remove it from the list of this monkey 
      if (.not.allocated(monkeys(throws_to)%worry_level)) then 
        allocate(monkeys(throws_to)%worry_level(0))
      endif 

      monkeys(throws_to)%worry_level = [monkeys(throws_to)%worry_level, item_worry]
      if (n_items == 1) then 
        ! This was the last item 
        deallocate(this%worry_level)
        n_items = 0 
      else 
        this%worry_level = [this%worry_level(2:)]
        n_items = size(this%worry_level, 1)
      endif
    enddo
  end subroutine play_turn
end module m_day11