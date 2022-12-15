program aoc_day7
  use m_kinds
  use m_strings, only: split
  use m_io, only: nrows
  use stdlib_sorting, only: sort
  implicit none

  type :: dir_names
    character(len=:), allocatable :: name, parent
    integer(i4), allocatable :: i_children(:)
    integer(i4) :: i_node, i_parent, n_children
    integer(i4) :: depth
  end type

  character(len=:), allocatable :: fname
  character(len=100) :: buffer
  character(len=:), allocatable :: cmd, s1, s2, s3
  ! character(len=10), allocatable :: directories(:)
  character(len=10) :: name
  character(len=100) :: iom
  integer(kind=i4) :: iu, ios, i, sz, current_level, file_size, inode, n_nodes, k

  integer(i4) :: nmax, d_depth, to_free, free, i_dir
  integer(kind=i4), allocatable :: sizes(:), directories(:), parent(:)
  type(dir_names), allocatable :: dir(:)
  integer(i4), parameter :: total_disk_size = 70000000
  integer(i4), parameter :: storage_required = 30000000
  integer(i4), allocatable :: indices(:)

  ! fname = 'data/test/d7p1.txt'
  fname = 'data/d7p1_input.txt'

  nmax = nrows(fname)
  allocate(dir(nmax))
  allocate(directories(nmax), source=0)
  allocate(sizes(nmax), source=0)
  allocate(parent(nmax), source=0)
  open (newunit=iu, file=trim(fname), action='read', form='formatted')
    i = 0
    do 
      i=i+1
      read(iu, '(a)', iostat=ios, iomsg=iom) buffer

      if (ios /= 0) then 
        ! print*, ios, trim(iom)
        i = i - 1
        exit
      endif 

      call split(buffer, ' ', s1, s2)
      select case (s1)
      case ('$')
        ! This is a command
        call split(s2, ' ', cmd, s3)
        select case (cmd) 
        case ('cd')
          select case (trim(s3))
          case('/')
            ! Root directory, initialize arrays
            n_nodes = 1
            inode = 1

            dir(inode)%parent = ''
            dir(inode)%name = trim(s3)   
            dir(inode)%i_node = inode
            dir(inode)%i_parent = 0
            dir(inode)%depth = 0    

            ! print*, "Entering directory ",trim(s3),". Node ", index_current_node
          case('..')
            ! Update size of the parent of the last visited node
            sizes(dir(inode)%i_parent) = sizes(dir(inode)%i_parent) + sizes(inode)

            ! Update current node.
            inode = dir(inode)%i_parent
            d_depth = dir(inode)%depth
          case default
            ! Enter a new directory. Find the right node index.
            ! n_nodes = n_nodes + 1 ! Add a node 
            ! inode = n_nodes ! Current node is last node
            ! d_depth = d_depth + 1 ! Increment depth
            print*, "------- MAP ----------"
            print*, "Entering ", trim(s3)
            do i = 1, dir(inode)%n_children
              k = dir(inode)%i_children(i)
              if (dir(k)%name == trim(s3)) exit 
            enddo 
            ! print*, dir(inode)%name,';', dir(i)%name, i, trim(s3), n_nodes
            inode = dir(k)%i_node
            d_depth = dir(inode)%depth

            ! if (dir(inode)%name /= trim(s3)) then 
            print*, "Mapped to inode ", inode
            print*, "Name ", dir(inode)%name
            !   error stop
            ! endif 


            ! dir(inode)%parent = dir(inode-1)%name
            ! dir(inode)%name = trim(s3)
            ! dir(inode)%i_node = inode
            ! dir(inode)%i_parent = dir(inode-1)%i_node
            ! dir(inode)%depth = d_depth  
          end select
        case ('ls')
          dir(inode)%n_children = 0
          ! cycle
        end select
      case('dir')
        ! Directories. Add the nodes
        ! Enter a new directory. Add a node.
        n_nodes = n_nodes + 1 ! Add a node 
        ! inode = n_nodes ! Current node is last node
        ! d_depth = d_depth + 1 ! Increment depth

        ! ! Add child to the parent node
        if (.not.allocated(dir(inode)%i_children)) allocate(dir(inode)%i_children(0))
        dir(inode)%n_children = dir(inode)%n_children + 1
        dir(inode)%i_children = [dir(inode)%i_children, n_nodes]

        ! Add node
        dir(n_nodes)%parent = dir(inode)%name
        dir(n_nodes)%name = trim(s2)
        dir(n_nodes)%i_node = n_nodes
        dir(n_nodes)%i_parent = dir(inode)%i_node
        dir(n_nodes)%depth = d_depth + 1 
        ! cycle
        print*, "-------- ADD ----------"
        print*, "Added node ", dir(n_nodes)%name
        print*, "With id", dir(n_nodes)%i_node
        print*, "Parent is", dir(n_nodes)%parent
      case default
        ! Files. First field is the size 
        read(s1, *) file_size
        sizes(inode) = sizes(inode) + file_size
      end select
      ! print*, trim(buffer), parent(n_nodes), directories(n_nodes), dir(n_nodes)%name, sizes(n_nodes)
    enddo 
  close(iu)

  ! i=inode
  ! print*, 'dir: ',dir(i)%name, ', parent: ',dir(i)%parent,', size:', sizes(i) 

  ! print *, d_depth, inode, dir(inode)%name
  do while (d_depth > 0)
    ! Update size of the parent of the last visited node
    sizes(dir(inode)%i_parent) = sizes(dir(inode)%i_parent) + sizes(inode)

    ! Update current node.
    inode = dir(inode)%i_parent
    d_depth = dir(inode)%depth
  enddo

  do i = 1, n_nodes
    
    print*, 'dir: ',dir(i)%name, ', parent: ',dir(i)%parent,', size:', sizes(i) 
    ! if (sizes(i)<=100000) print*, 'dir: ',dir(i)%name, ', parent: ',dir(i)%parent,', size:', sizes(i) 
  enddo
  print*, "Answer day 7 part 1 =", sum(sizes, mask=sizes<=100000)

  ! allocate(indices(size(sizes,1)))
  ! call sort_index(sizes, indices)

  free = total_disk_size - sizes(1)
  to_free = storage_required - free
  ! minloc(sizes(indices),1, mask=sizes(indices) >= to_free)
  i_dir = minloc(sizes,1, mask=sizes >= to_free)
  print*, dir(i_dir)%name, sizes(i_dir), to_free
  print*, "Answer day 7 part 2 =", sizes(i_dir)
end program aoc_day7