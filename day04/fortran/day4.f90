program day4
  use day4m
  implicit none

  integer, allocatable :: grids(:,:,:)
  integer :: n
  integer :: gidx
  integer :: called
  integer, allocatable :: numbers(:)
  logical, allocatable :: complete_boards(:)

  call read_problem(numbers, grids)

  allocate(complete_boards(size(grids, 1)))
  complete_boards(:) = .false.


  n = 1
  do n = 1, size(numbers)
     called = numbers(n)
     print *, "called: ", called
     where (grids == called)
        grids = 0
     end where
     
     do gidx = 1, size(grids, 1)
        if (.not. complete_boards(gidx) .and. iscomplete(grids(gidx,:,:))) then
           print *, "finished board ", gidx, "with score: ", called * sum(grids(gidx,:,:))
           complete_boards(gidx) = .true.
        end if
     end do
  end do

  print *, "done"
end program day4
