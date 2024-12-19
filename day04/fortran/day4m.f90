module day4m
  Use, intrinsic :: iso_fortran_env, Only : iostat_end  
  implicit none
  private
  public iscomplete, read_problem
contains

  subroutine read_problem(numbers, grids)
    integer, allocatable, intent(out) :: numbers(:)
    integer, allocatable, intent(out) :: grids(:,:,:)
    
    integer :: io
    character(:), allocatable :: data
    integer :: i
    integer :: n
    integer :: number
    integer, allocatable :: temp(:)
    integer, allocatable :: grids_temp(:,:,:)
    integer :: err

    open(newunit=io, file="../../input.txt", status="old", action="read")
    allocate(character(100 * 3) :: data)

    ! go through the first line reading out characters
    read(io, "(A)") data
    i = 1
    number = 1
    allocate(numbers(1))
    do while (data(i:i) /= " ")
       if (data(i:i) == ",") then
          i = i + 1
          number = number + 1
          if (number > size(numbers)) then
             call move_alloc(numbers, temp)
             allocate(numbers(number * 2))
             numbers(:number) = temp
             deallocate(temp)
          end if
          numbers(number) = 0
       else
          read(data(i:i), "(1i1)") n
          numbers(number) = numbers(number) * 10 + n
          i = i + 1
       endif
    end do
    ! trim numbers to length
    call move_alloc(numbers, temp)
    allocate(numbers(number))
    numbers(:number) = temp(:number)
    deallocate(temp)

    ! make data smaller for convenience
    deallocate(data)
    allocate(character(4 * 5) :: data)

    ! read the empty line
    read(io, "(A)") data

    ! read the first line of grids
    ! read(io, "(A)") data
    ! print *, data
    allocate(grids(1,5,5))

    ! read out our grids, one at a time
    i = 1
    gridloop: do while (.true.)
       if (i > size(grids, 1)) then
          call move_alloc(grids, grids_temp)
          allocate(grids(i * 2,5,5))
          grids(:i,:,:) = grids_temp
          deallocate(grids_temp)
       end if
       
       read(io, "(5I3)") grids(i,:,:)
       read(io, "(A)", iostat=err) data
       if (err == iostat_end) then
          exit gridloop
       end if
       i = i + 1
    end do gridloop
    ! trim grids to length
    call move_alloc(grids, grids_temp)
    allocate(grids(i,5,5))
    grids(:i,:,:) = grids_temp(:i,:,:)
    deallocate(grids_temp)
    
    close(io)
  end subroutine read_problem

  logical function iscomplete(grid) result(s)
    integer, intent(in) :: grid(:,:)

    integer :: nr, nc
    integer :: idx

    logical, allocatable :: dones(:)

    nr = size(grid, 1)
    nc = size(grid, 2)

    allocate(dones(nr + nc + 2))

    do idx = 1, nr
       dones(idx) = all(grid(idx,:) == 0)
    end do

    do idx = 1, nc
       dones(nr + idx) = all(grid(:,idx) == 0)
    end do

    dones(nr + nc + 1) = .true.
    dones(nr + nc + 2) = .true.
    do idx = 1, min(nr, nc)
       dones(nr + nc + 1) = dones(nr + nc + 1) .and. (grid(idx,idx) == 0)
       dones(nr + nc + 2) = dones(nr + nc + 2) .and. (grid(6 - idx,idx) == 0)
    end do

    if (any(dones)) then
       s = .TRUE.
    else
       s = .FALSE.
    end if
    
  end function iscomplete
end module day4m

