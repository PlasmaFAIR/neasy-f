program example
  use neasyf
  implicit none

  integer :: file_id, dim_id
  real, dimension(3) :: coord_array, data_array

  file_id = neasyf_open("example.nc", action="w")
  call neasyf_dim(file_id, "coord", [1.0, 2.0, 3.0], units="m", dimid=dim_id)
  call neasyf_write(file_id, "array", [88, 99, 110], dim_ids=[dim_id])
  call neasyf_close(file_id)

  file_id = neasyf_open("example.nc", action="r")
  call neasyf_read(file_id, "coord", coord_array)
  call neasyf_read(file_id, "array", data_array)
  call neasyf_close(file_id)

  block
    integer :: i
    do i = 1, 3
      if (abs(coord_array(i) - real(i)) > 1e-8) then
        print*, coord_array
        error stop "Coordinate array different"
      end if
    end do

    if (any(data_array /= [88, 99, 110])) then
      print*, data_array
      error stop "Data array different"
    end if
  end block

end program example
