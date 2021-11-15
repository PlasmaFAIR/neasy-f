program example
  use neasyf, only : neasyf_open, neasyf_write, neasyf_dim, neasyf_close
  implicit none

  integer :: file_id, dim_id

  file_id = neasyf_open("example.nc", action="w")
  call neasyf_dim(file_id, "coord", [1.0, 2.0, 3.0], units="m", dimid=dim_id)
  call neasyf_write(file_id, "array", [88, 99, 110], dim_ids=[dim_id])
  call neasyf_close(file_id)
  
end program example
