program example
  use netfy, only : netfy_open, netfy_write, netfy_make_dim
  implicit none

  integer :: file_id, dim_id

  file_id = netfy_open("example.nc", action="w")
  call netfy_dim(file_id, "coord", [1.0, 2.0, 3.0])
  call netfy_write(file_id, "array", [88, 99, 110], dim_name="coord")
  call netfy_close(file_id)
  
end program example
