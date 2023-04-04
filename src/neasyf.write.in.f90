  !> Write a variable to a netCDF file or group, defining it if it isn't already
  !> defined in the dataset.
  !>
  !> Optional arguments "unit" and "long_name" allow you to create attributes
  !> of the same names.
  !>
  !> Exactly one of `dim_ids` or `dim_names` must be present if the variable
  !> doesn't already exist in the file.
  !>
  !> If you pass `dim_names`, then Fortran requires each element be the same
  !> length. If you have dimension names of different lengths, you can simplify
  !> passing this array by doing something like:
  !>
  !>     call neasyf_write(file_id, "var", data, dim_names=&
  !>                       [character(len=len("longer_dim"))::&
  !>                           "short", &
  !>                           "longer_dim" &
  !>                       ])
  !>
  !> which avoids the need to manually pad each dimension name with spaces.
  subroutine neasyf_write_rank{n}(parent_id, name, values, dim_ids, dim_names, &
       varid, units, long_name, start, count, stride, map, compression)
    use, intrinsic :: iso_fortran_env, only : error_unit
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, nf90_put_att, &
         nf90_inq_dimid, NF90_NOERR, NF90_ENOTVAR, NF90_EDIMMETA
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), dimension({array(n)}), intent(in) :: values
    !> Array of dimension IDs
    integer, dimension(:), optional, intent(in) :: dim_ids
    !> Array of dimension names
    character(len=*), dimension(:), optional, intent(in) :: dim_names
    !> If provided, used to return the NetCDF ID of the variable
    integer, optional, intent(out) :: varid
    !> Units of coordinate
    character(len=*), optional, intent(in) :: units
    !> Long descriptive name
    character(len=*), optional, intent(in) :: long_name
    integer, dimension(:), optional, intent(in) :: start, count, stride, map
    !> If non-zero, use compression.
    !>
    !> Enables the `shuffle` netCDF filter and sets the `deflate_level`
    !> parameter to `compression`. You can set the default compression through
    !> [[neasyf_default_compression]]
    integer, optional, intent(in) :: compression

    integer, dimension(:), allocatable :: local_dim_ids
    integer :: dim_index
    integer(nf_kind) :: nf_type
    integer :: status
    integer :: var_id
    integer :: local_compression

    status = nf90_inq_varid(parent_id, name, var_id)
    ! Variable doesn't exist, so let's create it
    if (status == NF90_ENOTVAR) then
      if (.not. (present(dim_ids) .or. present(dim_names))) then
        call neasyf_error(NF90_EDIMMETA, var=name, &
             message="neasyf_write: one of 'dim_ids' or 'dim_names' must be present if variable doesn't already exist")
      end if

      ! Either use the passed in array of dimension IDs, or look them up from the array of names
      if (present(dim_ids)) then
        local_dim_ids = dim_ids
      else
        allocate(local_dim_ids(ubound(dim_names, 1)))
        do dim_index = 1, ubound(dim_names, 1)
          status = nf90_inq_dimid(parent_id, trim(dim_names(dim_index)), local_dim_ids(dim_index))
          call neasyf_error(status, var=name, dim=trim(dim_names(dim_index)))
        end do
      end if

      local_compression = neasyf_default_compression
      if (present(compression)) then
        local_compression = compression
      end if

      nf_type = neasyf_type(values)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, local_dim_ids, var_id, &
           shuffle=(local_compression > 0), deflate_level=local_compression)
      deallocate(local_dim_ids)

      if (present(units)) then
        status = nf90_put_att(parent_id, var_id, "units", units)
        call neasyf_error(status, var=name, varid=var_id, att="units")
      end if

      if (present(long_name)) then
        status = nf90_put_att(parent_id, var_id, "long_name", long_name)
        call neasyf_error(status, var=name, varid=var_id, att="long_name")
      end if
    end if
    ! Something went wrong with one of the previous two calls
    if (status /= NF90_NOERR) then
      call neasyf_error(status, var=name, varid=var_id, &
                        message="defining variable")
    end if

    status = polymorphic_put_var(parent_id, var_id, values, start, count, stride, map)
    call neasyf_error(status, parent_id, var=name, varid=var_id, message="writing variable")

    if (present(varid)) then
      varid = var_id
    end if
  end subroutine neasyf_write_rank{n}
