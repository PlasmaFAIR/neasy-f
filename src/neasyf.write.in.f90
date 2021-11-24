  !> Write a variable to a netCDF file or group, defining it if it isn't already
  !> defined in the dataset.
  !>
  !> Optional arguments "unit" and "long_name" allow you to create attributes
  !> of the same names.
  subroutine neasyf_write_rank{n}(parent_id, name, values, dim_ids, varid, &
       units, long_name, start, count, stride, map)
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, nf90_put_att, &
         NF90_NOERR, NF90_ENOTVAR
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), dimension({array(n)}), intent(in) :: values
    !> Array of dimension IDs
    integer, dimension({n}), intent(in) :: dim_ids
    !> If provided, used to return the NetCDF ID of the variable
    integer, optional, intent(out) :: varid
    !> Units of coordinate
    character(len=*), optional, intent(in) :: units
    !> Long descriptive name
    character(len=*), optional, intent(in) :: long_name
    integer, dimension({n}), optional, intent(in) :: start, count, stride, map

    integer(nf_kind) :: nf_type
    integer :: status
    integer :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    ! Variable doesn't exist, so let's create it
    if (status == NF90_ENOTVAR) then
      nf_type = neasyf_type(values)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, dim_ids, var_id)

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
