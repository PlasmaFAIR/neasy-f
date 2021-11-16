  !> Wrapper around `nf90_get_var` that uses the variable name instead of ID
  subroutine neasyf_read_rank{n}(parent_id, var_name, values)
    use netcdf, only : nf90_max_name, nf90_inq_varid, nf90_inquire_variable
    !> NetCDF ID of the parent file or group
    integer, intent(in) :: parent_id
    !> Name of the variable
    character(len=*), intent(in) :: var_name
    !> Storage for the variable
    class(*), dimension({array(n)}), intent(out) :: values

    integer :: status
    integer(nf_kind) :: file_var_id
    character(len=nf90_max_name) :: file_var_name

    status = nf90_inq_varid(parent_id, var_name, file_var_id)
    call neasyf_error(status, ncid=parent_id)

    status = polymorphic_get_var(parent_id, file_var_id, values)

    call neasyf_error(status, parent_id, varid=file_var_id, var=var_name)
  end subroutine neasyf_read_rank{n}
