  !> Wrapper around `nf90_get_var` that uses the variable name instead of ID
  subroutine neasyf_read_rank{n}(parent_id, name, values)
    use netcdf, only : nf90_inq_varid, nf90_inquire_variable
    !> NetCDF ID of the parent file or group
    integer, intent(in) :: parent_id
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> Storage for the variable
    class(*), dimension({array(n)}), intent(out) :: values

    integer :: status
    integer(nf_kind) :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    call neasyf_error(status, ncid=parent_id, var=name, varid=var_id, &
                      message="finding variable")

    status = polymorphic_get_var(parent_id, var_id, values)

    call neasyf_error(status, parent_id, var=name, varid=var_id, &
                      message="reading variable")
  end subroutine neasyf_read_rank{n}
