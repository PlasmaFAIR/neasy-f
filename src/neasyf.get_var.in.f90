  function polymorphic_get_var_rank{n}(ncid, varid, values) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_get_var, NF90_EBADTYPE
    integer, intent(in) :: ncid, varid
    class(*), dimension({array(n)}), intent(out) :: values
    integer(nf_kind) :: status
    select type (values)
    type is (integer(int8))
      status = nf90_get_var(ncid, varid, values)
    type is (integer(int16))
      status = nf90_get_var(ncid, varid, values)
    type is (integer(int32))
      status = nf90_get_var(ncid, varid, values)
    type is (real(real32))
      status = nf90_get_var(ncid, varid, values)
    type is (real(real64))
      status = nf90_get_var(ncid, varid, values)
    type is (character(len=*))
      status = nf90_get_var(ncid, varid, values)
    class default
      status = NF90_EBADTYPE
    end select
  end function polymorphic_get_var_rank{n}
