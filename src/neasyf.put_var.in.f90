  !> A wrapper around `nf90_put_var` to handle runtime and unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_put_var`.
  function polymorphic_put_var_rank{n}(ncid, varid, values, start, count, stride, map) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_put_var, NF90_EBADTYPE
    integer, intent( in) :: ncid, varid
    class(*), dimension({array(n)}), intent(in) :: values
    integer, dimension(:), optional, intent(in) :: start, count, stride, map
    integer(nf_kind) :: status
    select type (values)
    type is (integer(int8))
      status = nf90_put_var(ncid, varid, values, start, count, stride, map)
    type is (integer(int16))
      status = nf90_put_var(ncid, varid, values, start, count, stride, map)
    type is (integer(int32))
      status = nf90_put_var(ncid, varid, values, start, count, stride, map)
    type is (real(real32))
      status = nf90_put_var(ncid, varid, values, start, count, stride, map)
    type is (real(real64))
      status = nf90_put_var(ncid, varid, values, start, count, stride, map)
    type is (character(len=*))
      status = nf90_put_var(ncid, varid, values, start, count, stride, map)
    class default
      status = NF90_EBADTYPE
    end select
  end function polymorphic_put_var_rank{n}
