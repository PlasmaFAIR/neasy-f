module netfy
  implicit none

  interface netcdf_type
    module procedure netcdf_type_scalar
    module procedure netcdf_type_rank1
  end interface netcdf_type

  interface netfy_write
    module procedure write_any_netcdf_scalar
    module procedure write_any_netcdf_rank1
  end interface netfy_write
  
contains

  function netcdf_type_scalar(variable) result(nf_type)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT, NF90_REAL, NF90_DOUBLE
    integer(kind(NF90_BYTE)) :: nf_type
    class(*), intent(in) :: variable

    select type (variable)
    type is (integer(int8))
      nf_type = NF90_BYTE
    type is (integer(int16))
      nf_type = NF90_SHORT
    type is (integer(int32))
      nf_type = NF90_INT
    type is (real(real32))
      nf_type = NF90_REAL
    type is (real(real64))
      nf_type = NF90_DOUBLE
    type is (character(len=*))
      nf_type = NF90_CHAR
    class default
      nf_type = -1
    end select
  end function netcdf_type_scalar
  
  function netcdf_type_rank1(variable) result(nf_type)
    use netcdf, only : NF90_BYTE
    integer(kind(NF90_BYTE)) :: nf_type
    class(*), dimension(:), intent(in) :: variable
    nf_type = netcdf_type(variable(1))
  end function netcdf_type_rank1

  subroutine write_any_netcdf_scalar(name, parent_id, value_)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, NF90_NOERR, NF90_ENOTVAR, NF90_INT
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), intent(in) :: value_

    integer(kind(NF90_INT)) :: nf_type
    integer :: status
    integer :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    ! Variable doesn't exist, so let's create it
    if (status == NF90_ENOTVAR) then
      nf_type = netcdf_type(value_)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, var_id)
    end if
    ! Something went wrong with one of the previous two calls
    if (status /= NF90_NOERR) then
      call netcdf_error(status, var=name, varid=var_id, &
                        message="(define_and_write_integer)")
    end if

    select type (value_)
    type is (integer(int8))
      status = nf90_put_var(parent_id, var_id, value_)
    type is (integer(int16))
      status = nf90_put_var(parent_id, var_id, value_)
    type is (integer(int32))
      status = nf90_put_var(parent_id, var_id, value_)
    type is (real(real32))
      status = nf90_put_var(parent_id, var_id, value_)
    type is (real(real64))
      status = nf90_put_var(parent_id, var_id, value_)
    type is (character(len=*))
      status = nf90_put_var(parent_id, var_id, value_)
    end select

    if (status /= NF90_NOERR) then
      call netcdf_error(status, parent_id, var=name, varid=var_id, &
                        message="(define_and_write_integer)")
    end if
  end subroutine write_any_netcdf_scalar

  subroutine write_any_netcdf_rank1(name, parent_id, value_, dim_ids)
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, NF90_NOERR, NF90_ENOTVAR, NF90_INT
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), intent(in), dimension(:) :: value_
    !> Array of dimension IDs
    integer, intent(in), dimension(:) :: dim_ids

    integer(kind(NF90_INT)) :: nf_type
    integer :: status
    integer :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    ! Variable doesn't exist, so let's create it
    if (status == NF90_ENOTVAR) then
      nf_type = netcdf_type(value_)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, dim_ids, var_id)
    end if
    ! Something went wrong with one of the previous two calls
    if (status /= NF90_NOERR) then
      call netcdf_error(status, var=name, varid=var_id, &
                        message="(define_and_write_integer)")
    end if

    status = nf90_put_var(parent_id, var_id, value_)
    if (status /= NF90_NOERR) then
      call netcdf_error(status, parent_id, var=name, varid=var_id, &
                        message="(define_and_write_integer)")
    end if
  end subroutine write_any_netcdf_rank1

  subroutine write_dim(name, parent_id, value_, units, unlimited)
    use netcdf, only : nf90_inq_dimid, nf90_def_var, nf90_def_dim, nf90_put_var, NF90_NOERR, NF90_ENOTVAR, NF90_UNLIMITED, NF90_INT
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), intent(in), dimension(:) :: value_
    !> Units of coordinate
    character(len=*), optional, intent(in) :: units
    !> Is this dimension unlimited?
    logical, optional, intent(in) :: unlimited

    integer(kind(NF90_INT)) :: nf_type
    integer :: status
    integer(kind(NF90_INT)) :: dim_id, var_id
    logical :: unlimited_local
    integer :: dim_size

    status = nf90_inq_dimid(parent_id, name, dim_id)
    if (status == NF90_NOERR) return
    if (status /= NF90_ENOTVAR) then
      call netcdf_error(status, dim=name, dimid=dim_id, &
                        message="(define_and_write_integer)")
    end if

    ! Dimension doesn't exist, so let's create it
    if (status == NF90_ENOTVAR) then
      unlimited_local = .false.
      if (present(unlimited)) then
        unlimited_local = unlimited
      end if

      if (unlimited_local) then
        dim_size = NF90_UNLIMITED
      else
        dim_size = size(value_)
      end if
      
      status = nf90_def_dim(parent_id, name, dim_size, dim_id)
      if (status /= NF90_NOERR) then
        call netcdf_error(status, dim=name, dimid=dim_id, &
                          message="(define_and_write_integer)")
      end if

      nf_type = netcdf_type(value_)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, dim_id, var_id)
      if (status /= NF90_NOERR) then
        call netcdf_error(status, var=name, varid=var_id, &
                          message="(define_and_write_integer)")
      end if
    end if

    status = nf90_put_var(parent_id, dim_id, value_)
    if (status /= NF90_NOERR) then
      call netcdf_error(status, parent_id, var=name, varid=dim_id, &
                        message="(define_and_write_integer)")
    end if
  end subroutine write_dim

  !> Convert a netCDF error code to a nice error message. Writes to `stderr`
  !>
  !> All the arguments except `istatus` are optional and are used to give more
  !> detailed information about the error
  subroutine netcdf_error(istatus, ncid, varid, dimid, file, dim, var, att, message)
    use, intrinsic :: iso_fortran_env, only : error_unit
    use netcdf, only: NF90_GLOBAL, nf90_strerror, nf90_inquire_variable, nf90_inquire_dimension, NF90_NOERR
    implicit none
    !> The netCDF error code to convert to a message
    integer, intent (in) :: istatus
    !> netCDF ID of the file
    integer, intent (in), optional :: ncid
    !> netCDF ID of the variable
    integer, intent (in), optional :: varid
    !> netCDF ID of the dimension
    integer, intent (in), optional :: dimid
    !> Name of the file
    character (*), intent (in), optional :: file
    !> Name of the dimension
    character (*), intent (in), optional :: dim
    !> Name of the variable
    character (*), intent (in), optional :: var
    !> Name of the attribute
    character (*), intent (in), optional :: att
    !> Custom text to append to the error message
    character (*), intent (in), optional :: message
    integer :: ist
    character (20) :: varname, dimname

    write (error_unit, '(2a)', advance='no') 'ERROR: ', trim (nf90_strerror (istatus))

    if (present(file)) &
         write (error_unit, '(2a)', advance='no') ' in file ', trim (file)

    if (present(dim)) &
         write (error_unit, '(2a)', advance='no') ' in dimension ', trim (dim)

    if (present(var)) &
         write (error_unit, '(2a)', advance='no') ' in variable ', trim (var)

    if (present(varid)) then
       if (.not. present(ncid)) then
         error stop 'ERROR in netcdf_error: ncid missing while varid present in the argument'
       end if

       if (present(att) ) then
         if (varid == NF90_GLOBAL) then
           write (error_unit, '(2a)') ' in global attribute ', trim(att)
         else
           write (error_unit, '(2a)') ' with the attribute ', trim(att)
         end if
       else
         ist = nf90_inquire_variable (ncid, varid, varname)
         if (ist == NF90_NOERR) then
           write (error_unit, '(a,i8,2a)', advance='no') ' in varid: ', varid, &
                & ' variable name: ', trim (varname)
         else
           write (error_unit, *) ''
           write (error_unit, '(3a,i8,a,i8)', advance='no') 'ERROR in netcdf_error: ', &
                trim (nf90_strerror(ist)), ' in varid: ', varid, &
                ', ncid: ', ncid
         end if
       end if
    end if

    if (present(dimid)) then
       if (.not. present(ncid)) then
         error stop 'ERROR in netcdf_error: ncid missing while dimid present in the argument'
       end if

       ist = nf90_inquire_dimension (ncid, dimid, dimname)
       if (ist == NF90_NOERR) then
         write (error_unit, '(a,i8,2a)', advance='no') ' in dimid: ', dimid, &
              & ' dimension name: ', trim (dimname)
       else
         write (error_unit, *) ''
         write (error_unit, '(3a,i8,a,i8)', advance='no') 'ERROR in netcdf_error: ', &
              trim (nf90_strerror(ist)), ' in dimid: ', dimid, &
              ', ncid: ', ncid
       end if
    end if

    if (present(message)) write (error_unit, '(a)', advance='no') trim(message)

    ! append line-break
    write(error_unit,*)

    error stop "Aborted by netcdf_error"
  end subroutine netcdf_error
end module write_any
