!> SPDX-License-Identifier: BSD-3-Clause
!> author: Peter Hill
!> ---
!>
!> neasy-f is a short-and-sweet wrapper for netCDF-Fortran. Rather than
!> attempting to be a feature-complete replacement, neasy-f wraps some common
!> functions together and is opinionated about usage. neasy-f can handle all of
!> the basic netCDF data types, both scalar and arrays up to 7D.
!>
!> Example
!> -------
!>
!> ```fortran
!> ncid = neasyf_open("my_file.nc", "w")
!>
!> ! Write some metadata
!> call neasyf_metadata(file_id, "neasy-f example", &
!>                      "0.2.0", auto_date=.true.)
!>
!> ! Create dimensions
!> call neasyf_dim(ncid, "x", unlimited=.true., x_dimid)
!> call neasyf_dim(ncid, "y", dim_size=NY, y_dimid)
!>
!> ! Writing arrays requires the dimensions
!> call neasyf_write(ncid, "data", data_out, [y_dimid, x_dimid], &
!>                   units="Pa", long_name="Pressure")
!>
!> ! Writing string scalars will automatically create a corresponding
!> ! dimension of the correct length as the trimmed string
!> call neasfy_write(ncid, "scalar_text", "Some text as a variable")
!>
!> call neasyf_close(ncid)
!> ```
!>
!> Usage
!> -----
!>
!> There are six main functions/subroutines provided by neasy-f:
!>
!> - Open a file with [[neasyf_open]]
!> - Write a standard set of metadata with [[neasyf_metadata]]
!> - Create dimensions with [[neasyf_dim]]
!> - Create and write variables with [[neasyf_write]]
!> - Read variables with [[neasyf_read]]
!> - Close a file with [[neasyf_close]]
!>
!> These all handle errors automatically, aborting the program if any are
!> detected. If you want to handle errors yourself, drop down to the standard
!> netCDF-Fortran API.
!>
!> There are two more public neasy-f functions that can be useful,
!> even when using the standard netCDF-Fortran API:
!>
!> - Handle errors with [[neasyf_error]]
!> - Get the netCDF type constant with [[neasyf_type]]
!>
!> Developing
!> ----------
!>
!> If you want to develop or make changes to neasy-f, you should do so to the
!> `*.in.f90` files and run the `generate_source.py` script -- or just rebuild
!> the library using CMake.
module neasyf
  use netcdf, only : NF90_INT
  implicit none

  private
  public :: neasyf_open, neasyf_close, neasyf_type, neasyf_dim
  public :: neasyf_write, neasyf_read, neasyf_error, neasyf_metadata

  integer, parameter :: nf_kind = kind(NF90_INT)

  interface neasyf_type
    module procedure neasyf_type_scalar
    module procedure neasyf_type_rank1
    module procedure neasyf_type_rank2
    module procedure neasyf_type_rank3
    module procedure neasyf_type_rank4
    module procedure neasyf_type_rank5
    module procedure neasyf_type_rank6
    module procedure neasyf_type_rank7
  end interface neasyf_type

  interface neasyf_write
    module procedure neasyf_write_scalar
    module procedure neasyf_write_rank1
    module procedure neasyf_write_rank2
    module procedure neasyf_write_rank3
    module procedure neasyf_write_rank4
    module procedure neasyf_write_rank5
    module procedure neasyf_write_rank6
    module procedure neasyf_write_rank7
  end interface neasyf_write

  interface neasyf_read
    module procedure neasyf_read_scalar
    module procedure neasyf_read_rank1
    module procedure neasyf_read_rank2
    module procedure neasyf_read_rank3
    module procedure neasyf_read_rank4
    module procedure neasyf_read_rank5
    module procedure neasyf_read_rank6
    module procedure neasyf_read_rank7
  end interface neasyf_read

  interface polymorphic_put_var
    module procedure polymorphic_put_var_scalar
    module procedure polymorphic_put_var_rank1
    module procedure polymorphic_put_var_rank2
    module procedure polymorphic_put_var_rank3
    module procedure polymorphic_put_var_rank4
    module procedure polymorphic_put_var_rank5
    module procedure polymorphic_put_var_rank6
    module procedure polymorphic_put_var_rank7
  end interface polymorphic_put_var

  interface polymorphic_get_var
    module procedure polymorphic_get_var_scalar
    module procedure polymorphic_get_var_rank1
    module procedure polymorphic_get_var_rank2
    module procedure polymorphic_get_var_rank3
    module procedure polymorphic_get_var_rank4
    module procedure polymorphic_get_var_rank5
    module procedure polymorphic_get_var_rank6
    module procedure polymorphic_get_var_rank7
  end interface polymorphic_get_var

contains

  !> Open a file, possibly creating it if it doesn't exist
  function neasyf_open(filename, action) result(ncid)
    use, intrinsic :: iso_fortran_env, only : error_unit
    use netcdf, only : nf90_open, nf90_create, NF90_NOWRITE, NF90_NETCDF4, NF90_CLOBBER, NF90_WRITE
    !> Name of the file on disk
    character(len=*), intent(in) :: filename
    !> How to open the file. One of:
    !>
    !> - "r": read-only, file must already exist
    !> - "w": write-only, overwriting any existing file
    !> - "rw": read-write, appending an existing file
    !>
    !> @todo Handle 'rw' for files that may or may not already exist
    character(len=*), intent(in) :: action
    integer(nf_kind) :: ncid
    integer :: status

    select case (action)
    case ('r')
      status = nf90_open(filename, NF90_NOWRITE, ncid)
    case ('rw')
      status = nf90_open(filename, ior(NF90_WRITE, NF90_NETCDF4), ncid)
    case ('w')
      status = nf90_create(filename, ior(NF90_CLOBBER, NF90_NETCDF4), ncid)
    case default
      write(error_unit, '(3a)') "ERROR: neasyf: Unsupported action '" // action // "'"
      error stop
    end select
    call neasyf_error(status, file=filename, ncid=ncid)
  end function neasyf_open

  !> Close an open file
  subroutine neasyf_close(ncid)
    use netcdf, only : nf90_close
    integer, intent(in) :: ncid
    call neasyf_error(nf90_close(ncid), ncid)
  end subroutine neasyf_close

  !> Return the current date and time in ISO8601 format:
  !>     YYYY-MM-DDThh:mm:ss.ssssZhh:mm
  function date_iso8601()
    character(:), allocatable :: date_iso8601
    character(8) :: date
    character(10) :: time
    character(5) :: zone
    call date_and_time(date, time, zone)

    date_iso8601 = date(1:4) // "-" // date(5:6) // "-" // date (7:8) &
         // "T" // time(1:2) // ":" // time(3:4) // ":" // time(5:10) &
         // "Z" // zone(1:3) // ":" // zone(4:5)
  end function date_iso8601

  !> Write some standard metadata as global attributes
  !>
  !> These are compatible with both the [netCDF attribute] and [CF
  !> Metadata][cf] conventions. The following metadata are written:
  !>
  !> - title
  !> - software_name
  !> - software_version
  !> - netcdf_version
  !> - date_created
  !> - id
  !>
  !> [netcdf]: https://docs.unidata.ucar.edu/netcdf-c/current/attribute_conventions.html
  !> [cf]: https://cfconventions.org
  subroutine neasyf_metadata(ncid, title, software_name, software_version, date_created, file_id, auto_date)
    use netcdf, only: nf90_put_att, nf90_inq_libvers, NF90_GLOBAL
    !> NetCDF file ID
    integer(nf_kind), intent(in) :: ncid
    !> Description of the file
    character(len=*), optional, intent(in) :: title
    !> Name of the software creating this file
    character(len=*), optional, intent(in) :: software_name
    !> Version of the software
    character(len=*), optional, intent(in) :: software_version
    !> Date and time this file was created. Conflicts with `auto_date`
    character(len=*), optional, intent(in) :: date_created
    !> Software-specific identifier for this file
    character(len=*), optional, intent(in) :: file_id
    !> If true, write 'created' with current time. Conflicts with `created`
    logical, optional, intent(in) :: auto_date

    integer(nf_kind) :: status

    if (present(date_created) .and. present(auto_date)) then
      error stop "neasyf_metadata: Both 'created' and 'auto_date' given; only one may be present"
    end if

    if (present(title)) then
      status = nf90_put_att(ncid, NF90_GLOBAL, "title", title)
      call neasyf_error(status, ncid=ncid, att="title", message="setting global attribute")
    end if

    if (present(software_name)) then
      status = nf90_put_att(ncid, NF90_GLOBAL, "software_name", software_name)
      call neasyf_error(status, ncid=ncid, att="software_name", message="setting global attribute")
    end if

    if (present(software_version)) then
      status = nf90_put_att(ncid, NF90_GLOBAL, "software_version", software_version)
      call neasyf_error(status, ncid=ncid, att="software_version", message="setting global attribute")
    end if

    status = nf90_put_att(ncid, NF90_GLOBAL, "netcdf_version", trim(nf90_inq_libvers()))
    call neasyf_error(status, ncid=ncid, att="netcdf_version", message="setting global attribute")

    if (present(file_id)) then
      status = nf90_put_att(ncid, NF90_GLOBAL, "id", file_id)
      call neasyf_error(status, ncid=ncid, att="id", message="setting global attribute")
    end if

    if (present(date_created)) then
      status = nf90_put_att(ncid, NF90_GLOBAL, "date_created", date_created)
      call neasyf_error(status, ncid=ncid, att="date_created", message="setting global attribute")
    else if (present(auto_date)) then
      if (auto_date) then
        status = nf90_put_att(ncid, NF90_GLOBAL, "date_created", date_iso8601())
        call neasyf_error(status, ncid=ncid, att="date_created", message="setting global attribute")
      end if
    end if

  end subroutine neasyf_metadata

  !> Return the corresponding netCDF type for [[variable]]
  function neasyf_type_scalar(variable) result(nf_type)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT, NF90_REAL, NF90_DOUBLE
    integer(nf_kind) :: nf_type
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
      error stop "neasfy_type: Unknown Fortran type, cannot convert to netCDF type. &
           &Neasy-f cannot handle this Fortran type, please use the standard netCDF API"
    end select
  end function neasyf_type_scalar

  function neasyf_type_rank1(variable) result(nf_type)
    integer(nf_kind) :: nf_type
    class(*), dimension(:), intent(in) :: variable
    nf_type = neasyf_type(variable(1))
  end function neasyf_type_rank1

  function neasyf_type_rank2(variable) result(nf_type)
    integer(nf_kind) :: nf_type
    class(*), dimension(:, :), intent(in) :: variable
    nf_type = neasyf_type(variable(1, 1))
  end function neasyf_type_rank2

  function neasyf_type_rank3(variable) result(nf_type)
    integer(nf_kind) :: nf_type
    class(*), dimension(:, :, :), intent(in) :: variable
    nf_type = neasyf_type(variable(1, 1, 1))
  end function neasyf_type_rank3

  function neasyf_type_rank4(variable) result(nf_type)
    integer(nf_kind) :: nf_type
    class(*), dimension(:, :, :, :), intent(in) :: variable
    nf_type = neasyf_type(variable(1, 1, 1, 1))
  end function neasyf_type_rank4

  function neasyf_type_rank5(variable) result(nf_type)
    integer(nf_kind) :: nf_type
    class(*), dimension(:, :, :, :, :), intent(in) :: variable
    nf_type = neasyf_type(variable(1, 1, 1, 1, 1))
  end function neasyf_type_rank5

  function neasyf_type_rank6(variable) result(nf_type)
    integer(nf_kind) :: nf_type
    class(*), dimension(:, :, :, :, :, :), intent(in) :: variable
    nf_type = neasyf_type(variable(1, 1, 1, 1, 1, 1))
  end function neasyf_type_rank6

  function neasyf_type_rank7(variable) result(nf_type)
    integer(nf_kind) :: nf_type
    class(*), dimension(:, :, :, :, :, :, :), intent(in) :: variable
    nf_type = neasyf_type(variable(1, 1, 1, 1, 1, 1, 1))
  end function neasyf_type_rank7


  function polymorphic_put_var_scalar(ncid, varid, values, start) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_put_var, NF90_EBADTYPE
    integer, intent(in) :: ncid, varid
    class(*), intent(in) :: values
    integer, dimension(:), optional, intent(in) :: start
    integer :: status
    select type (values)
    type is (integer(int8))
      status = nf90_put_var(ncid, varid, values, start)
    type is (integer(int16))
      status = nf90_put_var(ncid, varid, values, start)
    type is (integer(int32))
      status = nf90_put_var(ncid, varid, values, start)
    type is (real(real32))
      status = nf90_put_var(ncid, varid, values, start)
    type is (real(real64))
      status = nf90_put_var(ncid, varid, values, start)
    type is (character(len=*))
      status = nf90_put_var(ncid, varid, values, start)
    class default
      status = NF90_EBADTYPE
    end select
  end function polymorphic_put_var_scalar

  !> A wrapper around `nf90_put_var` to handle runtime and unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_put_var`.
  function polymorphic_put_var_rank1(ncid, varid, values, start, count, stride, map) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_put_var, NF90_EBADTYPE
    integer, intent( in) :: ncid, varid
    class(*), dimension(:), intent(in) :: values
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
  end function polymorphic_put_var_rank1

  !> A wrapper around `nf90_put_var` to handle runtime and unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_put_var`.
  function polymorphic_put_var_rank2(ncid, varid, values, start, count, stride, map) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_put_var, NF90_EBADTYPE
    integer, intent( in) :: ncid, varid
    class(*), dimension(:, :), intent(in) :: values
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
  end function polymorphic_put_var_rank2

  !> A wrapper around `nf90_put_var` to handle runtime and unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_put_var`.
  function polymorphic_put_var_rank3(ncid, varid, values, start, count, stride, map) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_put_var, NF90_EBADTYPE
    integer, intent( in) :: ncid, varid
    class(*), dimension(:, :, :), intent(in) :: values
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
  end function polymorphic_put_var_rank3

  !> A wrapper around `nf90_put_var` to handle runtime and unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_put_var`.
  function polymorphic_put_var_rank4(ncid, varid, values, start, count, stride, map) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_put_var, NF90_EBADTYPE
    integer, intent( in) :: ncid, varid
    class(*), dimension(:, :, :, :), intent(in) :: values
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
  end function polymorphic_put_var_rank4

  !> A wrapper around `nf90_put_var` to handle runtime and unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_put_var`.
  function polymorphic_put_var_rank5(ncid, varid, values, start, count, stride, map) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_put_var, NF90_EBADTYPE
    integer, intent( in) :: ncid, varid
    class(*), dimension(:, :, :, :, :), intent(in) :: values
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
  end function polymorphic_put_var_rank5

  !> A wrapper around `nf90_put_var` to handle runtime and unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_put_var`.
  function polymorphic_put_var_rank6(ncid, varid, values, start, count, stride, map) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_put_var, NF90_EBADTYPE
    integer, intent( in) :: ncid, varid
    class(*), dimension(:, :, :, :, :, :), intent(in) :: values
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
  end function polymorphic_put_var_rank6

  !> A wrapper around `nf90_put_var` to handle runtime and unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_put_var`.
  function polymorphic_put_var_rank7(ncid, varid, values, start, count, stride, map) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_put_var, NF90_EBADTYPE
    integer, intent( in) :: ncid, varid
    class(*), dimension(:, :, :, :, :, :, :), intent(in) :: values
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
  end function polymorphic_put_var_rank7


  function polymorphic_get_var_scalar(ncid, varid, values) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_get_var, NF90_EBADTYPE
    integer, intent(in) :: ncid, varid
    class(*), intent(out) :: values
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
  end function polymorphic_get_var_scalar

  !> Wrapper around `nf90_get_var` to enable unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_get_var`.
  function polymorphic_get_var_rank1(ncid, varid, values) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_get_var, NF90_EBADTYPE
    integer, intent(in) :: ncid, varid
    class(*), dimension(:), intent(out) :: values
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
  end function polymorphic_get_var_rank1

  !> Wrapper around `nf90_get_var` to enable unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_get_var`.
  function polymorphic_get_var_rank2(ncid, varid, values) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_get_var, NF90_EBADTYPE
    integer, intent(in) :: ncid, varid
    class(*), dimension(:, :), intent(out) :: values
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
  end function polymorphic_get_var_rank2

  !> Wrapper around `nf90_get_var` to enable unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_get_var`.
  function polymorphic_get_var_rank3(ncid, varid, values) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_get_var, NF90_EBADTYPE
    integer, intent(in) :: ncid, varid
    class(*), dimension(:, :, :), intent(out) :: values
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
  end function polymorphic_get_var_rank3

  !> Wrapper around `nf90_get_var` to enable unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_get_var`.
  function polymorphic_get_var_rank4(ncid, varid, values) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_get_var, NF90_EBADTYPE
    integer, intent(in) :: ncid, varid
    class(*), dimension(:, :, :, :), intent(out) :: values
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
  end function polymorphic_get_var_rank4

  !> Wrapper around `nf90_get_var` to enable unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_get_var`.
  function polymorphic_get_var_rank5(ncid, varid, values) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_get_var, NF90_EBADTYPE
    integer, intent(in) :: ncid, varid
    class(*), dimension(:, :, :, :, :), intent(out) :: values
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
  end function polymorphic_get_var_rank5

  !> Wrapper around `nf90_get_var` to enable unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_get_var`.
  function polymorphic_get_var_rank6(ncid, varid, values) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_get_var, NF90_EBADTYPE
    integer, intent(in) :: ncid, varid
    class(*), dimension(:, :, :, :, :, :), intent(out) :: values
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
  end function polymorphic_get_var_rank6

  !> Wrapper around `nf90_get_var` to enable unlimited polymorphism.
  !> All arguments have the same meanings as `nf90_get_var`.
  function polymorphic_get_var_rank7(ncid, varid, values) result(status)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_get_var, NF90_EBADTYPE
    integer, intent(in) :: ncid, varid
    class(*), dimension(:, :, :, :, :, :, :), intent(out) :: values
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
  end function polymorphic_get_var_rank7


  !> Create a dimension if it doesn't already exist.
  !>
  !> If the dimension doesn't exist, also create a variable of the same name and
  !> fill it with [[values]], or the integers in the range `1..dim_size`. The
  !> optional argument [[unlimited]] can be used to make this dimension
  !> unlimited in extent.
  !>
  !> Optional arguments "unit" and "long_name" allow you to create attributes
  !> of the same names.
  !>
  !> The netCDF IDs of the dimension and corresponding variable can be returned
  !> through [[dimid]] and [[varid]] respectively.
  subroutine neasyf_dim(parent_id, name, values, dim_size, dimid, varid, units, long_name, unlimited)

    use netcdf, only : nf90_inq_dimid, nf90_inq_varid, nf90_def_var, nf90_def_dim, nf90_put_var, nf90_put_att, &
         NF90_NOERR, NF90_EBADDIM, NF90_ENOTVAR, NF90_UNLIMITED
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Coordinate values
    class(*), dimension(:), optional, intent(in) :: values
    !> Size of the dimension if values isn't specified
    integer, optional, intent(in) :: dim_size
    !> NetCDF ID of the dimension
    integer, optional, intent(out) :: dimid
    !> NetCDF ID of the corresponding variable
    integer, optional, intent(out) :: varid
    !> Units of coordinate
    character(len=*), optional, intent(in) :: units
    !> Long descriptive name of coordinate
    character(len=*), optional, intent(in) :: long_name
    !> Is this dimension unlimited?
    logical, optional, intent(in) :: unlimited

    integer :: status
    integer(nf_kind) :: dim_id, var_id
    integer :: i
    integer :: local_size
    integer, dimension(:), allocatable :: local_values
    logical :: local_unlimited

    if (present(values) .and. present(dim_size)) then
      error stop "neasyf_dim: Both 'values' and 'dim_size' given. Only one must be present"
    end if

    status = nf90_inq_dimid(parent_id, name, dim_id)
    if (status == NF90_NOERR) then
      if (present(dimid)) then
        dimid = dim_id
      end if

      if (present(varid)) then
        call neasyf_error(nf90_inq_varid(parent_id, name, var_id), var=name, &
                          message="retrieving ID of existing dimension variable")
        varid = var_id
      end if

      return
    end if

    if (status /= NF90_EBADDIM) then
      call neasyf_error(status, ncid=parent_id, dim=name, dimid=dim_id)
    end if

    if (.not. (present(values) .or. present(dim_size) .or. present(unlimited))) then
      error stop "neasyf_dim: Dimension does not exist and none of 'values', 'dim_size', or 'unlimited' given. &
           &Exactly one must be present"
    end if

    ! TODO: Check existing size is compatible with current arguments

    local_size = -1

    ! Dimension doesn't exist, so let's create it. First we need to get the
    ! initial size of the dimension
    if (present(values)) then
      local_size = size(values)
    else if (present(dim_size)) then
      local_size = dim_size
      local_values = [(i, i=1, dim_size)]
    end if

    ! Setting the dimension to be unlimited overrides the size
    if (present(unlimited)) then
      local_unlimited = unlimited
      if (unlimited) then
        local_size = NF90_UNLIMITED
      end if
    end if

    if (local_size < 0) then
      error stop "neasyf_dim: Dimension does not exist, and initial size not set. &
           &Either pass one of 'values' or 'dim_size', or set 'unlimited=.true.'"
    end if

    if (local_size == 0 .and. .not. local_unlimited) then
      error stop "neasyf_dim: Dimension does not exist, and initial size is 0. &
           &If you're trying to create an unlimited dimension, pass 'unlimited=.true.' instead"
    end if

    status = nf90_def_dim(parent_id, name, local_size, dim_id)
    call neasyf_error(status, dim=name, dimid=dim_id, message="creating dimension")

    if (present(dimid)) then
      dimid = dim_id
    end if

    ! For unlimited dimensions, if no initial size or values provided, we're done
    if (local_unlimited .and. .not. (present(values) .or. present(dim_size))) then
      if (present(varid)) then
        error stop "neasyf_dim: Deferring variable creation, but 'varid' passed and would be given an invalid value. &
             &Please remove 'varid' or pass one of 'values' or 'dim_size'"
      end if
      return
    end if

    ! We could avoid the duplicated call here by copying the values into an allocatable class(*)
    if (present(values)) then
      call neasyf_write(parent_id, name, values, dim_ids=[dim_id], units=units, long_name=long_name, varid=var_id)
    else
      call neasyf_write(parent_id, name, local_values, dim_ids=[dim_id], units=units, long_name=long_name, varid=var_id)
    end if

    if (present(varid)) then
      varid = var_id
    end if
  end subroutine neasyf_dim

  subroutine neasyf_write_scalar(parent_id, name, values, dim_ids, dim_names, units, long_name, start)
    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, real64
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, nf90_put_att, &
         NF90_NOERR, NF90_ENOTVAR, NF90_EDIMMETA, nf90_def_dim, NF90_CHAR, nf90_inq_dimid
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), intent(in) :: values
    !> Array of dimension IDs
    integer, dimension(:), optional, intent(in) :: dim_ids
    !> Array of dimension names
    character(len=*), dimension(:), optional, intent(in) :: dim_names
    !> Units of coordinate
    character(len=*), optional, intent(in) :: units
    !> Long descriptive name
    character(len=*), optional, intent(in) :: long_name
    !> Insertion index (one-based). Note that this is an array with one element!
    integer, dimension(1), optional, intent(in) :: start

    integer(nf_kind) :: nf_type
    integer :: status
    integer :: var_id, dim_id, dim_index
    integer, dimension(:), allocatable :: local_dim_ids

    status = nf90_inq_varid(parent_id, name, var_id)
    ! Variable doesn't exist, so let's create it
    if (status == NF90_ENOTVAR) then
      select type(values)
      type is (character(len=*))
        status = nf90_def_dim(parent_id, name // "_dim", len_trim(values), dim_id)
        call neasyf_error(status, var=name, message="defining string dimension")

        status = nf90_def_var(parent_id, name, NF90_CHAR, [dim_id], var_id)
      class default
        ! TODO: check if nf_type indicates a derived type
        nf_type = neasyf_type(values)

        if (present(start)) then
          ! If we've been passed `start`, then we must be writing into
          ! a 1D array, in which case we need to know the associated
          ! dimension in order to create the variable
          if (.not. (present(dim_ids) .or. present(dim_names))) then
            call neasyf_error(NF90_EDIMMETA, var=name, &
                 message="neasyf_write: one of 'dim_ids' or 'dim_names' must be present if 1D variable doesn't already exist")
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
          ! Create an N-D variable, although we're only going to write a single value
          status = nf90_def_var(parent_id, name, nf_type, local_dim_ids, var_id)
        else
          ! Create a scalar variable
          status = nf90_def_var(parent_id, name, nf_type, var_id)
        end if
      end select
      call neasyf_error(status, var=name, varid=var_id, message="defining variable")

      if (present(units)) then
        status = nf90_put_att(parent_id, var_id, "units", units)
        call neasyf_error(status, var=name, varid=var_id, att="units")
      end if

      if (present(long_name)) then
        status = nf90_put_att(parent_id, var_id, "long_name", long_name)
        call neasyf_error(status, var=name, varid=var_id, att="long_name")
      end if
    else
      call neasyf_error(status, var=name, varid=var_id)
    end if

    status = polymorphic_put_var(parent_id, var_id, values, start=start)
    call neasyf_error(status, parent_id, var=name, varid=var_id, message="writing variable")
  end subroutine neasyf_write_scalar

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
  subroutine neasyf_write_rank1(parent_id, name, values, dim_ids, dim_names, &
       varid, units, long_name, start, count, stride, map)
    use, intrinsic :: iso_fortran_env, only : error_unit
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, nf90_put_att, &
         nf90_inq_dimid, NF90_NOERR, NF90_ENOTVAR, NF90_EDIMMETA
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), dimension(:), intent(in) :: values
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

    integer, dimension(:), allocatable :: local_dim_ids
    integer :: dim_index
    integer(nf_kind) :: nf_type
    integer :: status
    integer :: var_id

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

      nf_type = neasyf_type(values)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, local_dim_ids, var_id)
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
  end subroutine neasyf_write_rank1

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
  subroutine neasyf_write_rank2(parent_id, name, values, dim_ids, dim_names, &
       varid, units, long_name, start, count, stride, map)
    use, intrinsic :: iso_fortran_env, only : error_unit
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, nf90_put_att, &
         nf90_inq_dimid, NF90_NOERR, NF90_ENOTVAR, NF90_EDIMMETA
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), dimension(:, :), intent(in) :: values
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

    integer, dimension(:), allocatable :: local_dim_ids
    integer :: dim_index
    integer(nf_kind) :: nf_type
    integer :: status
    integer :: var_id

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

      nf_type = neasyf_type(values)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, local_dim_ids, var_id)
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
  end subroutine neasyf_write_rank2

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
  subroutine neasyf_write_rank3(parent_id, name, values, dim_ids, dim_names, &
       varid, units, long_name, start, count, stride, map)
    use, intrinsic :: iso_fortran_env, only : error_unit
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, nf90_put_att, &
         nf90_inq_dimid, NF90_NOERR, NF90_ENOTVAR, NF90_EDIMMETA
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), dimension(:, :, :), intent(in) :: values
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

    integer, dimension(:), allocatable :: local_dim_ids
    integer :: dim_index
    integer(nf_kind) :: nf_type
    integer :: status
    integer :: var_id

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

      nf_type = neasyf_type(values)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, local_dim_ids, var_id)
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
  end subroutine neasyf_write_rank3

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
  subroutine neasyf_write_rank4(parent_id, name, values, dim_ids, dim_names, &
       varid, units, long_name, start, count, stride, map)
    use, intrinsic :: iso_fortran_env, only : error_unit
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, nf90_put_att, &
         nf90_inq_dimid, NF90_NOERR, NF90_ENOTVAR, NF90_EDIMMETA
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), dimension(:, :, :, :), intent(in) :: values
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

    integer, dimension(:), allocatable :: local_dim_ids
    integer :: dim_index
    integer(nf_kind) :: nf_type
    integer :: status
    integer :: var_id

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

      nf_type = neasyf_type(values)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, local_dim_ids, var_id)
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
  end subroutine neasyf_write_rank4

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
  subroutine neasyf_write_rank5(parent_id, name, values, dim_ids, dim_names, &
       varid, units, long_name, start, count, stride, map)
    use, intrinsic :: iso_fortran_env, only : error_unit
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, nf90_put_att, &
         nf90_inq_dimid, NF90_NOERR, NF90_ENOTVAR, NF90_EDIMMETA
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), dimension(:, :, :, :, :), intent(in) :: values
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

    integer, dimension(:), allocatable :: local_dim_ids
    integer :: dim_index
    integer(nf_kind) :: nf_type
    integer :: status
    integer :: var_id

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

      nf_type = neasyf_type(values)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, local_dim_ids, var_id)
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
  end subroutine neasyf_write_rank5

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
  subroutine neasyf_write_rank6(parent_id, name, values, dim_ids, dim_names, &
       varid, units, long_name, start, count, stride, map)
    use, intrinsic :: iso_fortran_env, only : error_unit
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, nf90_put_att, &
         nf90_inq_dimid, NF90_NOERR, NF90_ENOTVAR, NF90_EDIMMETA
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), dimension(:, :, :, :, :, :), intent(in) :: values
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

    integer, dimension(:), allocatable :: local_dim_ids
    integer :: dim_index
    integer(nf_kind) :: nf_type
    integer :: status
    integer :: var_id

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

      nf_type = neasyf_type(values)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, local_dim_ids, var_id)
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
  end subroutine neasyf_write_rank6

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
  subroutine neasyf_write_rank7(parent_id, name, values, dim_ids, dim_names, &
       varid, units, long_name, start, count, stride, map)
    use, intrinsic :: iso_fortran_env, only : error_unit
    use netcdf, only : nf90_inq_varid, nf90_def_var, nf90_put_var, nf90_put_att, &
         nf90_inq_dimid, NF90_NOERR, NF90_ENOTVAR, NF90_EDIMMETA
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> NetCDF ID of the parent group/file
    integer, intent(in) :: parent_id
    !> Value of the integer to write
    class(*), dimension(:, :, :, :, :, :, :), intent(in) :: values
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

    integer, dimension(:), allocatable :: local_dim_ids
    integer :: dim_index
    integer(nf_kind) :: nf_type
    integer :: status
    integer :: var_id

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

      nf_type = neasyf_type(values)
      ! TODO: check if nf_type indicates a derived type
      status = nf90_def_var(parent_id, name, nf_type, local_dim_ids, var_id)
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
  end subroutine neasyf_write_rank7


  subroutine neasyf_read_scalar(parent_id, name, values)
    use netcdf, only : nf90_max_name, nf90_inq_varid, nf90_inquire_variable
    !> NetCDF ID of the parent file or group
    integer, intent(in) :: parent_id
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> Storage for the variable
    class(*), intent(out) :: values

    integer :: status
    integer(nf_kind) :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    call neasyf_error(status, ncid=parent_id, var=name, varid=var_id, &
                      message="finding variable")

    status = polymorphic_get_var(parent_id, var_id, values)

    call neasyf_error(status, parent_id, var=name, varid=var_id, &
                      message="reading variable")
  end subroutine neasyf_read_scalar

  !> Wrapper around `nf90_get_var` that uses the variable name instead of ID
  subroutine neasyf_read_rank1(parent_id, name, values)
    use netcdf, only : nf90_max_name, nf90_inq_varid, nf90_inquire_variable
    !> NetCDF ID of the parent file or group
    integer, intent(in) :: parent_id
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> Storage for the variable
    class(*), dimension(:), intent(out) :: values

    integer :: status
    integer(nf_kind) :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    call neasyf_error(status, ncid=parent_id, var=name, varid=var_id, &
                      message="finding variable")

    status = polymorphic_get_var(parent_id, var_id, values)

    call neasyf_error(status, parent_id, var=name, varid=var_id, &
                      message="reading variable")
  end subroutine neasyf_read_rank1

  !> Wrapper around `nf90_get_var` that uses the variable name instead of ID
  subroutine neasyf_read_rank2(parent_id, name, values)
    use netcdf, only : nf90_max_name, nf90_inq_varid, nf90_inquire_variable
    !> NetCDF ID of the parent file or group
    integer, intent(in) :: parent_id
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> Storage for the variable
    class(*), dimension(:, :), intent(out) :: values

    integer :: status
    integer(nf_kind) :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    call neasyf_error(status, ncid=parent_id, var=name, varid=var_id, &
                      message="finding variable")

    status = polymorphic_get_var(parent_id, var_id, values)

    call neasyf_error(status, parent_id, var=name, varid=var_id, &
                      message="reading variable")
  end subroutine neasyf_read_rank2

  !> Wrapper around `nf90_get_var` that uses the variable name instead of ID
  subroutine neasyf_read_rank3(parent_id, name, values)
    use netcdf, only : nf90_max_name, nf90_inq_varid, nf90_inquire_variable
    !> NetCDF ID of the parent file or group
    integer, intent(in) :: parent_id
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> Storage for the variable
    class(*), dimension(:, :, :), intent(out) :: values

    integer :: status
    integer(nf_kind) :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    call neasyf_error(status, ncid=parent_id, var=name, varid=var_id, &
                      message="finding variable")

    status = polymorphic_get_var(parent_id, var_id, values)

    call neasyf_error(status, parent_id, var=name, varid=var_id, &
                      message="reading variable")
  end subroutine neasyf_read_rank3

  !> Wrapper around `nf90_get_var` that uses the variable name instead of ID
  subroutine neasyf_read_rank4(parent_id, name, values)
    use netcdf, only : nf90_max_name, nf90_inq_varid, nf90_inquire_variable
    !> NetCDF ID of the parent file or group
    integer, intent(in) :: parent_id
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> Storage for the variable
    class(*), dimension(:, :, :, :), intent(out) :: values

    integer :: status
    integer(nf_kind) :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    call neasyf_error(status, ncid=parent_id, var=name, varid=var_id, &
                      message="finding variable")

    status = polymorphic_get_var(parent_id, var_id, values)

    call neasyf_error(status, parent_id, var=name, varid=var_id, &
                      message="reading variable")
  end subroutine neasyf_read_rank4

  !> Wrapper around `nf90_get_var` that uses the variable name instead of ID
  subroutine neasyf_read_rank5(parent_id, name, values)
    use netcdf, only : nf90_max_name, nf90_inq_varid, nf90_inquire_variable
    !> NetCDF ID of the parent file or group
    integer, intent(in) :: parent_id
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> Storage for the variable
    class(*), dimension(:, :, :, :, :), intent(out) :: values

    integer :: status
    integer(nf_kind) :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    call neasyf_error(status, ncid=parent_id, var=name, varid=var_id, &
                      message="finding variable")

    status = polymorphic_get_var(parent_id, var_id, values)

    call neasyf_error(status, parent_id, var=name, varid=var_id, &
                      message="reading variable")
  end subroutine neasyf_read_rank5

  !> Wrapper around `nf90_get_var` that uses the variable name instead of ID
  subroutine neasyf_read_rank6(parent_id, name, values)
    use netcdf, only : nf90_max_name, nf90_inq_varid, nf90_inquire_variable
    !> NetCDF ID of the parent file or group
    integer, intent(in) :: parent_id
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> Storage for the variable
    class(*), dimension(:, :, :, :, :, :), intent(out) :: values

    integer :: status
    integer(nf_kind) :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    call neasyf_error(status, ncid=parent_id, var=name, varid=var_id, &
                      message="finding variable")

    status = polymorphic_get_var(parent_id, var_id, values)

    call neasyf_error(status, parent_id, var=name, varid=var_id, &
                      message="reading variable")
  end subroutine neasyf_read_rank6

  !> Wrapper around `nf90_get_var` that uses the variable name instead of ID
  subroutine neasyf_read_rank7(parent_id, name, values)
    use netcdf, only : nf90_max_name, nf90_inq_varid, nf90_inquire_variable
    !> NetCDF ID of the parent file or group
    integer, intent(in) :: parent_id
    !> Name of the variable
    character(len=*), intent(in) :: name
    !> Storage for the variable
    class(*), dimension(:, :, :, :, :, :, :), intent(out) :: values

    integer :: status
    integer(nf_kind) :: var_id

    status = nf90_inq_varid(parent_id, name, var_id)
    call neasyf_error(status, ncid=parent_id, var=name, varid=var_id, &
                      message="finding variable")

    status = polymorphic_get_var(parent_id, var_id, values)

    call neasyf_error(status, parent_id, var=name, varid=var_id, &
                      message="reading variable")
  end subroutine neasyf_read_rank7


  !> Convert a netCDF error code to a nice error message. Writes to `stderr`
  !>
  !> All the arguments except `istatus` are optional and are used to give more
  !> detailed information about the error
  subroutine neasyf_error(istatus, ncid, varid, dimid, file, dim, var, att, message)
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

    if (istatus == NF90_NOERR) return

    write (error_unit, '(2a)', advance='no') 'ERROR: ', trim (nf90_strerror (istatus))

    if (present(file)) &
         write (error_unit, '(3a)', advance='no') ' in file "', trim (file), '"'

    if (present(dim)) &
         write (error_unit, '(3a)', advance='no') ' in dimension "', trim (dim), '"'

    if (present(var)) &
         write (error_unit, '(3a)', advance='no') ' in variable "', trim (var), '"'

    if (present(varid)) then
       if (.not. present(ncid)) then
         if (present(file) .or. present(dim) .or. present(var)) write (error_unit, *)
         error stop 'neasyf_error: ncid missing while varid present in the argument'
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
           write (error_unit, '(3a,i8,a,i8)', advance='no') 'ERROR in neasyf_error: ', &
                trim (nf90_strerror(ist)), ' in varid: ', varid, &
                ', ncid: ', ncid
         end if
       end if
    end if

    if (present(dimid)) then
       if (.not. present(ncid)) then
         error stop 'ERROR in neasyf_error: ncid missing while dimid present in the argument'
       end if

       ist = nf90_inquire_dimension (ncid, dimid, dimname)
       if (ist == NF90_NOERR) then
         write (error_unit, '(a,i8,2a)', advance='no') ' in dimid: ', dimid, &
              & ' dimension name: ', trim (dimname)
       else
         write (error_unit, *) ''
         write (error_unit, '(3a,i8,a,i8)', advance='no') 'ERROR in neasyf_error: ', &
              trim (nf90_strerror(ist)), ' in dimid: ', dimid, &
              ', ncid: ', ncid
       end if
    end if

    if (present(message)) write (error_unit, '(a)', advance='no') " " // trim(message)

    ! append line-break
    write(error_unit,*)

    error stop "Aborted by neasyf_error"
  end subroutine neasyf_error
end module neasyf
