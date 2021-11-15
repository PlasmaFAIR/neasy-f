neasy-f
=======

neasy-f is a short-and-sweet wrapper for netCDF-Fortran. Rather than attempting
to be a feature-complete replacement, neasy-f wraps some common functions
together and is opinionated about usage.

For example, here's how you might write some two-dimensional data with plain
netCDF:

```fortran
  call check( nf90_create("my_file.nc", ior(nf90_clobber, nf90_netcdf4), ncid) )

  call check( nf90_def_dim(ncid, "x", NX, x_dimid) )
  call check( nf90_def_dim(ncid, "y", NY, y_dimid) )

  call check( nf90_def_var(ncid, "data", NF90_INT, [y_dimid, x_dimid], varid)
  call check( nf90_put_att(ncid, varid, "units", "Pa") )
  call check( nf90_put_att(ncid, varid, "description", "Synthetic pressure") )

  call check( nf90_enddef(ncid) )

  call check( nf90_put_var(ncid, varid, data_out) )

  call check( nf90_close(ncid) )
```

where `check` is a subroutine you must write yourself. Here's the same example,
but using neasy-f:

```fortran
  ncid = neasyf_open("my_file.nc", "w")

  call neasyf_dim(ncid, "x", [(x, x=1, NX)], x_dimid)
  call neasyf_dim(ncid, "y", [(x, x=1, NY)], y_dimid)

  call neasyf_write(ncid, "data", data_out, [y_dimid, x_dimid], &
       units="Pa", description="Synthetic pressure")

  call neasyf_close(ncid)
```

There's a few differences for this simple example:

- neasy-f handles checking the errors internally, and calls `error stop`
  immediately
- neasy-f always uses the netCDF-4 file-type, so that we don't need to call
  `nf90_enddef`
- ...which also means we don't need to separate the definition and writing
  stages
- ...and which also means we can infer the netCDF type from the Fortran type
- neasy-f handles opening files in "read", "write", and "read-write" modes in
  the same call, so we don't need to switch between `nf90_create` and
  `nf90_open` calls
- neasy-f always creates corresponding variables for the dimensions


neasy-f is designed to be "mostly drop-in", meaning you can use it to entirely
replace your use of "plain" netCDF, or to swap out only a few.

Requirements
------------

neasy-f requires a Fortran 2008 compiler and netCDF-Fortran.

Compilation
-----------

You can build neasy-f into a library using CMake:

```bash
$ cmake . -B build
$ cmake --build build
```
