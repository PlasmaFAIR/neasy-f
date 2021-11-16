---
project: neasy-f
project_github: https://github.com/PlasmaFAIR/neasy-f
summary: "Nice and easy" netCDF-Fortran wrapper
author: Peter Hill
src_dir: ./src
output_dir: ./docs/html
predocmark: >
exclude:
      neasy_f.in.f90
      neasy_f.type.in.f90
      neasy_f.get_var.in.f90
      neasy_f.put_var.in.f90
      neasy_f.read.in.f90
      neasy_f.write.in.f90
license: bsd
---

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

  call neasyf_dim(ncid, "x", dim_size=NX, x_dimid)
  call neasyf_dim(ncid, "y", dim_size=NY, y_dimid)

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

Implementation
--------------

To simplify a lot of the code, neasy-f uses unlimited polymorphism (`class(*)`)
for a few dummy/input variables. This means that instead of needing to write
separate implementations for `integer, real, character`, and their different
`kind`s, _as well as_ handling scalar and array arguments, we only need to
overload on the array rank. This possibly has a slight runtime overhead, as
we've shifted from compile-time dispatch to runtime, but this is should be small
compared to reading from/writing to disk.

NetCDF supports up to 7 dimensions. Most of neasy-f's implementation is agnostic
to the dimension, with some slight differences between the scalar and
n-dimensional overloads. The result is that there is a _lot_ of repeated code in
neasy-f. To handle this, there is a short Python script to generate the 1-7D
overloads of the neasy-f functions.

The various `*.in.f90` files hold the "real" implementations, and
`src/generate_source.py` generates the code and spits it out. To update the main
code, run:

```
$ ./generate_source.py > neasy_f.f90
```
