neasy-f
=======

neasy-f is a short-and-sweet wrapper for netCDF-Fortran. Rather than attempting
to be a feature-complete replacement, neasy-f provides wrappers for some common
operations, trying to keep simple things simple.

For example, here's how you might write some two-dimensional data with plain
netCDF:

```fortran
  call check( nf90_create("my_file.nc", ior(nf90_clobber, nf90_netcdf4), ncid) )

  call check( nf90_def_dim(ncid, "x", NX, x_dimid) )
  call check( nf90_def_dim(ncid, "y", NY, y_dimid) )

  call check( nf90_def_var(ncid, "data", NF90_INT, [y_dimid, x_dimid], varid)
  call check( nf90_put_att(ncid, varid, "units", "Pa") )
  call check( nf90_put_att(ncid, varid, "long_name", "Synthetic pressure") )

  call check( nf90_enddef(ncid) )

  call check( nf90_put_var(ncid, varid, data_out) )

  call check( nf90_close(ncid) )
```

where `check` is a subroutine you must write yourself. Here's the same example,
but using neasy-f:

```fortran
  ncid = neasyf_open("my_file.nc", "w")

  call neasyf_dim(ncid, "x", dim_size=NX)
  call neasyf_dim(ncid, "y", dim_size=NY)

  call neasyf_write(ncid, "data", data_out, ["y", "x"], &
       units="Pa", long_name="Synthetic pressure")

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
- neasy-f can check the dimensions from an array of strings, which means we
  don't need to store the dimension IDs


neasy-f is designed to be "mostly drop-in", meaning you can use it to entirely
replace your use of "plain" netCDF, or to swap out only a few.

Requirements
------------

neasy-f requires a Fortran 2008 compiler and netCDF-Fortran. neasy-f uses
netCDF4 files, and so requires netCDF to have been built with HDF5.

Tested with gfortran 11 and netCDF-Fortran 4.5.3.

For developers,
[fypp](https://fypp.readthedocs.io/en/stable/index.html) is also required.

Known Issues
------------

- gfortran has a bug when passing slices of arrays of derived types:
  trying to write `some_array%member` will result in the wrong values
  getting written. To workaround this, copy `some_array%member` into a
  temporary variable and pass that to `neasyf_write`

- Creating fixed zero-length variables or dimensions is not possible,
  as netCDF uses `size = 0` to represent unlimited dimensions

Compilation
-----------

There are a few ways you can use neasy-f in your project.

### Single file inclusion

Perhaps the easiest way is to copy `src/neasyf.f90` into your project and build
it as part of your package. The downside to this approach is that you will have
to apply any updates manually. You'll also need to make sure you compile against
netCDF-Fortran correctly.

### External library

You can build neasy-f into a library using CMake:

```bash
$ cmake . -B build
$ cmake --build build
```

Note that if you do not have `nf-fortran` in your `$PATH`, you will need to tell
CMake the location of netCDF-Fortran like:

```bash
$ cmake . -B build -DnetCDFFortran_ROOT=/path/to/netcdf
```

You can then use the built libraries under `build/lib` and the `.mod` file under
`build/mod`. You can install these somewhere convenient with:

```bash
$ cmake . -B build -DCMAKE_INSTALL_PREFIX=/path/to/install
$ cmake --build build --target install
```

Neasy-f installs configuration files for CMake, so you can include neasy-f in
your CMake project with:

```cmake
find_package(neasyf)

target_link_libraries(<your target> PRIVATE neasyf::neasyf)
```

You can point CMake at either the install location or the build directory
directly with `-Dneasyf_ROOT=/path/to/install/or/build/dir`

### Internal target in CMake

You can use neasy-f directly in a CMake project with `FetchContent` like so:

```cmake
include(FetchContent)

FetchContent_declare(neasyf
    GIT_REPOSITORY https://github.com/PlasmaFAIR/neasy-f.git
    GIT_TAG v0.1.0)
FetchContent_makeavailable(neasyf)

target_link_libraries(<your target> PRIVATE neasyf::neasyf)
```

Implementation
--------------

NetCDF supports up to 7 dimensions. Most of neasy-f's implementation is agnostic
to the type, kind, and dimension, with some slight differences between the
scalar and n-dimensional overloads. The result is that there is a _lot_ of
repeated code in neasy-f. To handle this, we use the fypp preprocessor to
generate the (6 types/kinds times 8 rank) overloads of the neasy-f functions.

The compiled `neasyf.f90` file is generated from `neasyf.in.f90` by running:

```
fypp src/neasy.in.f90 src/neasyf.f90
```

If you're developing neasy-f, you might find it useful to add the `-n` flag,
which adds line directives, making it easier to track down compiler warnings and
errors.

If you develop neasy-f and make changes to the `neasyf.in.f90` files, don't
forget to re-generate the main `neasyf.f90` file and commit it.
