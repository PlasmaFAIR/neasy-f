---
project: neasy-f
project_github: https://github.com/PlasmaFAIR/neasy-f
summary: "Nice and easy" netCDF-Fortran wrapper
author: Peter Hill
src_dir: ../src
output_dir: ./html
predocmark: >
exclude:
      neasyf.in.f90
      neasyf.type.in.f90
      neasyf.get_var.in.f90
      neasyf.put_var.in.f90
      neasyf.read.in.f90
      neasyf.write.in.f90
license: bsd
---

neasy-f is a short-and-sweet wrapper for netCDF-Fortran. Rather than
attempting to be a feature-complete replacement, neasy-f wraps some common
functions together and is opinionated about usage.

### Example

```fortran
ncid = neasyf_open("my_file.nc", "w")
call neasyf_dim(ncid, "x", unlimited=.true., x_dimid)
call neasyf_dim(ncid, "y", dim_size=NY, y_dimid)
call neasyf_write(ncid, "data", data_out, [y_dimid, x_dimid], &
                  units="Pa", description="Pressure")
call neasyf_close(ncid)
```
