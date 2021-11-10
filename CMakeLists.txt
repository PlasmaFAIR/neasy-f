cmake_minimum_required(VERSION 3.20)

project(nf-wrapper
  VERSION 0.0.0
  DESCRIPTION "High-level netCDF Fortran wrapper"
  LANGUAGES Fortran
  )

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

add_library(nf-wrapper)
target_sources(nf-wrapper PRIVATE
  src/write_any.f90
  )

find_package(netCDFFortran REQUIRED)
target_link_libraries(nf-wrapper PUBLIC netCDF::netcdff)
