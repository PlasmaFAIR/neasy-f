cmake_minimum_required(VERSION 3.20)

project(neasy-f
  VERSION 0.0.0
  DESCRIPTION "High-level netCDF Fortran wrapper"
  LANGUAGES Fortran
  )

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

string(COMPARE EQUAL
  "${PROJECT_NAME}" "${CMAKE_PROJECT_NAME}"
  PROJECT_IS_TOP_LEVEL
)

option(NEASYF_BUILD_EXAMPLES "Build the neasy-f examples" ${PROJECT_IS_TOP_LEVEL})

add_library(neasyf)
target_sources(neasyf PRIVATE
  src/neasy_f.f90
  )
add_library(neasyf::neasyf ALIAS neasyf)

# Location of built .mod files
set(neasyf_module_directory ${CMAKE_CURRENT_BINARY_DIR}/mod)
set(neasyf_module_install_directory "${CMAKE_INSTALL_INCLUDEDIR}" CACHE STRING
  "Fortran module installation directory")

set_target_properties(neasyf PROPERTIES
  Fortran_FORMAT FREE
  Fortran_MODULE_DIRECTORY ${neasyf_module_directory}
  )


target_include_directories(neasyf PUBLIC
  $<BUILD_INTERFACE:${neasyf_module_directory}>
  $<INSTALL_INTERFACE:${neasyf_module_install_directory}>
  )

find_package(netCDFFortran REQUIRED)
target_link_libraries(neasyf PUBLIC netCDF::netcdff)

if (NEASYF_BUILD_EXAMPLES)
  add_subdirectory(examples)
endif()
