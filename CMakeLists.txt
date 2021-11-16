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

# We need this early on to set the install location of .mod files
include(GNUInstallDirs)

##################################################
# Options

option(NEASYF_BUILD_EXAMPLES "Build the neasy-f examples" ${PROJECT_IS_TOP_LEVEL})
option(NEASYF_BUILD_TESTS "Build the neasy-f tests" ${PROJECT_IS_TOP_LEVEL})
option(NEASYF_DOWNLOAD_PFUNIT "Download pFUnit as part of the configuration" ${NEASYF_BUILD_TESTS})
option(NEASYF_GENERATE_SOURCE "Generate the source file from the input files. This option is for developers only" ${PROJECT_IS_TOP_LEVEL})

##################################################
# neasy-f library

add_library(neasyf)
target_sources(neasyf PRIVATE
  src/neasyf.f90
  )
add_library(neasyf::neasyf ALIAS neasyf)

# Location of built .mod files
set(neasyf_module_directory ${CMAKE_CURRENT_BINARY_DIR}/mod)
set(neasyf_module_install_directory "${CMAKE_INSTALL_INCLUDEDIR}" CACHE STRING
  "Fortran module installation directory")

set_target_properties(neasyf PROPERTIES
  Fortran_FORMAT FREE
  Fortran_MODULE_DIRECTORY ${neasyf_module_directory}
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib
  ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib
  )

target_include_directories(neasyf PUBLIC
  $<BUILD_INTERFACE:${neasyf_module_directory}>
  $<INSTALL_INTERFACE:${neasyf_module_install_directory}>
  )

if (NEASYF_GENERATE_SOURCE)
  find_package(Python3)
  if (NOT Python3_FOUND)
    message(FATAL_ERROR "Python not found. Unable to generate source file. If you just want to build \
neasy-f, then disable source generation with NEASYF_GENERATE_SOURCE=off.")
  endif()

  set(neasy_f_source_file "${CMAKE_CURRENT_SOURCE_DIR}/src/neasyf.f90")
  add_custom_command(OUTPUT ${neasy_f_source_file}
    COMMAND ${Python3_EXECUTABLE} generate_source.py --write-to ${neasy_f_source_file}
    DEPENDS
      "${CMAKE_CURRENT_SOURCE_DIR}/src/generate_source.py"
      "${CMAKE_CURRENT_SOURCE_DIR}/src/neasyf.in.f90"
      "${CMAKE_CURRENT_SOURCE_DIR}/src/neasyf.type.in.f90"
      "${CMAKE_CURRENT_SOURCE_DIR}/src/neasyf.get_var.in.f90"
      "${CMAKE_CURRENT_SOURCE_DIR}/src/neasyf.put_var.in.f90"
      "${CMAKE_CURRENT_SOURCE_DIR}/src/neasyf.read.in.f90"
      "${CMAKE_CURRENT_SOURCE_DIR}/src/neasyf.write.in.f90"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src
    COMMENT "Generating source code"
    )
endif()

##################################################
# Dependencies

find_package(netCDFFortran REQUIRED)
target_link_libraries(neasyf PUBLIC netCDF::netcdff)

##################################################
# Examples and tests

if (NEASYF_BUILD_EXAMPLES)
  add_subdirectory(examples)
endif()

if (NEASYF_BUILD_TESTS)
  if (NEASYF_DOWNLOAD_PFUNIT)
    message(STATUS "Downloading pFUnit...")
    include(FetchContent)
    set(NEASYF_DOWNLOAD_PFUNIT_URL "https://github.com/Goddard-Fortran-Ecosystem/pFUnit"
      CACHE STRING "URL of pFUnit repository")

    set(NEASYF_PFUNIT_LOCATION "${NEASYF_DOWNLOAD_PFUNIT_URL}")

    FetchContent_Declare(
      pfunit
      GIT_REPOSITORY ${NEASYF_DOWNLOAD_PFUNIT_URL}
      GIT_TAG        main
      )

    set(SKIP_OPENMP YES CACHE BOOL "" FORCE)
    set(SKIP_MPI YES CACHE BOOL "" FORCE)
    # Workaround for bug in current main at time of commit
    set(BUILD_SHARED ${BUILD_SHARED_LIBS})
    if (NOT CMAKE_Fortran_FLAGS)
      if (CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
        # Workaround for an issue in pFUnit: preprocessor macro
        # expansion of the current source directory, which may
        # truncate the line
        set(CMAKE_Fortran_FLAGS "-ffree-line-length-none" CACHE STRING "" FORCE)
        message(STATUS "Setting CMAKE_Fortran_FLAGS = ${CMAKE_Fortran_FLAGS}")
      endif()
    endif()
    FetchContent_MakeAvailable(pfunit)
  else()
    find_package(PFUNIT REQUIRED)
    set(NEASYF_PFUNIT_LOCATION "${PFUNIT_ROOT}")
  endif()

  include(CTest)
  enable_testing()

  add_custom_target(build-check)
  set(NEASYF_CTEST_CASES)
  neasyf_add_standard_tests(TEST_NAMES
    dims
    write
    read
    )
  add_custom_target(check
    COMMAND ctest --output-on-failure
    DEPENDS ${NEASYF_CTEST_CASES}
    )
  add_dependencies(check build-check)
else()
  set(NEASYF_PFUNIT_LOCATION "Not used")
endif()

##################################################
# Installation

install(TARGETS neasyf
  EXPORT neasyfTargets
  LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  )
install(DIRECTORY "${neasyf_module_directory}/"
  DESTINATION "${neasyf_module_install_directory}"
  FILES_MATCHING PATTERN "*.mod"
  )

include(CMakePackageConfigHelpers)
write_basic_package_version_file(
  neasyfConfigVersion.cmake
  VERSION ${PACKAGE_VERSION}
  COMPATIBILITY SameMajorVersion
  )

install(EXPORT neasyfTargets
  FILE neasyfTargets.cmake
  NAMESPACE neasyf::
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/neasyf"
  )

configure_package_config_file(neasyfConfig.cmake.in neasyfConfig.cmake
  INSTALL_DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/neasyfConfig.cmake"
  )

# CMake configuration files
install(
  FILES
      "${CMAKE_CURRENT_BINARY_DIR}/neasyfConfig.cmake"
      "${CMAKE_CURRENT_BINARY_DIR}/neasyfConfigVersion.cmake"
      "${CMAKE_CURRENT_SOURCE_DIR}/cmake/GKfunctions.cmake"
      "${CMAKE_CURRENT_SOURCE_DIR}/cmake/FindnetCDF.cmake"
      "${CMAKE_CURRENT_SOURCE_DIR}/cmake/FindnetCDFFortran.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/neasyf"
  )

export(EXPORT neasyfTargets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/neasyfTargets.cmake"
  NAMESPACE neasyf::
  )

export(PACKAGE neasyf)
