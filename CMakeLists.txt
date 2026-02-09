# ==============================================================================
# NASTRAN-95 Modernized Build System
# ==============================================================================
# Modern CMake build system for historic NASA NASTRAN-95 FEA software
#
# Original: FORTRAN 77, monolithic static library (1995)
# Modernized: Fortran 2003+, modular architecture, parallel build
#
# Build time improvement: 2-3 hours → <10 minutes (estimated)
#
# Authors: Original NASA/NASTRAN team (1970s-1995)
#          Modernization: 2026
# ==============================================================================

cmake_minimum_required(VERSION 3.15)
project(NASTRAN95
  VERSION 1.0.0
  LANGUAGES Fortran C
  DESCRIPTION "NASA Structural Analysis System - Modernized"
)

# ==============================================================================
# Project Configuration
# ==============================================================================

# Set C and Fortran standards
set(CMAKE_Fortran_STANDARD 2003)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD 99)

# Module output directory (for .mod files)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

# Add module path for CMake modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# ==============================================================================
# Build Options
# ==============================================================================

option(BUILD_SHARED_LIBS "Build shared libraries instead of static" OFF)
option(ENABLE_OPENMP "Enable OpenMP parallelization for element loops" ON)
option(USE_EXTERNAL_BLAS "Use external optimized BLAS/LAPACK (recommended)" ON)
option(USE_MUMPS "Enable MUMPS sparse direct solver backend" OFF)
option(MUMPS_USE_MPI "Link MUMPS backend with MPI Fortran runtime" ON)
option(BUILD_EXAMPLES "Build example programs" ON)
option(BUILD_TESTS "Build unit tests" ON)
option(BUILD_DOCUMENTATION "Build Doxygen documentation" OFF)
option(ENABLE_PROFILING "Enable performance profiling instrumentation" OFF)

set(MUMPS_ROOT "" CACHE PATH "Root directory of a MUMPS installation")
set(MUMPS_INCLUDE_DIR "" CACHE PATH "Directory containing dmumps_struc.h")
set(MUMPS_LIBRARIES "" CACHE STRING "Semicolon-separated list of MUMPS/ScaLAPACK libraries")

# ==============================================================================
# Compiler Flags
# ==============================================================================

# Common Fortran flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")  # Enable preprocessor

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  # GFortran flags
  # Note: Removed -ffree-form to allow auto-detection (.f = fixed, .f90 = free)
  # Note: Removed -fimplicit-none to support legacy FORTRAN 77 code
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=legacy")  # Allow legacy features
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdec")  # Enable DEC extensions (CARRIAGECONTROL, etc.)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-invalid-boz")  # Allow old BOZ syntax (e.g., 'F1111'X)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffixed-line-length-none")  # No line length limit
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fd-lines-as-comments")  # Treat 'D' lines as comments
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-range-check")  # Don't check constant ranges
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wno-tabs -Wno-conversion")  # Warn but allow tabs and conversions
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=bounds -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native -funroll-loops")

  # Educational/debugging aids (relaxed for legacy code)
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -ffpe-trap=invalid,zero,overflow")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  # Intel Fortran flags
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -free -implicitnone")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -check all -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -xHost -ipo")

endif()

# Profiling flags
if(ENABLE_PROFILING)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DENABLE_PROFILING")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DENABLE_PROFILING")
endif()

# ==============================================================================
# Dependencies
# ==============================================================================

# BLAS/LAPACK (for high-performance linear algebra)
if(USE_EXTERNAL_BLAS)
  find_package(BLAS REQUIRED)
  find_package(LAPACK REQUIRED)
  message(STATUS "Using external BLAS: ${BLAS_LIBRARIES}")
  message(STATUS "Using external LAPACK: ${LAPACK_LIBRARIES}")
else()
  message(STATUS "Using internal linear algebra routines (slower)")
endif()

# MUMPS (optional sparse direct solver backend)
if(USE_MUMPS)
  if(MUMPS_USE_MPI)
    find_package(MPI REQUIRED COMPONENTS Fortran)
  endif()

  if(NOT MUMPS_INCLUDE_DIR)
    if(MUMPS_ROOT)
      find_path(MUMPS_INCLUDE_DIR
        NAMES dmumps_struc.h
        HINTS ${MUMPS_ROOT}
        PATH_SUFFIXES include include/mumps
      )
    else()
      find_path(MUMPS_INCLUDE_DIR NAMES dmumps_struc.h)
    endif()
  endif()

  if(NOT MUMPS_LIBRARIES)
    set(_mumps_lib_hints)
    if(MUMPS_ROOT)
      list(APPEND _mumps_lib_hints ${MUMPS_ROOT})
    endif()

    find_library(MUMPS_DMUMPS_LIBRARY
      NAMES dmumps
      HINTS ${_mumps_lib_hints}
      PATH_SUFFIXES lib lib64
    )
    find_library(MUMPS_COMMON_LIBRARY
      NAMES mumps_common
      HINTS ${_mumps_lib_hints}
      PATH_SUFFIXES lib lib64
    )
    find_library(MUMPS_PORD_LIBRARY
      NAMES pord
      HINTS ${_mumps_lib_hints}
      PATH_SUFFIXES lib lib64
    )

    if(MUMPS_DMUMPS_LIBRARY AND MUMPS_COMMON_LIBRARY AND MUMPS_PORD_LIBRARY)
      set(MUMPS_LIBRARIES
        "${MUMPS_DMUMPS_LIBRARY};${MUMPS_COMMON_LIBRARY};${MUMPS_PORD_LIBRARY}"
        CACHE STRING "Semicolon-separated list of MUMPS/ScaLAPACK libraries"
        FORCE
      )
    endif()
  endif()

  if(NOT MUMPS_INCLUDE_DIR OR NOT MUMPS_LIBRARIES)
    message(FATAL_ERROR
      "USE_MUMPS=ON but MUMPS was not fully configured.\n"
      "Set MUMPS_INCLUDE_DIR (path with dmumps_struc.h) and MUMPS_LIBRARIES "
      "(e.g. dmumps;mumps_common;pord;scalapack;blas/lapack)."
    )
  endif()

  add_library(MUMPS::MUMPS INTERFACE IMPORTED)
  set_target_properties(MUMPS::MUMPS PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${MUMPS_INCLUDE_DIR}"
    INTERFACE_LINK_LIBRARIES "${MUMPS_LIBRARIES}"
  )

  if(MUMPS_USE_MPI)
    target_link_libraries(MUMPS::MUMPS INTERFACE MPI::MPI_Fortran)
  endif()

  add_compile_definitions(NASTRAN_HAVE_MUMPS=1)
  message(STATUS "MUMPS enabled: include=${MUMPS_INCLUDE_DIR}")
  message(STATUS "MUMPS libraries: ${MUMPS_LIBRARIES}")
endif()

# OpenMP (for parallel element loops)
if(ENABLE_OPENMP)
  find_package(OpenMP REQUIRED)
  message(STATUS "OpenMP enabled - parallel element processing")
endif()

# ==============================================================================
# Global Include Directories
# ==============================================================================

include_directories(
  ${CMAKE_Fortran_MODULE_DIRECTORY}
  ${CMAKE_SOURCE_DIR}/src/common
  ${CMAKE_SOURCE_DIR}/src/common/include  # FORTRAN 77 .COM include files
)

# ==============================================================================
# Source Subdirectories
# ==============================================================================

# Common modules (precision, constants, shared state)
add_subdirectory(src/common)

# System layer (I/O, database, platform abstraction)
add_subdirectory(src/system)

# Core algorithms (alg*, mma* families consolidated)
add_subdirectory(src/core)

# Element implementations (shell, beam, aero, solid)
add_subdirectory(src/elements)

# Matrix solvers (direct, iterative, eigenvalue)
add_subdirectory(src/solvers)

# Analysis modules (static, dynamic, modal)
add_subdirectory(src/analysis)

# Utilities (parser, output formatting, helpers)
add_subdirectory(src/utilities)

# Block data initialization (converted to modules)
add_subdirectory(src/blockdata)

# Legacy/uncategorized files (temporary - 735 files)
add_subdirectory(src/legacy)

# Main executable
add_subdirectory(bin)

# ==============================================================================
# Optional Components
# ==============================================================================

if(BUILD_EXAMPLES)
  add_subdirectory(examples)
endif()

if(BUILD_TESTS)
  enable_testing()
  add_subdirectory(tests)
endif()

if(BUILD_DOCUMENTATION)
  find_package(Doxygen)
  if(DOXYGEN_FOUND)
    add_subdirectory(docs)
  else()
    message(WARNING "Doxygen not found - documentation will not be built")
  endif()
endif()

# ==============================================================================
# Installation
# ==============================================================================

# Install executables
install(TARGETS nastran
  RUNTIME DESTINATION bin
)

# Install configuration template
install(FILES etc/nastran.conf.template
  DESTINATION etc
  RENAME nastran.conf
)

# Install documentation
install(DIRECTORY docs/
  DESTINATION share/doc/nastran95
  PATTERN "*.md"
)

# Install examples
if(BUILD_EXAMPLES)
  install(DIRECTORY examples/
    DESTINATION share/nastran95/examples
  )
endif()

# Install demonstration problems
install(DIRECTORY inp/
  DESTINATION share/nastran95/inp
)

# ==============================================================================
# Package Configuration
# ==============================================================================

include(CMakePackageConfigHelpers)

# Generate package config file
configure_package_config_file(
  ${CMAKE_SOURCE_DIR}/cmake/NASTRAN95Config.cmake.in
  ${CMAKE_BINARY_DIR}/NASTRAN95Config.cmake
  INSTALL_DESTINATION lib/cmake/NASTRAN95
)

# Generate version file
write_basic_package_version_file(
  ${CMAKE_BINARY_DIR}/NASTRAN95ConfigVersion.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion
)

# Install package configuration
install(FILES
  ${CMAKE_BINARY_DIR}/NASTRAN95Config.cmake
  ${CMAKE_BINARY_DIR}/NASTRAN95ConfigVersion.cmake
  DESTINATION lib/cmake/NASTRAN95
)

# ==============================================================================
# Build Summary
# ==============================================================================

message(STATUS "")
message(STATUS "=========================================================")
message(STATUS " NASTRAN-95 Modernized Build Configuration")
message(STATUS "=========================================================")
message(STATUS " Build type:          ${CMAKE_BUILD_TYPE}")
message(STATUS " Fortran compiler:    ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS " Fortran standard:    F2003")
message(STATUS "")
message(STATUS " Options:")
message(STATUS "   Shared libraries:  ${BUILD_SHARED_LIBS}")
message(STATUS "   OpenMP parallel:   ${ENABLE_OPENMP}")
message(STATUS "   External BLAS:     ${USE_EXTERNAL_BLAS}")
message(STATUS "   MUMPS backend:     ${USE_MUMPS}")
message(STATUS "   Build examples:    ${BUILD_EXAMPLES}")
message(STATUS "   Build tests:       ${BUILD_TESTS}")
message(STATUS "   Profiling:         ${ENABLE_PROFILING}")
message(STATUS "")
message(STATUS " Install prefix:      ${CMAKE_INSTALL_PREFIX}")
message(STATUS "=========================================================")
message(STATUS "")
