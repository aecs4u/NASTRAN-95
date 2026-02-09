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
option(BUILD_EXAMPLES "Build example programs" ON)
option(BUILD_TESTS "Build unit tests" ON)
option(BUILD_DOCUMENTATION "Build Doxygen documentation" OFF)
option(ENABLE_PROFILING "Enable performance profiling instrumentation" OFF)

# ==============================================================================
# Compiler Flags
# ==============================================================================

# Common Fortran flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")  # Enable preprocessor

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  # GFortran flags
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-form -fimplicit-none")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -pedantic")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native -funroll-loops")

  # Educational/debugging aids
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

# Utilities (parser, output formatting, helpers)
add_subdirectory(src/utilities)

# Block data initialization (converted to modules)
add_subdirectory(src/blockdata)

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
message(STATUS "   Build examples:    ${BUILD_EXAMPLES}")
message(STATUS "   Build tests:       ${BUILD_TESTS}")
message(STATUS "   Profiling:         ${ENABLE_PROFILING}")
message(STATUS "")
message(STATUS " Install prefix:      ${CMAKE_INSTALL_PREFIX}")
message(STATUS "=========================================================")
message(STATUS "")
