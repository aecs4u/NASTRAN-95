# ==============================================================================
# NASTRAN-95 Regression Tests
# ==============================================================================
# Focused checks that validate modified routines against original baselines.
# ==============================================================================

message(STATUS "Tests configured: ${CMAKE_CURRENT_SOURCE_DIR}")

find_package(Python3 REQUIRED COMPONENTS Interpreter)

set(NASTRAN_ORIGINAL_SOURCE_ROOT "" CACHE PATH
    "Root path of original NASTRAN-95 sources used as comparison baseline")

if(NOT NASTRAN_ORIGINAL_SOURCE_ROOT)
  if(EXISTS "/mnt/mobile/workspace/NASTRAN-95")
    set(NASTRAN_ORIGINAL_SOURCE_ROOT "/mnt/mobile/workspace/NASTRAN-95")
  else()
    set(NASTRAN_ORIGINAL_SOURCE_ROOT "${CMAKE_SOURCE_DIR}")
  endif()
endif()

message(STATUS "Baseline source root: ${NASTRAN_ORIGINAL_SOURCE_ROOT}")

set(_VALIDATE_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/validate_modified_vs_original.py")
set(_TEST_NASTRN "${CMAKE_CURRENT_SOURCE_DIR}/src/system/platform/test_nastrn_vs_original.py")
set(_TEST_XSEM00 "${CMAKE_CURRENT_SOURCE_DIR}/src/system/database/test_xsem00_vs_original.py")
set(_TEST_XREAD "${CMAKE_CURRENT_SOURCE_DIR}/src/utilities/parser/test_xread_vs_original.py")
set(_TEST_ALL_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/test_all_src_vs_original.py")
set(_TEST_ALL_COMMON "${CMAKE_CURRENT_SOURCE_DIR}/src/common/test_all_common_vs_original.py")
set(_TEST_ALL_SYSTEM "${CMAKE_CURRENT_SOURCE_DIR}/src/system/test_all_system_vs_original.py")
set(_TEST_ALL_CORE "${CMAKE_CURRENT_SOURCE_DIR}/src/core/test_all_core_vs_original.py")
set(_TEST_ALL_ELEMENTS "${CMAKE_CURRENT_SOURCE_DIR}/src/elements/test_all_elements_vs_original.py")
set(_TEST_ALL_SOLVERS "${CMAKE_CURRENT_SOURCE_DIR}/src/solvers/test_all_solvers_vs_original.py")
set(_TEST_ALL_ANALYSIS "${CMAKE_CURRENT_SOURCE_DIR}/src/analysis/test_all_analysis_vs_original.py")
set(_TEST_ALL_UTILITIES "${CMAKE_CURRENT_SOURCE_DIR}/src/utilities/test_all_utilities_vs_original.py")
set(_TEST_ALL_BLOCKDATA "${CMAKE_CURRENT_SOURCE_DIR}/src/blockdata/test_all_blockdata_vs_original.py")
set(_TEST_ALL_LEGACY "${CMAKE_CURRENT_SOURCE_DIR}/src/legacy/test_all_legacy_vs_original.py")

add_test(
  NAME validate_src_system_platform_nastrn
  COMMAND ${Python3_EXECUTABLE} ${_TEST_NASTRN}
)

add_test(
  NAME validate_src_system_database_xsem00
  COMMAND ${Python3_EXECUTABLE} ${_TEST_XSEM00}
)

add_test(
  NAME validate_src_utilities_parser_xread
  COMMAND ${Python3_EXECUTABLE} ${_TEST_XREAD}
)

add_test(
  NAME validate_src_common_all
  COMMAND ${Python3_EXECUTABLE} ${_TEST_ALL_COMMON}
)

add_test(
  NAME validate_src_system_all
  COMMAND ${Python3_EXECUTABLE} ${_TEST_ALL_SYSTEM}
)

add_test(
  NAME validate_src_core_all
  COMMAND ${Python3_EXECUTABLE} ${_TEST_ALL_CORE}
)

add_test(
  NAME validate_src_elements_all
  COMMAND ${Python3_EXECUTABLE} ${_TEST_ALL_ELEMENTS}
)

add_test(
  NAME validate_src_solvers_all
  COMMAND ${Python3_EXECUTABLE} ${_TEST_ALL_SOLVERS}
)

add_test(
  NAME validate_src_analysis_all
  COMMAND ${Python3_EXECUTABLE} ${_TEST_ALL_ANALYSIS}
)

add_test(
  NAME validate_src_utilities_all
  COMMAND ${Python3_EXECUTABLE} ${_TEST_ALL_UTILITIES}
)

add_test(
  NAME validate_src_blockdata_all
  COMMAND ${Python3_EXECUTABLE} ${_TEST_ALL_BLOCKDATA}
)

add_test(
  NAME validate_src_legacy_all
  COMMAND ${Python3_EXECUTABLE} ${_TEST_ALL_LEGACY}
)

add_test(
  NAME validate_src_all
  COMMAND ${Python3_EXECUTABLE} ${_TEST_ALL_SRC}
)

set_tests_properties(
  validate_src_system_platform_nastrn
  validate_src_system_database_xsem00
  validate_src_utilities_parser_xread
  validate_src_common_all
  validate_src_system_all
  validate_src_core_all
  validate_src_elements_all
  validate_src_solvers_all
  validate_src_analysis_all
  validate_src_utilities_all
  validate_src_blockdata_all
  validate_src_legacy_all
  validate_src_all
  PROPERTIES
  ENVIRONMENT "NASTRAN_ORIGINAL_SOURCE_ROOT=${NASTRAN_ORIGINAL_SOURCE_ROOT}"
)

option(ENABLE_PER_FILE_SRC_TESTS "Create one routine-validation test per src Fortran file" ON)

if(ENABLE_PER_FILE_SRC_TESTS)
  file(GLOB_RECURSE _SRC_FORTRAN_FILES
    RELATIVE ${CMAKE_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src/*.f
    ${CMAKE_SOURCE_DIR}/src/*.for
    ${CMAKE_SOURCE_DIR}/src/*.f90
  )
  list(SORT _SRC_FORTRAN_FILES)
  list(LENGTH _SRC_FORTRAN_FILES _SRC_FORTRAN_COUNT)
  message(STATUS "Per-file src routine tests: ${_SRC_FORTRAN_COUNT}")

  foreach(_src_file IN LISTS _SRC_FORTRAN_FILES)
    string(REGEX REPLACE "[^A-Za-z0-9_]" "_" _src_slug "${_src_file}")
    set(_src_test_name "validate_src_file_${_src_slug}")

    add_test(
      NAME ${_src_test_name}
      COMMAND ${Python3_EXECUTABLE} ${_VALIDATE_SCRIPT}
              --repo-root ${CMAKE_SOURCE_DIR}
              --original-root ${NASTRAN_ORIGINAL_SOURCE_ROOT}
              --source-file ${_src_file}
              --auto-map
              --allow-no-routines
              --label ${_src_file}
    )

    set_property(
      TEST ${_src_test_name}
      PROPERTY ENVIRONMENT
      "NASTRAN_ORIGINAL_SOURCE_ROOT=${NASTRAN_ORIGINAL_SOURCE_ROOT}"
    )
  endforeach()
endif()
