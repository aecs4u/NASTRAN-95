# ==============================================================================
# NASTRAN-95 Examples
# ==============================================================================
# Example input files and reference outputs for verification
#
# This directory contains demonstration problems from the original
# NASTRAN-95 test suite.
# ==============================================================================

# No build targets - examples are just input files
# Installation is handled by the root CMakeLists.txt

message(STATUS "Examples configured: ${CMAKE_CURRENT_SOURCE_DIR}")

# Optional: Add custom target to list available examples
file(GLOB EXAMPLE_FILES input/*.dat input/*.bdf)
list(LENGTH EXAMPLE_FILES EXAMPLE_COUNT)
message(STATUS "Found ${EXAMPLE_COUNT} example input files")
