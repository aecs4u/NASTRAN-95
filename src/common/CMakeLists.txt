# ==============================================================================
# Common Modules - Foundation for NASTRAN-95 Modernization
# ==============================================================================
# These modules replace COMMON blocks and provide type-safe infrastructure
#
# Modules:
#   - precision_module.f90  - Precision kinds (dp, sp, ip, etc.)
#   - constants_module.f90  - Physical, mathematical, system constants
#
# All other NASTRAN modules depend on these common modules.
# ==============================================================================

# Create library for common modules
add_library(nastran_common
  precision_module.f90
  constants_module.f90
)

# Set module directory
set_target_properties(nastran_common PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

# These modules have no dependencies (foundation layer)
# Other modules will link against nastran_common

# Install module files
install(TARGETS nastran_common
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)

# Install module .mod files
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
  DESTINATION include/nastran
  FILES_MATCHING PATTERN "*.mod"
)
