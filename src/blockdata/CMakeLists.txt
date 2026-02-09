# ==============================================================================
# Block Data - Initialization Modules
# ==============================================================================
# Converted from original bd/ BLOCK DATA statements to Fortran 90+ modules
# Replaces COMMON block initialization with module variables
# ==============================================================================

# Note: In modern Fortran, BLOCK DATA is replaced by module initialization
# These will be refactored to module variables with default values

add_library(nastran_blockdata
  semdbd_module.f90
  exiobd_module.f90
  # Other bd/*.f files converted to modules
)

target_link_libraries(nastran_blockdata
  PUBLIC nastran_common
)

# Installation
install(TARGETS nastran_blockdata
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)
