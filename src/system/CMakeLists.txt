# ==============================================================================
# System Layer - I/O, Database, Platform Services
# ==============================================================================
# Replaces original mds/ (Machine-Dependent Services) directory
# Provides platform abstraction, file I/O (GINO), and database management
# ==============================================================================

# I/O subsystem library (GINO layer)
add_library(nastran_io
  io/open.f90
  io/close.f90
  io/read.f90
  io/write.f90
  io/rewind.f90
  io/gino.f90
  io/pack.f90
  io/unpack.f90
  io/bpack.f90
  io/bunpak.f90
)

target_link_libraries(nastran_io
  PUBLIC nastran_common
)

# Database management subsystem
add_library(nastran_database
  database/dbmmgr.f90
  database/dbmint.f90
  database/dbmio.f90
)

target_link_libraries(nastran_database
  PUBLIC nastran_common
  PUBLIC nastran_io
)

# Platform abstraction layer
add_library(nastran_platform
  platform/btstrp.f90
  platform/dummy.f90
  platform/nastrn.f90
)

target_link_libraries(nastran_platform
  PUBLIC nastran_common
  PUBLIC nastran_io
  PUBLIC nastran_database
)

# Combined system library (convenience target)
add_library(nastran_system INTERFACE)
target_link_libraries(nastran_system INTERFACE
  nastran_io
  nastran_database
  nastran_platform
)

# Installation
install(TARGETS
  nastran_io
  nastran_database
  nastran_platform
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)
