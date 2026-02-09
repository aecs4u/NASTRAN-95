# ==============================================================================
# System Layer - I/O, Database, Platform Services
# ==============================================================================
# Migrated from mds/ (Machine-Dependent Services) directory in Phase 3
# Provides platform abstraction, file I/O (GINO), and database management
# ==============================================================================

# Collect system files by subsystem
file(GLOB IO_SOURCES io/*.f)
file(GLOB DATABASE_SOURCES database/*.f)
file(GLOB PLATFORM_SOURCES platform/*.f)

# Temporarily exclude problematic files with gfortran parser issues
# TODO: Fix FORTRAN 77 DATA statement parsing in ascm0*.f files
list(FILTER DATABASE_SOURCES EXCLUDE REGEX "ascm01\\.f$")
list(FILTER DATABASE_SOURCES EXCLUDE REGEX "ascm05\\.f$")
list(FILTER DATABASE_SOURCES EXCLUDE REGEX "ascm06\\.f$")
list(FILTER DATABASE_SOURCES EXCLUDE REGEX "ascm07\\.f$")

# I/O subsystem library (GINO layer - 122 files)
if(IO_SOURCES)
  add_library(nastran_io ${IO_SOURCES})

  target_link_libraries(nastran_io
    PUBLIC nastran_common
  )

  list(LENGTH IO_SOURCES IO_COUNT)
  message(STATUS "System/IO: Found ${IO_COUNT} files")
endif()

# Database management subsystem (73 files)
if(DATABASE_SOURCES)
  add_library(nastran_database ${DATABASE_SOURCES})

  target_link_libraries(nastran_database
    PUBLIC nastran_common
    PUBLIC nastran_io
  )

  list(LENGTH DATABASE_SOURCES DB_COUNT)
  message(STATUS "System/Database: Found ${DB_COUNT} files")
endif()

# Platform abstraction layer (3 files)
if(PLATFORM_SOURCES)
  add_library(nastran_platform ${PLATFORM_SOURCES})

  target_link_libraries(nastran_platform
    PUBLIC nastran_common
    PUBLIC nastran_io
    PUBLIC nastran_database
  )

  list(LENGTH PLATFORM_SOURCES PLATFORM_COUNT)
  message(STATUS "System/Platform: Found ${PLATFORM_COUNT} files")
endif()

# Combined system library (convenience target)
add_library(nastran_system INTERFACE)
target_link_libraries(nastran_system INTERFACE
  $<$<TARGET_EXISTS:nastran_io>:nastran_io>
  $<$<TARGET_EXISTS:nastran_database>:nastran_database>
  $<$<TARGET_EXISTS:nastran_platform>:nastran_platform>
)

# Installation
if(TARGET nastran_io)
  install(TARGETS nastran_io ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
endif()

if(TARGET nastran_database)
  install(TARGETS nastran_database ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
endif()

if(TARGET nastran_platform)
  install(TARGETS nastran_platform ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
endif()
