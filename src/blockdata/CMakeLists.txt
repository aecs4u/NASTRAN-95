# ==============================================================================
# Block Data - Initialization
# ==============================================================================
# Migrated from bd/ directory in Phase 3 (40 files)
# Original BLOCK DATA statements for COMMON block initialization
# Will be refactored to module variables in Phase 4
# ==============================================================================

# Collect all block data files
file(GLOB BLOCKDATA_SOURCES init/*.f)

# Block data initialization library (40 files)
if(BLOCKDATA_SOURCES)
  add_library(nastran_blockdata ${BLOCKDATA_SOURCES})

  target_link_libraries(nastran_blockdata
    PUBLIC nastran_common
  )

  list(LENGTH BLOCKDATA_SOURCES BD_COUNT)
  message(STATUS "Blockdata/Init: Found ${BD_COUNT} files")

  # Installation
  install(TARGETS nastran_blockdata
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()
