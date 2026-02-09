# ==============================================================================
# Legacy/Uncategorized Files
# ==============================================================================
# Temporary library for 735 files not yet categorized
# These files remain in mis/, mds/, bd/ directories
# TODO: Properly categorize and migrate these files in future phases
# ==============================================================================

# Collect all remaining uncategorized files
file(GLOB LEGACY_MIS_SOURCES ${CMAKE_SOURCE_DIR}/mis/*.f)
file(GLOB LEGACY_MDS_SOURCES ${CMAKE_SOURCE_DIR}/mds/*.f)
file(GLOB LEGACY_BD_SOURCES ${CMAKE_SOURCE_DIR}/bd/*.f)

set(LEGACY_SOURCES ${LEGACY_MIS_SOURCES} ${LEGACY_MDS_SOURCES} ${LEGACY_BD_SOURCES})

if(LEGACY_SOURCES)
  add_library(nastran_legacy ${LEGACY_SOURCES})

  target_link_libraries(nastran_legacy
    PUBLIC nastran_common
    PUBLIC nastran_system
  )

  list(LENGTH LEGACY_SOURCES LEGACY_COUNT)
  message(STATUS "Legacy/Uncategorized: Found ${LEGACY_COUNT} files")

  # Installation
  install(TARGETS nastran_legacy
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()
