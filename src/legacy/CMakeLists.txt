# ==============================================================================
# Legacy/Uncategorized Files
# ==============================================================================
# Temporary library for 735 files not yet categorized
# These files remain in mis/, mds/, bd/ directories
# TODO: Properly categorize and migrate these files in future phases
# ==============================================================================

# Collect all remaining uncategorized files from old locations
file(GLOB LEGACY_MIS_SOURCES ${CMAKE_SOURCE_DIR}/mis/*.f)
file(GLOB LEGACY_MDS_SOURCES ${CMAKE_SOURCE_DIR}/mds/*.f)
file(GLOB LEGACY_BD_SOURCES ${CMAKE_SOURCE_DIR}/bd/*.f)

# Auto-exclude any mis/*.f file that has a corresponding file in src/utilities/
# This prevents duplicate symbols when files are migrated to src/utilities/
file(GLOB UTIL_PARSER_SOURCES ${CMAKE_SOURCE_DIR}/src/utilities/parser/*.f ${CMAKE_SOURCE_DIR}/src/utilities/parser/*.f90)
file(GLOB UTIL_OUTPUT_SOURCES ${CMAKE_SOURCE_DIR}/src/utilities/output/*.f ${CMAKE_SOURCE_DIR}/src/utilities/output/*.f90)
file(GLOB UTIL_HELPER_SOURCES ${CMAKE_SOURCE_DIR}/src/utilities/helpers/*.f ${CMAKE_SOURCE_DIR}/src/utilities/helpers/*.f90)

set(UTIL_ALL_SOURCES ${UTIL_PARSER_SOURCES} ${UTIL_OUTPUT_SOURCES} ${UTIL_HELPER_SOURCES})

set(EXCLUDED_COUNT 0)
foreach(UTIL_FILE ${UTIL_ALL_SOURCES})
  get_filename_component(BASENAME ${UTIL_FILE} NAME_WE)
  set(LEGACY_FILE ${CMAKE_SOURCE_DIR}/mis/${BASENAME}.f)
  if(LEGACY_FILE IN_LIST LEGACY_MIS_SOURCES)
    list(REMOVE_ITEM LEGACY_MIS_SOURCES ${LEGACY_FILE})
    math(EXPR EXCLUDED_COUNT "${EXCLUDED_COUNT} + 1")
  endif()
endforeach()

if(EXCLUDED_COUNT GREATER 0)
  message(STATUS "Legacy: Excluded ${EXCLUDED_COUNT} mis/ files (modernized versions in src/utilities/)")
endif()

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
