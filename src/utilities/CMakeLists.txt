# ==============================================================================
# Utilities - Parser, Output, Helpers
# ==============================================================================
# Migrated from mis/, um/, utility/ directories in Phase 3
# ==============================================================================

# Collect utility files by type
file(GLOB PARSER_SOURCES parser/*.f)
file(GLOB OUTPUT_SOURCES output/*.f)
file(GLOB HELPER_SOURCES helpers/*.f)

# Input parser (35 files)
if(PARSER_SOURCES)
  add_library(nastran_parser ${PARSER_SOURCES})

  target_link_libraries(nastran_parser
    PUBLIC nastran_common
  )

  list(LENGTH PARSER_SOURCES PARSER_COUNT)
  message(STATUS "Utilities/Parser: Found ${PARSER_COUNT} files")
endif()

# Output formatting (167 files)
if(OUTPUT_SOURCES)
  add_library(nastran_output ${OUTPUT_SOURCES})

  target_link_libraries(nastran_output
    PUBLIC nastran_common
    PUBLIC nastran_system
  )

  list(LENGTH OUTPUT_SOURCES OUTPUT_COUNT)
  message(STATUS "Utilities/Output: Found ${OUTPUT_COUNT} files")
endif()

# Helper utilities (108 files - sorting, packing, MCB, etc.)
if(HELPER_SOURCES)
  add_library(nastran_helpers ${HELPER_SOURCES})

  target_link_libraries(nastran_helpers
    PUBLIC nastran_common
  )

  list(LENGTH HELPER_SOURCES HELPER_COUNT)
  message(STATUS "Utilities/Helpers: Found ${HELPER_COUNT} files")
endif()

# Combined utilities library (convenience target)
add_library(nastran_utilities INTERFACE)
target_link_libraries(nastran_utilities INTERFACE
  $<$<TARGET_EXISTS:nastran_parser>:nastran_parser>
  $<$<TARGET_EXISTS:nastran_output>:nastran_output>
  $<$<TARGET_EXISTS:nastran_helpers>:nastran_helpers>
)

# Installation
if(TARGET nastran_parser)
  install(TARGETS nastran_parser ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
endif()

if(TARGET nastran_output)
  install(TARGETS nastran_output ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
endif()

if(TARGET nastran_helpers)
  install(TARGETS nastran_helpers ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
endif()
