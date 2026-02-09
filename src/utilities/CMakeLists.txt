# ==============================================================================
# Utilities - Parser, Output, Helpers, Profiling
# ==============================================================================
# Support utilities from mis/, um/ directories
# ==============================================================================

# Input parser
add_library(nastran_parser
  parser/input_module.f90
  parser/numtyp.f90
  parser/fornum.f90
)

target_link_libraries(nastran_parser
  PUBLIC nastran_common
)

# Output formatting
add_library(nastran_output
  output/exio_module.f90
  output/format_module.f90
)

target_link_libraries(nastran_output
  PUBLIC nastran_common
  PUBLIC nastran_system
)

# Helper utilities (sort, search, etc.)
add_library(nastran_helpers
  helpers/sort_module.f90
  helpers/search_module.f90
  helpers/sub1.f90
)

target_link_libraries(nastran_helpers
  PUBLIC nastran_common
)

# Profiling utilities (optional)
if(ENABLE_PROFILING)
  add_library(nastran_profiling
    profiling/timer_module.f90
    profiling/profile_module.f90
  )

  target_link_libraries(nastran_profiling
    PUBLIC nastran_common
  )
endif()

# Combined utilities library (convenience target)
add_library(nastran_utilities INTERFACE)
target_link_libraries(nastran_utilities INTERFACE
  nastran_parser
  nastran_output
  nastran_helpers
)

if(ENABLE_PROFILING)
  target_link_libraries(nastran_utilities INTERFACE
    nastran_profiling
  )
endif()

# Installation
install(TARGETS
  nastran_parser
  nastran_output
  nastran_helpers
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)

if(ENABLE_PROFILING)
  install(TARGETS nastran_profiling
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()
