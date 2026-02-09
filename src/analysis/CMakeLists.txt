# ==============================================================================
# Analysis - Static, Dynamic, Modal Analysis Modules
# ==============================================================================
# Migrated from mis/ directory in Phase 3
# Analysis driver modules for different solution sequences
# ==============================================================================

# Collect analysis files by type
file(GLOB STATIC_SOURCES static/*.f)
file(GLOB DYNAMIC_SOURCES dynamic/*.f)
file(GLOB MODAL_SOURCES modal/*.f)

# Static analysis library (GP, TA, SMA - 89 files)
if(STATIC_SOURCES)
  add_library(nastran_analysis_static ${STATIC_SOURCES})

  target_link_libraries(nastran_analysis_static
    PUBLIC nastran_common
    PUBLIC nastran_core
    PUBLIC nastran_elements
    PUBLIC nastran_system
  )

  list(LENGTH STATIC_SOURCES STATIC_COUNT)
  message(STATUS "Analysis/Static: Found ${STATIC_COUNT} files")
endif()

# Dynamic/transient analysis library (8 files)
if(DYNAMIC_SOURCES)
  add_library(nastran_analysis_dynamic ${DYNAMIC_SOURCES})

  target_link_libraries(nastran_analysis_dynamic
    PUBLIC nastran_common
    PUBLIC nastran_core
    PUBLIC nastran_system
  )

  list(LENGTH DYNAMIC_SOURCES DYNAMIC_COUNT)
  message(STATUS "Analysis/Dynamic: Found ${DYNAMIC_COUNT} files")
endif()

# Modal/eigenvalue analysis library (76 files)
if(MODAL_SOURCES)
  add_library(nastran_analysis_modal ${MODAL_SOURCES})

  target_link_libraries(nastran_analysis_modal
    PUBLIC nastran_common
    PUBLIC nastran_core
    PUBLIC nastran_solvers
    PUBLIC nastran_system
  )

  if(USE_EXTERNAL_BLAS)
    target_link_libraries(nastran_analysis_modal
      PUBLIC ${LAPACK_LIBRARIES}
    )
  endif()

  list(LENGTH MODAL_SOURCES MODAL_COUNT)
  message(STATUS "Analysis/Modal: Found ${MODAL_COUNT} files")
endif()

# Combined analysis library (convenience target)
add_library(nastran_analysis INTERFACE)
target_link_libraries(nastran_analysis INTERFACE
  $<$<TARGET_EXISTS:nastran_analysis_static>:nastran_analysis_static>
  $<$<TARGET_EXISTS:nastran_analysis_dynamic>:nastran_analysis_dynamic>
  $<$<TARGET_EXISTS:nastran_analysis_modal>:nastran_analysis_modal>
)

# Installation
if(TARGET nastran_analysis_static)
  install(TARGETS nastran_analysis_static
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()

if(TARGET nastran_analysis_dynamic)
  install(TARGETS nastran_analysis_dynamic
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()

if(TARGET nastran_analysis_modal)
  install(TARGETS nastran_analysis_modal
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()
