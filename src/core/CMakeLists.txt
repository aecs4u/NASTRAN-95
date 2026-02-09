# ==============================================================================
# Core - Algorithms, Matrices, Solvers
# ==============================================================================
# Core computational algorithms and utilities
# Migrated from mis/ directory in Phase 3
# ==============================================================================

# Collect algorithm files (ALG family)
file(GLOB ALG_SOURCES algorithms/*.f)

# Collect matrix operation files (MMA family)
file(GLOB MATRIX_SOURCES matrices/*.f)

# Collect solver files
file(GLOB SOLVER_SOURCES solvers/*.f)

# Algorithms library (ALG family - 39 files)
if(ALG_SOURCES)
  add_library(nastran_algorithms ${ALG_SOURCES})

  target_link_libraries(nastran_algorithms
    PUBLIC nastran_common
  )

  message(STATUS "Core/Algorithms: Found ${CMAKE_MATCH_COUNT} files")
endif()

# Matrix operations library (MMA family - 49 files)
if(MATRIX_SOURCES)
  add_library(nastran_matrices ${MATRIX_SOURCES})

  target_link_libraries(nastran_matrices
    PUBLIC nastran_common
  )

  if(USE_EXTERNAL_BLAS)
    target_link_libraries(nastran_matrices
      PUBLIC ${BLAS_LIBRARIES}
      PUBLIC ${LAPACK_LIBRARIES}
    )
  endif()

  message(STATUS "Core/Matrices: Found ${CMAKE_MATCH_COUNT} files")
endif()

# Core solvers library (41 files - legacy routines from mis/)
if(SOLVER_SOURCES)
  add_library(nastran_core_solvers ${SOLVER_SOURCES})

  target_link_libraries(nastran_core_solvers
    PUBLIC nastran_common
    PUBLIC nastran_matrices
  )

  if(USE_EXTERNAL_BLAS)
    target_link_libraries(nastran_core_solvers
      PUBLIC ${LAPACK_LIBRARIES}
    )
  endif()

  message(STATUS "Core/Solvers: Found ${CMAKE_MATCH_COUNT} files")
endif()

# Combined core library (convenience target)
add_library(nastran_core INTERFACE)
target_link_libraries(nastran_core INTERFACE
  $<$<TARGET_EXISTS:nastran_algorithms>:nastran_algorithms>
  $<$<TARGET_EXISTS:nastran_matrices>:nastran_matrices>
  $<$<TARGET_EXISTS:nastran_core_solvers>:nastran_core_solvers>
)

# Installation
if(TARGET nastran_algorithms)
  install(TARGETS nastran_algorithms
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()

if(TARGET nastran_matrices)
  install(TARGETS nastran_matrices
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()

if(TARGET nastran_core_solvers)
  install(TARGETS nastran_core_solvers
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()
