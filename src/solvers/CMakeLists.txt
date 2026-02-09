# ==============================================================================
# Solvers - Matrix Solution Algorithms
# ==============================================================================
# Direct, iterative, and eigenvalue solvers
# Original source: mis/ directory (solver-specific routines)
# ==============================================================================

# Collect solver files by type
file(GLOB DIRECT_SOURCES direct/*.f)
file(GLOB ITERATIVE_SOURCES iterative/*.f)
file(GLOB EIGENVALUE_SOURCES eigenvalue/*.f)
file(GLOB BACKEND_SOURCES backends/*.f backends/*.f90)

# Direct solvers (LU factorization, forward/backward substitution)
if(DIRECT_SOURCES)
  add_library(nastran_solvers_direct ${DIRECT_SOURCES})

  target_link_libraries(nastran_solvers_direct
    PUBLIC nastran_common
    PUBLIC nastran_matrices
  )

  if(USE_EXTERNAL_BLAS)
    target_link_libraries(nastran_solvers_direct
      PUBLIC ${BLAS_LIBRARIES}
      PUBLIC ${LAPACK_LIBRARIES}
    )
  endif()

  list(LENGTH DIRECT_SOURCES DIRECT_COUNT)
  message(STATUS "Solvers/Direct: Found ${DIRECT_COUNT} files")
endif()

# Iterative solvers (CG, GMRES, etc.)
if(ITERATIVE_SOURCES)
  add_library(nastran_solvers_iterative ${ITERATIVE_SOURCES})

  target_link_libraries(nastran_solvers_iterative
    PUBLIC nastran_common
    PUBLIC nastran_matrices
  )

  list(LENGTH ITERATIVE_SOURCES ITERATIVE_COUNT)
  message(STATUS "Solvers/Iterative: Found ${ITERATIVE_COUNT} files")
endif()

# Eigenvalue solvers (Lanczos, inverse power, etc.)
if(EIGENVALUE_SOURCES)
  add_library(nastran_solvers_eigenvalue ${EIGENVALUE_SOURCES})

  target_link_libraries(nastran_solvers_eigenvalue
    PUBLIC nastran_common
    PUBLIC nastran_matrices
  )

  if(TARGET nastran_solvers_direct)
    target_link_libraries(nastran_solvers_eigenvalue PUBLIC nastran_solvers_direct)
  endif()

  if(USE_EXTERNAL_BLAS)
    target_link_libraries(nastran_solvers_eigenvalue PUBLIC ${LAPACK_LIBRARIES})
  endif()

  list(LENGTH EIGENVALUE_SOURCES EIGEN_COUNT)
  message(STATUS "Solvers/Eigenvalue: Found ${EIGEN_COUNT} files")
endif()

# Combined solvers library (convenience target)
add_library(nastran_solvers INTERFACE)
target_link_libraries(nastran_solvers INTERFACE
  $<$<TARGET_EXISTS:nastran_solvers_direct>:nastran_solvers_direct>
  $<$<TARGET_EXISTS:nastran_solvers_iterative>:nastran_solvers_iterative>
  $<$<TARGET_EXISTS:nastran_solvers_eigenvalue>:nastran_solvers_eigenvalue>
)

# Backend adapters (MUMPS, GPU, etc.)
if(BACKEND_SOURCES)
  add_library(nastran_solvers_backends ${BACKEND_SOURCES})

  target_link_libraries(nastran_solvers_backends
    PUBLIC nastran_common
  )

  if(USE_MUMPS)
    target_link_libraries(nastran_solvers_backends
      PUBLIC MUMPS::MUMPS
    )
  endif()

  target_link_libraries(nastran_solvers INTERFACE
    nastran_solvers_backends
  )

  list(LENGTH BACKEND_SOURCES BACKEND_COUNT)
  message(STATUS "Solvers/Backends: Found ${BACKEND_COUNT} files")
endif()

# Installation
if(TARGET nastran_solvers_direct)
  install(TARGETS nastran_solvers_direct ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
endif()

if(TARGET nastran_solvers_iterative)
  install(TARGETS nastran_solvers_iterative ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
endif()

if(TARGET nastran_solvers_eigenvalue)
  install(TARGETS nastran_solvers_eigenvalue ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
endif()

if(TARGET nastran_solvers_backends)
  install(TARGETS nastran_solvers_backends ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
endif()
