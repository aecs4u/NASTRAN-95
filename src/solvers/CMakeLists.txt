# ==============================================================================
# Solvers - Matrix Solution Algorithms
# ==============================================================================
# Direct, iterative, and eigenvalue solvers
# Original source: mis/ directory (solver-specific routines)
# ==============================================================================

# Direct solvers (LU factorization, forward/backward substitution)
add_library(nastran_solvers_direct
  direct/lu_factorization_module.f90
  direct/sparse_factorization.f90
  direct/forward_backward_sub.f90
)

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

# Iterative solvers (CG, GMRES, etc.)
add_library(nastran_solvers_iterative
  iterative/conjugate_gradient.f90
  iterative/gmres.f90
)

target_link_libraries(nastran_solvers_iterative
  PUBLIC nastran_common
  PUBLIC nastran_matrices
)

# Eigenvalue solvers (Lanczos, inverse power, etc.)
add_library(nastran_solvers_eigenvalue
  eigenvalue/lanczos.f90
  eigenvalue/inverse_power.f90
)

target_link_libraries(nastran_solvers_eigenvalue
  PUBLIC nastran_common
  PUBLIC nastran_matrices
  PUBLIC nastran_solvers_direct
)

if(USE_EXTERNAL_BLAS)
  target_link_libraries(nastran_solvers_eigenvalue
    PUBLIC ${LAPACK_LIBRARIES}
  )
endif()

# Combined solvers library (convenience target)
add_library(nastran_solvers INTERFACE)
target_link_libraries(nastran_solvers INTERFACE
  nastran_solvers_direct
  nastran_solvers_iterative
  nastran_solvers_eigenvalue
)

# Installation
install(TARGETS
  nastran_solvers_direct
  nastran_solvers_iterative
  nastran_solvers_eigenvalue
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)
