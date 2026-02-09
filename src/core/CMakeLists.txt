# ==============================================================================
# Core - Algorithms, Matrices, Analysis Drivers
# ==============================================================================
# Core computational algorithms from original mis/ directory
# Includes ALG family, MMA family, and analysis drivers
# ==============================================================================

# Algorithms library (ALG family - consolidated)
add_library(nastran_algorithms
  algorithms/spline_module.f90
  algorithms/pressure_iteration_module.f90
  algorithms/streamline_module.f90
  # Additional algorithm modules as they are refactored
)

target_link_libraries(nastran_algorithms
  PUBLIC nastran_common
)

# Matrix operations library (MMA family - consolidated)
add_library(nastran_matrices
  matrices/matrix_module.f90
  matrices/sparse_matrix_module.f90
  # Additional matrix modules as they are refactored
)

target_link_libraries(nastran_matrices
  PUBLIC nastran_common
)

if(USE_EXTERNAL_BLAS)
  target_link_libraries(nastran_matrices
    PUBLIC ${BLAS_LIBRARIES}
    PUBLIC ${LAPACK_LIBRARIES}
  )
endif()

# Analysis drivers
add_library(nastran_analysis
  analysis/asdmap.f90
  analysis/dbase.f90
)

target_link_libraries(nastran_analysis
  PUBLIC nastran_common
  PUBLIC nastran_system
  PUBLIC nastran_algorithms
  PUBLIC nastran_matrices
)

# Combined core library (convenience target)
add_library(nastran_core INTERFACE)
target_link_libraries(nastran_core INTERFACE
  nastran_algorithms
  nastran_matrices
  nastran_analysis
)

# Installation
install(TARGETS
  nastran_algorithms
  nastran_matrices
  nastran_analysis
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)
