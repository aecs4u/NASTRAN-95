# ==============================================================================
# Executables - Main NASTRAN Program
# ==============================================================================
# Creates the main nastran executable linking all libraries
# ==============================================================================

# Main NASTRAN executable
add_executable(nastran
  ${CMAKE_SOURCE_DIR}/src/system/platform/nastrn.f90
)

# Link all NASTRAN libraries
target_link_libraries(nastran
  PRIVATE nastran_common
  PRIVATE nastran_system
  PRIVATE nastran_core
  PRIVATE nastran_elements
  PRIVATE nastran_solvers
  PRIVATE nastran_utilities
  PRIVATE nastran_blockdata
)

# Link external dependencies
if(USE_EXTERNAL_BLAS)
  target_link_libraries(nastran
    PRIVATE ${BLAS_LIBRARIES}
    PRIVATE ${LAPACK_LIBRARIES}
  )
endif()

if(ENABLE_OPENMP)
  target_link_libraries(nastran
    PRIVATE OpenMP::OpenMP_Fortran
  )
endif()

# Set executable properties
set_target_properties(nastran PROPERTIES
  OUTPUT_NAME nastran
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# Install executable
install(TARGETS nastran
  RUNTIME DESTINATION bin
)

# Install shell wrapper script
install(PROGRAMS
  ${CMAKE_SOURCE_DIR}/bin/nastran.sh
  DESTINATION bin
  RENAME nastran-wrapper
)
