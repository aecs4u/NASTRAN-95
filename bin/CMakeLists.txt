# ==============================================================================
# Executables - Main NASTRAN Program
# ==============================================================================
# Creates the main nastran executable linking all libraries
# ==============================================================================

# Add Rust parser bridge library directory
set(RUST_BRIDGE_DIR "${CMAKE_SOURCE_DIR}/nastran_parser_bridge/target/release")
set(USE_RUST_BRIDGE OFF)

if(EXISTS "${RUST_BRIDGE_DIR}/libnastran_parser_bridge.so")
  message(STATUS "✓ Rust parser bridge found: ${RUST_BRIDGE_DIR}")
  set(USE_RUST_BRIDGE ON)
  link_directories(${RUST_BRIDGE_DIR})

  # Find Python library manually (for venv compatibility)
  find_library(PYTHON3_LIBRARY NAMES python3.12 python3 PATHS
    /usr/lib/x86_64-linux-gnu
    /usr/lib/python3.12/config-3.12-x86_64-linux-gnu
    NO_DEFAULT_PATH
  )
  if(PYTHON3_LIBRARY)
    message(STATUS "✓ Python3 library found: ${PYTHON3_LIBRARY}")
  else()
    message(STATUS "⚠ Python3 library not found - using default")
  endif()
else()
  message(WARNING "⚠ Rust parser bridge not built - run: cd nastran_parser_bridge && ./build.sh")
endif()

# Main NASTRAN executable
add_executable(nastran
  ${CMAKE_SOURCE_DIR}/src/system/platform/nastrn.f
  ${CMAKE_SOURCE_DIR}/src/utilities/parser/rust_bridge_interface.f90
)

# Link all NASTRAN libraries
# Use linker group to resolve circular dependencies between Fortran libraries
# (Fortran programs commonly have cross-library call dependencies)
target_link_libraries(nastran
  PRIVATE -Wl,--start-group
  PRIVATE nastran_common
  PRIVATE nastran_system
  PRIVATE nastran_core
  PRIVATE nastran_elements
  PRIVATE nastran_solvers
  PRIVATE nastran_parser
  PRIVATE nastran_output
  PRIVATE nastran_helpers
  PRIVATE nastran_blockdata
  PRIVATE nastran_legacy
  PRIVATE nastran_analysis
  PRIVATE -Wl,--end-group
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

# Link Rust parser bridge and Python
if(USE_RUST_BRIDGE)
  target_link_libraries(nastran
    PRIVATE nastran_parser_bridge
  )
  if(PYTHON3_LIBRARY)
    target_link_libraries(nastran
      PRIVATE ${PYTHON3_LIBRARY}
      PRIVATE ${CMAKE_DL_LIBS}
      PRIVATE pthread
      PRIVATE util
      PRIVATE m
    )
  endif()
  message(STATUS "✓ Rust parser bridge will be linked to nastran executable")
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
