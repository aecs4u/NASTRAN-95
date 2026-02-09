# ==============================================================================
# Elements - Finite Element Implementations
# ==============================================================================
# Element libraries: shell, beam, aero, solid
# Original source: mis/ directory (element-specific files)
# ==============================================================================

# Shell elements (QUAD4, SQUD42, IFS3)
add_library(nastran_elements_shell
  shell/quad4s.f90
  shell/quad4d.f90
  shell/squd42_module.f90
  shell/ifs3p.f90
)

target_link_libraries(nastran_elements_shell
  PUBLIC nastran_common
  PUBLIC nastran_core
)

if(ENABLE_OPENMP)
  target_link_libraries(nastran_elements_shell
    PUBLIC OpenMP::OpenMP_Fortran
  )
endif()

# Beam/Rod elements
add_library(nastran_elements_beam
  beam/rod.f90
  beam/beam.f90
  beam/bar.f90
)

target_link_libraries(nastran_elements_beam
  PUBLIC nastran_common
  PUBLIC nastran_core
)

# Aeroelastic elements (AMG, APD, AMP families)
add_library(nastran_elements_aero
  aero/aeroelastic_blade_module.f90
  aero/aeroelastic_panel_module.f90
  # Consolidated from amgb1*, amgt1*, apd*, amp* files
)

target_link_libraries(nastran_elements_aero
  PUBLIC nastran_common
  PUBLIC nastran_core
  PUBLIC nastran_algorithms
)

# Solid elements (HEX, TETRA, WEDGE)
add_library(nastran_elements_solid
  solid/hex.f90
  solid/tetra.f90
  solid/wedge.f90
)

target_link_libraries(nastran_elements_solid
  PUBLIC nastran_common
  PUBLIC nastran_core
)

# Combined elements library (convenience target)
add_library(nastran_elements INTERFACE)
target_link_libraries(nastran_elements INTERFACE
  nastran_elements_shell
  nastran_elements_beam
  nastran_elements_aero
  nastran_elements_solid
)

# Installation
install(TARGETS
  nastran_elements_shell
  nastran_elements_beam
  nastran_elements_aero
  nastran_elements_solid
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)
