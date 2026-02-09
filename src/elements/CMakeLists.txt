# ==============================================================================
# Elements - Finite Element Implementations
# ==============================================================================
# Element libraries: shell, beam, aero, solid
# Migrated from mis/ directory in Phase 3
# ==============================================================================

# Collect element files by type
file(GLOB BEAM_SOURCES beam/*.f)
file(GLOB SHELL_SOURCES shell/*.f)
file(GLOB SOLID_SOURCES solid/*.f)
file(GLOB AERO_SOURCES aero/*.f)

# Beam elements library (ROD, BAR - 58 files)
if(BEAM_SOURCES)
  add_library(nastran_elements_beam ${BEAM_SOURCES})

  target_link_libraries(nastran_elements_beam
    PUBLIC nastran_common
    PUBLIC nastran_matrices
  )

  list(LENGTH BEAM_SOURCES BEAM_COUNT)
  message(STATUS "Elements/Beam: Found ${BEAM_COUNT} files")
endif()

# Shell elements library (QUAD, TRIA - 129 files)
if(SHELL_SOURCES)
  add_library(nastran_elements_shell ${SHELL_SOURCES})

  target_link_libraries(nastran_elements_shell
    PUBLIC nastran_common
    PUBLIC nastran_matrices
  )

  if(ENABLE_OPENMP AND TARGET OpenMP::OpenMP_Fortran)
    target_link_libraries(nastran_elements_shell PUBLIC OpenMP::OpenMP_Fortran)
  endif()

  list(LENGTH SHELL_SOURCES SHELL_COUNT)
  message(STATUS "Elements/Shell: Found ${SHELL_COUNT} files")
endif()

# Solid elements library (IHEX, HEX - 18 files)
if(SOLID_SOURCES)
  add_library(nastran_elements_solid ${SOLID_SOURCES})

  target_link_libraries(nastran_elements_solid
    PUBLIC nastran_common
    PUBLIC nastran_matrices
  )

  if(ENABLE_OPENMP AND TARGET OpenMP::OpenMP_Fortran)
    target_link_libraries(nastran_elements_solid PUBLIC OpenMP::OpenMP_Fortran)
  endif()

  list(LENGTH SOLID_SOURCES SOLID_COUNT)
  message(STATUS "Elements/Solid: Found ${SOLID_COUNT} files")
endif()

# Aeroelastic elements library (AMG, APD, AMP - 57 files)
if(AERO_SOURCES)
  add_library(nastran_elements_aero ${AERO_SOURCES})

  target_link_libraries(nastran_elements_aero
    PUBLIC nastran_common
    PUBLIC nastran_matrices
    PUBLIC nastran_algorithms
  )

  list(LENGTH AERO_SOURCES AERO_COUNT)
  message(STATUS "Elements/Aero: Found ${AERO_COUNT} files")
endif()

# Combined elements library (convenience target)
add_library(nastran_elements INTERFACE)
target_link_libraries(nastran_elements INTERFACE
  $<$<TARGET_EXISTS:nastran_elements_beam>:nastran_elements_beam>
  $<$<TARGET_EXISTS:nastran_elements_shell>:nastran_elements_shell>
  $<$<TARGET_EXISTS:nastran_elements_solid>:nastran_elements_solid>
  $<$<TARGET_EXISTS:nastran_elements_aero>:nastran_elements_aero>
)

# Installation
if(TARGET nastran_elements_beam)
  install(TARGETS nastran_elements_beam
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()

if(TARGET nastran_elements_shell)
  install(TARGETS nastran_elements_shell
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()

if(TARGET nastran_elements_solid)
  install(TARGETS nastran_elements_solid
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()

if(TARGET nastran_elements_aero)
  install(TARGETS nastran_elements_aero
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
  )
endif()
