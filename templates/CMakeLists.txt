# CMakeLists.txt for template-based element generation
#
# This file demonstrates how to generate precision-specific element
# implementations from single-source templates using the Fortran preprocessor.
#
# Usage:
#   mkdir build && cd build
#   cmake ..
#   make
#
# Generated files will be in: build/generated/

cmake_minimum_required(VERSION 3.15)
project(NASTRAN_Templates Fortran)

# Output directory for generated files
set(GENERATED_DIR ${CMAKE_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${GENERATED_DIR})

# Template source directory
set(TEMPLATE_DIR ${CMAKE_SOURCE_DIR}/templates)

#===============================================================================
# Function: generate_precision_variant
#
# Generates a precision-specific source file from a template.
#
# Arguments:
#   TEMPLATE - Template source file (e.g., rod_template.f90)
#   OUTPUT   - Generated output file (e.g., rodd.f)
#   PRECISION - Either "DOUBLE" or "SINGLE"
#===============================================================================
function(generate_precision_variant TEMPLATE OUTPUT PRECISION)
  get_filename_component(TEMPLATE_NAME ${TEMPLATE} NAME)
  get_filename_component(OUTPUT_NAME ${OUTPUT} NAME)

  if(PRECISION STREQUAL "DOUBLE")
    set(PRECISION_FLAG "-DDOUBLE_PRECISION")
    set(VARIANT_NAME "double precision")
  else()
    set(PRECISION_FLAG "")
    set(VARIANT_NAME "single precision")
  endif()

  add_custom_command(
    OUTPUT ${OUTPUT}
    COMMAND ${CMAKE_Fortran_COMPILER} -E ${PRECISION_FLAG}
            -I${TEMPLATE_DIR}
            ${TEMPLATE}
            -o ${OUTPUT}
    DEPENDS ${TEMPLATE} ${TEMPLATE_DIR}/precision_macros.h
    COMMENT "Generating ${OUTPUT_NAME} (${VARIANT_NAME}) from ${TEMPLATE_NAME}"
    VERBATIM
  )
endfunction()

#===============================================================================
# ROD Element Template Generation
#===============================================================================

generate_precision_variant(
  ${TEMPLATE_DIR}/rod_template.f90
  ${GENERATED_DIR}/rodd.f
  DOUBLE
)

generate_precision_variant(
  ${TEMPLATE_DIR}/rod_template.f90
  ${GENERATED_DIR}/rods.f
  SINGLE
)

# Create custom target to build ROD variants
add_custom_target(generate_rod_elements
  DEPENDS ${GENERATED_DIR}/rodd.f ${GENERATED_DIR}/rods.f
  COMMENT "Generating ROD element precision variants"
)

#===============================================================================
# TRIA3 Element Template Generation
#===============================================================================

generate_precision_variant(
  ${TEMPLATE_DIR}/tria3_template.f90
  ${GENERATED_DIR}/tria3d.f
  DOUBLE
)

generate_precision_variant(
  ${TEMPLATE_DIR}/tria3_template.f90
  ${GENERATED_DIR}/tria3s.f
  SINGLE
)

# Create custom target to build TRIA3 variants
add_custom_target(generate_tria3_elements
  DEPENDS ${GENERATED_DIR}/tria3d.f ${GENERATED_DIR}/tria3s.f
  COMMENT "Generating TRIA3 element precision variants"
)

#===============================================================================
# BAR Element Template Generation
#===============================================================================

generate_precision_variant(
  ${TEMPLATE_DIR}/bar_template.f90
  ${GENERATED_DIR}/bard.f
  DOUBLE
)

generate_precision_variant(
  ${TEMPLATE_DIR}/bar_template.f90
  ${GENERATED_DIR}/bars.f
  SINGLE
)

# Create custom target to build BAR variants
add_custom_target(generate_bar_elements
  DEPENDS ${GENERATED_DIR}/bard.f ${GENERATED_DIR}/bars.f
  COMMENT "Generating BAR element precision variants"
)

#===============================================================================
# QUAD4 Element Template Generation (Largest single file: 1,718 lines)
#===============================================================================

generate_precision_variant(
  ${TEMPLATE_DIR}/quad4_template.f90
  ${GENERATED_DIR}/quad4d.f
  DOUBLE
)

generate_precision_variant(
  ${TEMPLATE_DIR}/quad4_template.f90
  ${GENERATED_DIR}/quad4s.f
  SINGLE
)

# Create custom target to build QUAD4 variants
add_custom_target(generate_quad4_elements
  DEPENDS ${GENERATED_DIR}/quad4d.f ${GENERATED_DIR}/quad4s.f
  COMMENT "Generating QUAD4 element precision variants"
)

#===============================================================================
# Future Template Generations (Commented Out - Templates Not Yet Created)
#===============================================================================

# # IHEX Main Element
# generate_precision_variant(
#   ${TEMPLATE_DIR}/ihex_main_template.f90
#   ${GENERATED_DIR}/ihexd.f
#   DOUBLE
# )
# generate_precision_variant(
#   ${TEMPLATE_DIR}/ihex_main_template.f90
#   ${GENERATED_DIR}/ihexs.f
#   SINGLE
# )

# # IHEX Helper
# generate_precision_variant(
#   ${TEMPLATE_DIR}/ihex_helper_template.f90
#   ${GENERATED_DIR}/ihexsd.f
#   DOUBLE
# )
# generate_precision_variant(
#   ${TEMPLATE_DIR}/ihex_helper_template.f90
#   ${GENERATED_DIR}/ihexss.f
#   SINGLE
# )

#===============================================================================
# Verification Target
#
# Compares generated files with original legacy files to ensure correctness.
#===============================================================================

add_custom_target(verify_templates
  COMMAND ${CMAKE_COMMAND} -E echo "Verifying generated files..."
  COMMAND bash ${CMAKE_SOURCE_DIR}/scripts/verify_generated.sh
  DEPENDS generate_rod_elements generate_tria3_elements generate_bar_elements generate_quad4_elements
  COMMENT "Verifying template-generated files match originals"
)

#===============================================================================
# Clean Target
#
# Removes all generated files.
#===============================================================================

add_custom_target(clean_generated
  COMMAND ${CMAKE_COMMAND} -E remove_directory ${GENERATED_DIR}
  COMMAND ${CMAKE_COMMAND} -E make_directory ${GENERATED_DIR}
  COMMENT "Cleaning generated files"
)

#===============================================================================
# Print Configuration
#===============================================================================

message(STATUS "Template configuration:")
message(STATUS "  Template directory: ${TEMPLATE_DIR}")
message(STATUS "  Generated directory: ${GENERATED_DIR}")
message(STATUS "  Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "")
message(STATUS "Targets:")
message(STATUS "  make generate_rod_elements   - Generate ROD precision variants")
message(STATUS "  make generate_tria3_elements - Generate TRIA3 precision variants")
message(STATUS "  make generate_bar_elements   - Generate BAR precision variants")
message(STATUS "  make generate_quad4_elements - Generate QUAD4 precision variants")
message(STATUS "  make verify_templates        - Verify generated vs original")
message(STATUS "  make clean_generated         - Remove all generated files")
