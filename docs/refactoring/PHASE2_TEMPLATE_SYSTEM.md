# Phase 2: Template-Based Consolidation System

## Executive Summary

Phase 2 implements **template-based single-source consolidation** for all precision-variant element pairs analyzed in Phase 1. This eliminates 4,951 lines (49%) of duplicate code while maintaining backward compatibility and numerical equivalence.

**Status**: Phase 2 - Design COMPLETE, Implementation READY
**Target Elements**: QUAD4, TRIA3, ROD, BAR, IHEX (5 families, 12 files)
**Total Savings**: 4,951 lines → ~2,450 lines (52% reduction)
**Implementation Time**: 6-8 weeks

---

## Template System Architecture

### Approach: Fortran Preprocessor (FPP)

Use standard Fortran preprocessor directives to generate precision-specific code from single source:

```fortran
! Template: rod_template.f90

#ifdef DOUBLE_PRECISION
#  define PREC_TYPE DOUBLE PRECISION
#  define PREC_SUFFIX D
#  define SQRT_FUNC DSQRT
#  define ABS_FUNC DABS
#  define TRANS_FUNC TRANSD
#  define GMMAT_FUNC GMMATD
#  define CAST_FUNC(X) DBLE(X)
#  define ZERO 0.0D0
#  define TWO 2.0D0
#else
#  define PREC_TYPE REAL
#  define PREC_SUFFIX
#  define SQRT_FUNC SQRT
#  define ABS_FUNC ABS
#  define TRANS_FUNC TRANSS
#  define GMMAT_FUNC GMMATS
#  define CAST_FUNC(X) X
#  define ZERO 0.0
#  define TWO 2.0
#endif

      SUBROUTINE ROD##PREC_SUFFIX
C     ROD ELEMENT - Generated from rod_template.f90

      PREC_TYPE EVECT(3), EL, KE, ME, TE

      EL = SQRT_FUNC(EVECT(1)**2 + EVECT(2)**2 + EVECT(3)**2)
      KE = CAST_FUNC(E*AFACT)/EL
      ME = (CAST_FUNC(RHO*AFACT+MU))*EL/TWO

      CALL TRANS_FUNC(...)
      CALL GMMAT_FUNC(...)

      END SUBROUTINE
```

### Build System Integration

**CMakeLists.txt**:
```cmake
# Generate double precision version
add_custom_command(
  OUTPUT rodd.f
  COMMAND ${CMAKE_Fortran_COMPILER} -E -DDOUBLE_PRECISION
          ${CMAKE_SOURCE_DIR}/templates/rod_template.f90
          -o ${CMAKE_BINARY_DIR}/generated/rodd.f
  DEPENDS templates/rod_template.f90
  COMMENT "Generating RODD from template"
)

# Generate single precision version
add_custom_command(
  OUTPUT rods.f
  COMMAND ${CMAKE_Fortran_COMPILER} -E
          ${CMAKE_SOURCE_DIR}/templates/rod_template.f90
          -o ${CMAKE_BINARY_DIR}/generated/rods.f
  DEPENDS templates/rod_template.f90
  COMMENT "Generating RODS from template"
)

add_library(nastran_elements_rod rodd.f rods.f)
```

---

## Template Macro System

### Standard Precision Macros

All templates use these standard macro definitions:

```fortran
! precision_macros.h - Included by all templates

#ifdef DOUBLE_PRECISION
#  define PREC_TYPE DOUBLE PRECISION
#  define PREC_SUFFIX D
#  define PREC_KIND 8
#
#  ! Math functions
#  define SQRT_FUNC DSQRT
#  define ABS_FUNC DABS
#  define SIN_FUNC DSIN
#  define COS_FUNC DCOS
#  define ATAN2_FUNC DATAN2
#  define EXP_FUNC DEXP
#  define LOG_FUNC DLOG
#
#  ! Matrix operations
#  define TRANS_FUNC TRANSD
#  define GMMAT_FUNC GMMATD
#  define HMAT_FUNC HMATD
#
#  ! Type conversion
#  define CAST_FUNC(X) DBLE(X)
#
#  ! Numeric literals
#  define ZERO 0.0D0
#  define ONE 1.0D0
#  define TWO 2.0D0
#  define THREE 3.0D0
#  define FOUR 4.0D0
#  define SIX 6.0D0
#  define HALF 0.5D0
#  define THIRD 0.333333333333333D0
#  define TOLERANCE 1.0D-10
#
#  ! Open core addressing
#  define CORE_ADJUST /2
#
#else  ! SINGLE_PRECISION
#  define PREC_TYPE REAL
#  define PREC_SUFFIX
#  define PREC_KIND 4
#
#  ! Math functions
#  define SQRT_FUNC SQRT
#  define ABS_FUNC ABS
#  define SIN_FUNC SIN
#  define COS_FUNC COS
#  define ATAN2_FUNC ATAN2
#  define EXP_FUNC EXP
#  define LOG_FUNC LOG
#
#  ! Matrix operations
#  define TRANS_FUNC TRANSS
#  define GMMAT_FUNC GMMATS
#  define HMAT_FUNC HMATS
#
#  ! Type conversion
#  define CAST_FUNC(X) X
#
#  ! Numeric literals
#  define ZERO 0.0
#  define ONE 1.0
#  define TWO 2.0
#  define THREE 3.0
#  define FOUR 4.0
#  define SIX 6.0
#  define HALF 0.5
#  define THIRD 0.333333
#  define TOLERANCE 1.0E-6
#
#  ! Open core addressing
#  define CORE_ADJUST
#
#endif
```

---

## Element-Specific Templates

### ROD Element Template

**File**: `templates/rod_template.f90`

**Complexity**: Simple (313 lines template → 313 lines generated each)

**Macro Usage**:
- 15 PREC_TYPE declarations
- 13 function call replacements (SQRT, TRANS, GMMAT)
- 2 CAST_FUNC conversions
- 12 numeric literal replacements

**Savings**: 313 lines (50% reduction from 626 total)

### TRIA3 Element Template

**File**: `templates/tria3_template.f90`

**Complexity**: Medium (545 lines template → 545 lines generated each)

**Macro Usage**:
- 25 PREC_TYPE declarations
- 40 function call replacements
- 10 CAST_FUNC conversions
- 30 numeric literal replacements

**Savings**: 545 lines (50% reduction from 1,094 total)

### BAR Element Template

**File**: `templates/bar_template.f90`

**Complexity**: Medium-High (908 lines template → 880-908 lines generated)

**Special Handling**:
- Loop optimization in BARS (lines 102-105) requires conditional generation
- Comment attribution difference (SPERRY → UNISYS)

```fortran
#ifdef DOUBLE_PRECISION
C     G.CHAN/SPERRY, 1984
      FL = ZERO
      DO 40 I = 1,3
      SMALLV(I) = SMALV(I)
   40 FL = FL + SMALLV(I)**2
      FL = SQRT_FUNC(FL)
#else
C     G.CHAN/UNISYS, 1984
      FL = SQRT_FUNC(SMALLV(1)**2 + SMALLV(2)**2 + SMALLV(3)**2)
#endif
```

**Savings**: 880 lines (49% reduction from 1,788 total)

### QUAD4 Element Template

**File**: `templates/quad4_template.f90`

**Complexity**: High (1,718 lines template → 1,713-1,718 lines generated)

**Macro Usage**:
- 50+ PREC_TYPE declarations
- 80+ function call replacements
- 30+ CAST_FUNC conversions
- 60+ numeric literal replacements

**Savings**: 1,621 lines (47% reduction from 3,431 total)

### IHEX Element Template (Two-Level)

**File**: `templates/ihex_main_template.f90` (1,395 lines)
**File**: `templates/ihex_helper_template.f90` (241 lines)

**Complexity**: Very High (largest consolidation)

**Special Handling**:
- Open core pointer adjustment (CORE_ADJUST macro)
- Two separate templates (main + helper)
- Coordinated generation

```fortran
! ihex_main_template.f90
IZ = IZS CORE_ADJUST + 1
NZ = NZS CORE_ADJUST + 1

CALL IHEXS##PREC_SUFFIX(IERR, ITYPE, IPT, KPT, DETJ, SHP, DSHP, BXYZ)
```

**Savings**: 1,816 lines (56% reduction from 3,266 total)

---

## Validation Strategy

### Automated Verification

**1. Diff-Based Verification** (Character-level comparison):

```bash
#!/bin/bash
# verify_generated.sh

# Generate from template
make generated/rodd.f

# Compare with original
diff -u mis/rodd.f generated/rodd.f > rodd_diff.txt

# Check for differences (should be minimal formatting only)
if [ -s rodd_diff.txt ]; then
  echo "WARNING: Differences found in generated RODD"
  cat rodd_diff.txt
else
  echo "✓ RODD generated correctly"
fi
```

**2. Numerical Validation**:

```bash
# Run same problem with original and generated code
./nastran_original example.inp > original.out
./nastran_generated example.inp > generated.out

# Compare results (10+ digit precision)
python3 compare_results.py original.out generated.out --tolerance 1e-10
```

**3. Regression Testing**:

```bash
# Test all 55 examples using consolidated elements
for example in $(find examples/input -name "*.inp" | head -55); do
  ./nastran_generated $example
  compare_output $example reference_output/
done
```

### Manual Review Checklist

For each template:

- [  ] All PREC_TYPE declarations converted
- [  ] All function calls use macros
- [  ] All numeric literals use macros
- [  ] All CAST_FUNC conversions applied
- [  ] Open core addressing correct (IHEX only)
- [  ] Generated code compiles without warnings
- [  ] Generated code matches original (diff check)
- [  ] Numerical results match (10+ digits)
- [  ] Comments preserved and updated
- [  ] Special cases handled (loop optimization in BAR)

---

## Implementation Schedule

### Week 1-2: Infrastructure

- [  ] Create `templates/` directory structure
- [  ] Implement `precision_macros.h`
- [  ] Update CMakeLists.txt for template generation
- [  ] Create verification scripts
- [  ] Set up automated testing

### Week 3: ROD Template (Pilot)

- [  ] Create rod_template.f90
- [  ] Generate rodd.f and rods.f
- [  ] Verify character-level match
- [  ] Run 13 ROD examples
- [  ] Document lessons learned

### Week 4: TRIA3 and BAR Templates

- [  ] Create tria3_template.f90
- [  ] Handle TRIA3D bugfix in template
- [  ] Create bar_template.f90
- [  ] Handle loop optimization in BAR template
- [  ] Verify both templates
- [  ] Run 26 examples (2 TRIA3 + 24 BAR)

### Week 5-6: QUAD4 Template

- [  ] Create quad4_template.f90 (largest file)
- [  ] Handle 50+ precision conversions
- [  ] Verify template
- [  ] Run 9 QUAD4 examples
- [  ] Performance profiling

### Week 7-8: IHEX Templates (Two-Level)

- [  ] Create ihex_main_template.f90
- [  ] Create ihex_helper_template.f90
- [  ] Coordinate generation (helper called by main)
- [  ] Handle open core addressing
- [  ] Verify both templates
- [  ] Run 7 IHEX examples

### Week 8: Final Integration

- [  ] Full regression suite (all 55 examples)
- [  ] Performance comparison
- [  ] Documentation update
- [  ] Remove Phase 1 wrappers (optional)
- [  ] Release Phase 2

---

## Metrics and Success Criteria

### Code Reduction Metrics

| Family | Original | Template | Generated | Savings | Reduction % |
|--------|----------|----------|-----------|---------|-------------|
| ROD | 626 | 313 | 626 (2×313) | 313 | 50% |
| TRIA3 | 1,094 | 545 | 1,090 (2×545) | 549 | 50% |
| BAR | 1,788 | 900 | 1,788 (908+880) | 888 | 50% |
| QUAD4 | 3,431 | 1,715 | 3,431 (1,718+1,713) | 1,716 | 50% |
| IHEX | 3,266 | 1,636 | 3,266 (2,787+479) | 1,630 | 50% |
| **Total** | **10,205** | **5,109** | **10,201** | **5,096** | **50%** |

### Success Criteria

**Functional Requirements**:
- ✅ Generated code compiles without errors
- ✅ Generated code matches original (character-level, modulo formatting)
- ✅ All 55 regression tests pass
- ✅ Numerical results match original (10+ digits)

**Performance Requirements**:
- ✅ Generated code performance within 5% of original
- ✅ Build time reduced (parallel generation)
- ✅ No runtime overhead

**Maintenance Requirements**:
- ✅ Single source of truth for each element
- ✅ Bug fixes apply to both precisions automatically
- ✅ Clear documentation of template system
- ✅ Automated verification integrated in CI

---

## Risk Assessment and Mitigation

### Risk 1: Generated Code Differs from Original

**Likelihood**: Medium
**Impact**: High (breaks numerical equivalence)

**Mitigation**:
- Character-level diff verification
- Extensive regression testing
- Numerical comparison (10+ digits)
- Pilot with simplest element (ROD)

### Risk 2: Build System Complexity

**Likelihood**: Low
**Impact**: Medium (harder to maintain)

**Mitigation**:
- Use standard CMake custom commands
- Clear documentation of generation process
- Automated verification scripts
- Fallback to original files if generation fails

### Risk 3: Template Maintenance Burden

**Likelihood**: Low
**Impact**: Low (offset by consolidation benefits)

**Mitigation**:
- Comprehensive macro documentation
- Example templates for each pattern
- Automated testing catches template errors
- Net reduction: 50% fewer lines to maintain

### Risk 4: Preprocessor Limitations

**Likelihood**: Very Low
**Impact**: Medium (may need alternative approach)

**Mitigation**:
- Standard Fortran preprocessor widely supported
- Macro system tested on pilot (ROD)
- Alternative: Code generation scripts (Python)
- Fallback: Keep Phase 1 wrappers

---

## Alternative Approaches Considered

### 1. Python Code Generation

**Pros**:
- More flexible than preprocessor
- Easier string manipulation
- Better error messages

**Cons**:
- Additional dependency
- More complex build system
- Not standard Fortran

**Decision**: Use for Phase 3 if needed

### 2. Fortran Generics (Fortran 2003)

**Pros**:
- Native language support
- Type-safe

**Cons**:
- Requires full F2003 rewrite
- Large effort
- Not backward compatible

**Decision**: Reserve for Phase 3

### 3. Keep Phase 1 Wrappers Only

**Pros**:
- Zero risk
- Minimal effort

**Cons**:
- Doesn't eliminate duplication
- Still maintain two copies
- Misses 50% code reduction

**Decision**: Not recommended, Phase 2 provides significant value

---

## Next Steps

1. ✅ Phase 1 complete (5 families, 12 files documented)
2. 🔄 **CURRENT**: Create ROD pilot template
3. ⏳ Implement full template system (6-8 weeks)
4. ⏳ Full regression testing
5. ⏳ Phase 3 planning (modern Fortran 2003)

---

## References

- **Phase 1 Analysis**:
  - QUAD4_CONSOLIDATION.md
  - TRIA3_CONSOLIDATION.md
  - ROD_CONSOLIDATION.md
  - BAR_CONSOLIDATION.md
  - IHEX_CONSOLIDATION.md

- **Build Systems**:
  - CMake Documentation: https://cmake.org/documentation/
  - Fortran Preprocessor: gfortran -E documentation

- **Testing**:
  - NASTRAN Example Problems: examples/input/ (132 files)

---

**Document Status**: Phase 2 design complete, ready for implementation
**Last Updated**: 2026-02-09
**Next Review**: After ROD pilot template complete
