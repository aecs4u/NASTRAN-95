!===============================================================================
! FILE: precision_macros.h
!
! PURPOSE:
!   Standard precision macro definitions for NASTRAN element template system.
!   Included by all element templates to enable single-source precision variants.
!
! USAGE:
!   ! In template file:
!   #include "precision_macros.h"
!
!   SUBROUTINE ELEMENT##PREC_SUFFIX
!   PREC_TYPE VAR1, VAR2
!   VAR1 = SQRT_FUNC(VAR2)
!   CALL TRANS_FUNC(...)
!   END SUBROUTINE
!
! BUILD:
!   # Double precision:
!   gfortran -E -DDOUBLE_PRECISION template.f90 -o element_d.f
!
!   # Single precision:
!   gfortran -E template.f90 -o element_s.f
!
! AUTHOR: Claude Code (2026-02-09)
!
!===============================================================================

#ifdef DOUBLE_PRECISION

!----- DOUBLE PRECISION CONFIGURATION -----

! Type declarations
#define PREC_TYPE DOUBLE PRECISION
#define PREC_SUFFIX D
#define PREC_KIND 8

! Math functions
#define SQRT_FUNC DSQRT
#define ABS_FUNC DABS
#define SIN_FUNC DSIN
#define COS_FUNC DCOS
#define ATAN_FUNC DATAN
#define ATAN2_FUNC DATAN2
#define EXP_FUNC DEXP
#define LOG_FUNC DLOG
#define MAX_FUNC DMAX1
#define MIN_FUNC DMIN1

! Matrix operations (NASTRAN convention)
#define TRANS_FUNC TRANSD
#define GMMAT_FUNC GMMATD
#define HMAT_FUNC HMATD
#define MPYA3_FUNC MPYA3D

! Type conversion
#define CAST_FUNC(X) DBLE(X)

! Numeric literals - Common values
#define ZERO 0.0D0
#define ONE 1.0D0
#define TWO 2.0D0
#define THREE 3.0D0
#define FOUR 4.0D0
#define FIVE 5.0D0
#define SIX 6.0D0
#define TWELVE 12.0D0
#define HALF 0.5D0
#define THIRD 0.333333333333333D0
#define QUARTER 0.25D0

! Engineering constants
#define PI 3.141592653589793D0
#define TWOPI 6.283185307179586D0

! Tolerances
#define TOLERANCE 1.0D-10
#define SMALL_TOL 1.0D-18
#define LARGE_TOL 1.0D-7

! Element-specific literals (TRIA3)
#define LITERAL_1E_M7 1.0D-7
#define LITERAL_5_6 0.833333333D0

! Element-specific literals (BAR mass matrix)
#define LITERAL_01 0.01D0
#define LITERAL_13 13.0D0
#define LITERAL_22 22.0D0
#define LITERAL_35 35.0D0
#define LITERAL_54 54.0D0
#define LITERAL_156 156.0D0
#define LITERAL_175 175.0D0
#define LITERAL_420 420.0D0

! Element-specific literals (QUAD4)
#define GAUSS_CONST 0.57735026918962D0
#define LITERAL_1_6 1.6D0
#define LITERAL_28 28.0D0
#define LITERAL_71 71.0D0
#define LITERAL_415 415.0D0

! Element-specific helper functions (QUAD4)
#define Q4SHP_FUNC Q4SHPD
#define Q4BMG_FUNC Q4BMGD
#define Q4GMG_FUNC Q4GMGD
#define Q4NRM_FUNC Q4NRMD
#define TRPLM_FUNC TRPLMD
#define TLDR_FUNC TLDRD
#define TERMS_FUNC TERMSD
#define ANGTR_FUNC ANGTRD

! Element-specific helper functions (IHEX)
#define IHEXS_FUNC IHEXSD

! Open core addressing (IHEX uses this)
#define CORE_ADJUST /2

#else

!----- SINGLE PRECISION CONFIGURATION -----

! Type declarations
#define PREC_TYPE REAL
#define PREC_SUFFIX
#define PREC_KIND 4

! Math functions
#define SQRT_FUNC SQRT
#define ABS_FUNC ABS
#define SIN_FUNC SIN
#define COS_FUNC COS
#define ATAN_FUNC ATAN
#define ATAN2_FUNC ATAN2
#define EXP_FUNC EXP
#define LOG_FUNC LOG
#define MAX_FUNC AMAX1
#define MIN_FUNC AMIN1

! Matrix operations (NASTRAN convention)
#define TRANS_FUNC TRANSS
#define GMMAT_FUNC GMMATS
#define HMAT_FUNC HMATS
#define MPYA3_FUNC MPYA3S

! Type conversion
#define CAST_FUNC(X) X

! Numeric literals - Common values
#define ZERO 0.0
#define ONE 1.0
#define TWO 2.0
#define THREE 3.0
#define FOUR 4.0
#define FIVE 5.0
#define SIX 6.0
#define TWELVE 12.0
#define HALF 0.5
#define THIRD 0.333333
#define QUARTER 0.25

! Engineering constants
#define PI 3.1415927
#define TWOPI 6.2831853

! Tolerances
#define TOLERANCE 1.0E-6
#define SMALL_TOL 1.0E-18
#define LARGE_TOL 1.0E-7

! Element-specific literals (TRIA3)
#define LITERAL_1E_M7 1.0E-7
#define LITERAL_5_6 0.833333333

! Element-specific literals (BAR mass matrix)
#define LITERAL_01 0.01
#define LITERAL_13 13.0
#define LITERAL_22 22.0
#define LITERAL_35 35.0
#define LITERAL_54 54.0
#define LITERAL_156 156.0
#define LITERAL_175 175.0
#define LITERAL_420 420.0

! Element-specific literals (QUAD4)
#define GAUSS_CONST 0.57735026918962
#define LITERAL_1_6 1.6
#define LITERAL_28 28.0
#define LITERAL_71 71.0
#define LITERAL_415 415.0

! Element-specific helper functions (QUAD4)
#define Q4SHP_FUNC Q4SHPS
#define Q4BMG_FUNC Q4BMGS
#define Q4GMG_FUNC Q4GMGS
#define Q4NRM_FUNC Q4NRMS
#define TRPLM_FUNC TRPLMS
#define TLDR_FUNC TLDRS
#define TERMS_FUNC TERMSS
#define ANGTR_FUNC ANGTRS

! Element-specific helper functions (IHEX)
#define IHEXS_FUNC IHEXSS

! Open core addressing (no adjustment for single precision)
#define CORE_ADJUST

#endif
