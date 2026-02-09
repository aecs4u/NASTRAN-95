#!/bin/bash
# Script to create IHEX templates from original files

cd /mnt/developer/git/aecs4u.it/calculix.BAK/NASTRAN-95

# Create IHEX main template from ihexd.f
echo "Creating ihex_template.f90..."
cat > templates/ihex_template.f90 << 'TEMPLATE_HEADER'
!===============================================================================
! TEMPLATE: ihex_template.f90
!
! PURPOSE:
!   Single-source template for IHEX (hexahedral solid) element
!   Generates both double and single precision variants via preprocessor
!
! GENERATES:
!   - ihexd.f (DOUBLE PRECISION) with -DDOUBLE_PRECISION flag
!   - ihexs.f (REAL single precision) without flag
!
! ORIGINAL FILES:
!   - mis/ihexd.f (1,395 lines)
!   - mis/ihexs.f (1,392 lines)
!   - Total: 2,787 lines → ~1,395 lines template (50% reduction)
!
! ELEMENT DESCRIPTION:
!   IHEX - Isoparametric hexahedral solid element
!   - 3 types: IHEX1 (8-node), IHEX2 (20-node), IHEX3 (32-node)
!   - 3 DOF per node (translations only)
!   - Supports isotropic and anisotropic materials
!   - Gaussian integration for stiffness and mass
!   - Heat transfer capability
!   - Differential stiffness for buckling
!
! BUILD COMMANDS:
!   # Double precision:
!   gfortran -cpp -E -DDOUBLE_PRECISION -I. ihex_template.f90 -o ihexd.f
!
!   # Single precision:
!   gfortran -cpp -E -I. ihex_template.f90 -o ihexs.f
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! TEMPLATE: Claude Code (2026-02-09)
!
!===============================================================================

#include "precision_macros.h"

      SUBROUTINE IHEX##PREC_SUFFIX (TYPE)
C
#ifdef DOUBLE_PRECISION
C     DOUBLE PRECISION VERSION
#else
C     SINGLE PRECISION VERSION
#endif
TEMPLATE_HEADER

# Process ihexd.f with replacements
sed '1,3d' mis/ihexd.f | sed '
# Skip subroutine line (already in header)
/^      SUBROUTINE IHEXD/d
# Replace type declarations
s/DOUBLE PRECISION/PREC_TYPE/g
# Replace matrix operations  
s/GMMATD/GMMAT_FUNC/g
s/TRANSD/TRANS_FUNC/g
# Replace helper function call
s/IHEXSD/IHEXS_FUNC/g
# Replace type conversions
s/DBLE(\([^)]*\))/CAST_FUNC(\1)/g
# Replace JCORE/IPREC pattern
s|IZS  += JCORE/IPREC|IZS  = JCORE CORE_ADJUST|
s|NZS  += NCORE/IPREC|NZS  = NCORE CORE_ADJUST|
# Replace common numeric literals
s/\([^.0-9]\)0\.0D0/\1ZERO/g
s/\([^.0-9]\)1\.0D0/\1ONE/g
s/\([^.0-9]\)2\.0D0/\1TWO/g
s/\([^.0-9]\)3\.0D0/\1THREE/g
s/\([^.0-9]\)6\.0D0/\1SIX/g
s/\([^.0-9]\)12\.0D0/\1TWELVE/g
# Replace specific literals at start of line
s/^      0\.0D0/      ZERO/
s/^      1\.0D0/      ONE/
' >> templates/ihex_template.f90

echo "      END" >> templates/ihex_template.f90

echo "IHEX main template created: $(wc -l < templates/ihex_template.f90) lines"

# Create IHEX helper template from ihexsd.f
echo "Creating ihexs_template.f90..."
cat > templates/ihexs_template.f90 << 'HELPER_HEADER'
!===============================================================================
! TEMPLATE: ihexs_template.f90
!
! PURPOSE:
!   Single-source template for IHEXS (IHEX shape function helper)
!   Generates both double and single precision variants via preprocessor
!
! GENERATES:
!   - ihexsd.f (DOUBLE PRECISION) with -DDOUBLE_PRECISION flag
!   - ihexss.f (REAL single precision) without flag
!
! ORIGINAL FILES:
!   - mis/ihexsd.f (241 lines)
!   - mis/ihexss.f (238 lines)
!   - Total: 479 lines → ~241 lines template (50% reduction)
!
! DESCRIPTION:
!   Isoparametric utility routine for IHEX elements
!   - Computes shape functions and their derivatives
!   - Calculates Jacobian matrix inverse and determinant
!   - Supports IHEX1 (8-node), IHEX2 (20-node), IHEX3 (32-node)
!
! BUILD COMMANDS:
!   # Double precision:
!   gfortran -cpp -E -DDOUBLE_PRECISION -I. ihexs_template.f90 -o ihexsd.f
!
!   # Single precision:
!   gfortran -cpp -E -I. ihexs_template.f90 -o ihexss.f
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! TEMPLATE: Claude Code (2026-02-09)
!
!===============================================================================

#include "precision_macros.h"

      SUBROUTINE IHEXS##PREC_SUFFIX (TYPE,SHP,DSHP,JACOB,DETJ,EID,XI,ETA,ZETA,BXYZ)
C
#ifdef DOUBLE_PRECISION
C     DOUBLE PRECISION VERSION
#else
C     SINGLE PRECISION VERSION
#endif
HELPER_HEADER

# Process ihexsd.f with replacements
sed '1,3d' mis/ihexsd.f | sed '
# Skip subroutine line (already in header)
/^      SUBROUTINE IHEXSD/d
# Replace type declarations
s/DOUBLE PRECISION/PREC_TYPE/g
# Replace common numeric literals
s/\([^.0-9-]\)0\.0\([^D0-9]\|$\)/\1ZERO\2/g
s/\([^.0-9-]\)1\.0\([^D0-9]\|$\)/\1ONE\2/g
s/\([^.0-9-]\)2\.0\([^D0-9]\|$\)/\1TWO\2/g
s/-1\.0\([^D0-9]\|$\)/-ONE\1/g
# Replace at line start
s/^      -1\.0$/      -ONE/
s/^      1\.0$/      ONE/
s/^      Z   =-1\.0$/      Z   =-ONE/
s/^      Y   =-1\.0$/      Y   =-ONE/
s/^      X   =-1\.0$/      X   =-ONE/
' >> templates/ihexs_template.f90

echo "      END" >> templates/ihexs_template.f90

echo "IHEX helper template created: $(wc -l < templates/ihexs_template.f90) lines"
echo "Total IHEX templates: $(($(wc -l < templates/ihex_template.f90) + $(wc -l < templates/ihexs_template.f90))) lines"
