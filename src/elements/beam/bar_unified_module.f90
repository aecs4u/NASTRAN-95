!===============================================================================
! MODULE: bar_unified_module
!
! PURPOSE:
!   Provides unified entry point for BAR element stiffness and mass matrix
!   generation, dispatching to precision-specific implementations (BARD/BARS).
!   This is Phase 1 of consolidation - documenting duplication while maintaining
!   backward compatibility.
!
! THEORY:
!   BAR elements are 1D beam members with bending, axial, and torsion:
!     - 2 nodes (endpoints)
!     - 6 DOF per node (3 translations + 3 rotations)
!     - Axial + bending (2 planes) + torsion
!     - Pin flags for selective DOF release
!     - Offsets at both ends
!
!   Stiffness matrix (12×12 local):
!     - Axial: K_axial = EA/L
!     - Bending: K_bend = [12EI/L³, 6EI/L², 4EI/L, 2EI/L]
!     - Torsion: K_torsion = GJ/L
!
!   Mass matrix (12×12 local, consistent):
!     M = (ρAL/420) × [coefficient matrix]
!     Coefficients: 156, 140, 22, 13, 4, 3
!
! CONSOLIDATION:
!   Consolidates:
!     - bard.f (908 lines) - Double precision
!     - bars.f (880 lines) - Single precision
!   Total: 1,788 lines with 97.2% duplication
!
!   Duplication: Precision-only differences (DOUBLE PRECISION vs REAL,
!   DSQRT vs SQRT, TRANSD vs TRANSS, GMMATD vs GMMATS)
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! PHASE 1 WRAPPER: Claude Code (2026-02-09)
!
!===============================================================================

module bar_unified_module
  use precision_module, only: ip
  implicit none
  private

  public :: bar_unified

  integer(ip), parameter :: PRECISION_SINGLE = 1
  integer(ip), parameter :: PRECISION_DOUBLE = 2

contains

  subroutine bar_unified()
    integer :: iprec
    logical :: nogo, heat
    integer :: ixtra, izr, nzr, dumy(12), kmbgg(3), icmbar
    common /emgprm/ ixtra, izr, nzr, dumy, kmbgg, iprec, nogo, heat, icmbar

    select case (iprec)
    case (PRECISION_SINGLE)
      call bars()
    case (PRECISION_DOUBLE)
      call bard()
    case default
      call bard()  ! Default to double precision
    end select

  end subroutine bar_unified

end module bar_unified_module
