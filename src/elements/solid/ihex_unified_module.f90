!===============================================================================
! MODULE: ihex_unified_module
!
! PURPOSE:
!   Provides unified entry point for IHEX (8-node hexahedral) element stiffness
!   and mass matrix generation, dispatching to precision-specific implementations
!   (IHEXD/IHEXS). This is Phase 1 of consolidation.
!
! THEORY:
!   IHEX (CHEXA8) is an 8-node hexahedral solid element:
!     - 8 corner nodes
!     - 3 DOF per node (3 translations, no rotations)
!     - Trilinear interpolation
!     - Suitable for 3D solid structures
!     - Integration: 1-point, 8-point, 14-point, or 27-point Gauss
!
!   Stiffness matrix (24×24):
!     [K] = ∫∫∫ [B]ᵀ[D][B] dV
!
!   Mass matrix (24×24, consistent):
!     [M] = ∫∫∫ ρ [N]ᵀ[N] dV
!
! CONSOLIDATION:
!   Consolidates:
!     - ihexd.f (1,395 lines) - Double precision main
!     - ihexs.f (1,392 lines) - Single precision main
!   Total: 2,787 lines with 99.8% duplication
!
!   Companion module (ihexs_helper_module) consolidates:
!     - ihexsd.f (241 lines) - Double precision helper
!     - ihexss.f (238 lines) - Single precision helper
!   Total: 479 lines with 99.6% duplication
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! PHASE 1 WRAPPER: Claude Code (2026-02-09)
!
!===============================================================================

module ihex_unified_module
  use precision_module, only: ip
  implicit none
  private

  public :: ihex_unified

  integer(ip), parameter :: PRECISION_SINGLE = 1
  integer(ip), parameter :: PRECISION_DOUBLE = 2

contains

  subroutine ihex_unified()
    integer :: sysbuf, nout, nogo, idum(51), prec
    common /system/ sysbuf, nout, nogo, idum, prec

    select case (prec)
    case (PRECISION_SINGLE)
      call ihexs()
    case (PRECISION_DOUBLE)
      call ihexd()
    case default
      call ihexd()  ! Default to double precision
    end select

  end subroutine ihex_unified

end module ihex_unified_module

!===============================================================================
! MODULE: ihexs_helper_module
!
! PURPOSE:
!   Provides unified entry point for IHEX shape function and Jacobian helper
!   routines, dispatching to precision-specific implementations (IHEXSD/IHEXSS).
!
! CONSOLIDATION:
!   Consolidates:
!     - ihexsd.f (241 lines) - Double precision shape functions + Jacobian
!     - ihexss.f (238 lines) - Single precision shape functions + Jacobian
!   Total: 479 lines with 99.6% duplication
!
!===============================================================================

module ihexs_helper_module
  use precision_module, only: ip
  implicit none
  private

  public :: ihexs_helper_unified

  integer(ip), parameter :: PRECISION_SINGLE = 1
  integer(ip), parameter :: PRECISION_DOUBLE = 2

contains

  subroutine ihexs_helper_unified(ierr, itype, ipnt, kpnt, detj, shp, dshp, bxyz)
    integer, intent(inout) :: ierr
    integer, intent(in) :: itype, ipnt, kpnt
    real, intent(inout) :: detj
    real, intent(inout) :: shp(*), dshp(3,*), bxyz(3,*)

    integer :: sysbuf, nout, nogo, idum(51), prec
    common /system/ sysbuf, nout, nogo, idum, prec

    select case (prec)
    case (PRECISION_SINGLE)
      call ihexss(ierr, itype, ipnt, kpnt, detj, shp, dshp, bxyz)
    case (PRECISION_DOUBLE)
      ! Note: Calling double precision version requires proper type handling
      call ihexsd(ierr, itype, ipnt, kpnt, detj, shp, dshp, bxyz)
    case default
      call ihexsd(ierr, itype, ipnt, kpnt, detj, shp, dshp, bxyz)
    end select

  end subroutine ihexs_helper_unified

end module ihexs_helper_module
