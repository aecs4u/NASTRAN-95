!===============================================================================
! MODULE: linalg_backend_mumps_module
!
! PURPOSE:
!   Defines the MUMPS backend API for sparse direct linear solves. This module
!   offers a compile-safe fallback when MUMPS is not enabled and a direct
!   DMUMPS call path when NASTRAN is configured with USE_MUMPS=ON.
!
! NOTES:
!   - When built without USE_MUMPS, calls return MUMPS_STATUS_NOT_BUILT.
!   - CSR matrix input is converted to COO triplets expected by MUMPS.
!   - Solver state is owned by mumps_solver_type and can be reused across RHS
!     solves after one factorization.
!   - Matrix input format is CSR (row_ptr, col_ind, values).
!
!===============================================================================
module linalg_backend_mumps_module
  use precision_module, only: dp, ip
  implicit none
  private

#ifdef NASTRAN_HAVE_MUMPS
  include 'dmumps_struc.h'
#endif

  integer(ip), parameter, public :: MUMPS_STATUS_OK = 0_ip
  integer(ip), parameter, public :: MUMPS_STATUS_NOT_BUILT = -100_ip
  integer(ip), parameter, public :: MUMPS_STATUS_INVALID_ARGUMENT = -102_ip
  integer(ip), parameter, public :: MUMPS_STATUS_NOT_FACTORIZED = -103_ip
  integer(ip), parameter, public :: MUMPS_STATUS_INIT_FAILED = -200_ip
  integer(ip), parameter, public :: MUMPS_STATUS_FACTOR_FAILED = -201_ip
  integer(ip), parameter, public :: MUMPS_STATUS_SOLVE_FAILED = -202_ip

  type, public :: mumps_solver_type
    logical :: is_enabled = .false.
    logical :: is_factorized = .false.
    integer(ip) :: matrix_order = 0_ip
    integer(ip) :: nnz = 0_ip
    integer(ip) :: symmetry = 0_ip
#ifdef NASTRAN_HAVE_MUMPS
    type(DMUMPS_STRUC) :: id
    integer(ip), pointer :: irn(:) => null()
    integer(ip), pointer :: jcn(:) => null()
    real(dp), pointer :: aval(:) => null()
#endif
  end type mumps_solver_type

  public :: mumps_backend_available
  public :: mumps_initialize
  public :: mumps_factor_csr
  public :: mumps_solve_dense_rhs
  public :: mumps_finalize

contains

  logical function mumps_backend_available()
#ifdef NASTRAN_HAVE_MUMPS
    mumps_backend_available = .true.
#else
    mumps_backend_available = .false.
#endif
  end function mumps_backend_available

  subroutine mumps_initialize(solver, symmetry, status, message, communicator)
    type(mumps_solver_type), intent(inout) :: solver
    integer(ip), intent(in), optional :: symmetry
    integer(ip), intent(out) :: status
    character(len=*), intent(out), optional :: message
    integer(ip), intent(in), optional :: communicator

    solver%is_enabled = mumps_backend_available()
    solver%is_factorized = .false.
    solver%matrix_order = 0_ip
    solver%nnz = 0_ip
    solver%symmetry = 0_ip
    if (present(symmetry)) solver%symmetry = symmetry

#ifdef NASTRAN_HAVE_MUMPS
    solver%id%PAR = 1
    solver%id%SYM = solver%symmetry
    if (present(communicator)) then
      solver%id%COMM = communicator
    else
      solver%id%COMM = USE_COMM_WORLD
    end if

    solver%id%JOB = -1
    call DMUMPS(solver%id)

    if (solver%id%INFOG(1) < 0) then
      status = MUMPS_STATUS_INIT_FAILED
      call set_message(message, mumps_message("MUMPS init failed", solver%id%INFOG(1), &
                                              solver%id%INFOG(2)))
      return
    end if

    ! Silence INFO/WARN logs by default. Keep error stream active.
    solver%id%ICNTL(1) = 6
    solver%id%ICNTL(2) = 0
    solver%id%ICNTL(3) = 0
    solver%id%ICNTL(4) = 0

    status = MUMPS_STATUS_OK
    call set_message(message, "MUMPS session initialized.")
#else
    status = MUMPS_STATUS_NOT_BUILT
    call set_message(message, "MUMPS backend not built (configure with -DUSE_MUMPS=ON).")
#endif
  end subroutine mumps_initialize

  subroutine mumps_factor_csr(solver, row_ptr, col_ind, values, status, message)
    type(mumps_solver_type), intent(inout) :: solver
    integer(ip), intent(in) :: row_ptr(:)
    integer(ip), intent(in) :: col_ind(:)
    real(dp), intent(in) :: values(:)
    integer(ip), intent(out) :: status
    character(len=*), intent(out), optional :: message

    if (size(row_ptr) < 2) then
      status = MUMPS_STATUS_INVALID_ARGUMENT
      call set_message(message, "CSR row_ptr must contain at least two entries.")
      return
    end if

    if (size(col_ind) /= size(values)) then
      status = MUMPS_STATUS_INVALID_ARGUMENT
      call set_message(message, "CSR col_ind and values must have the same size.")
      return
    end if

    if (size(values) == 0) then
      status = MUMPS_STATUS_INVALID_ARGUMENT
      call set_message(message, "Empty CSR matrix is not supported in current MUMPS path.")
      return
    end if

    solver%matrix_order = int(size(row_ptr) - 1, kind=ip)
    solver%nnz = int(size(values), kind=ip)

#ifdef NASTRAN_HAVE_MUMPS
    call clear_matrix_storage(solver)

    allocate(solver%irn(solver%nnz))
    allocate(solver%jcn(solver%nnz))
    allocate(solver%aval(solver%nnz))
    solver%aval = values

    call csr_to_coo(row_ptr, col_ind, solver%irn, solver%jcn, status, message)
    if (status /= MUMPS_STATUS_OK) then
      call clear_matrix_storage(solver)
      solver%is_factorized = .false.
      return
    end if

    solver%id%N = solver%matrix_order
    solver%id%NZ = solver%nnz
    solver%id%IRN => solver%irn
    solver%id%JCN => solver%jcn
    solver%id%A => solver%aval
    solver%id%JOB = 4
    call DMUMPS(solver%id)

    if (solver%id%INFOG(1) < 0) then
      solver%is_factorized = .false.
      status = MUMPS_STATUS_FACTOR_FAILED
      call set_message(message, mumps_message("MUMPS factorization failed", &
                                              solver%id%INFOG(1), solver%id%INFOG(2)))
      return
    end if

    solver%is_factorized = .true.
    status = MUMPS_STATUS_OK
    call set_message(message, "MUMPS factorization completed.")
#else
    status = MUMPS_STATUS_NOT_BUILT
    call set_message(message, "MUMPS backend not built (configure with -DUSE_MUMPS=ON).")
#endif
  end subroutine mumps_factor_csr

  subroutine mumps_solve_dense_rhs(solver, rhs, x, status, message)
    type(mumps_solver_type), intent(in) :: solver
    real(dp), intent(in) :: rhs(:,:)
    real(dp), intent(out) :: x(:,:)
    integer(ip), intent(out) :: status
    character(len=*), intent(out), optional :: message

    if (.not. solver%is_factorized) then
      status = MUMPS_STATUS_NOT_FACTORIZED
      call set_message(message, "MUMPS solve called before successful factorization.")
      return
    end if

    if (any(shape(rhs) /= shape(x))) then
      status = MUMPS_STATUS_INVALID_ARGUMENT
      call set_message(message, "rhs and x must have identical shapes.")
      return
    end if

#ifdef NASTRAN_HAVE_MUMPS
    block
      real(dp), pointer :: rhs_work(:)
      integer(ip) :: n, nrhs

      n = solver%matrix_order
      nrhs = int(size(rhs, 2), kind=ip)

      if (size(rhs, 1) /= n) then
        status = MUMPS_STATUS_INVALID_ARGUMENT
        call set_message(message, "rhs row count must match factorized matrix order.")
        return
      end if

      allocate(rhs_work(n * nrhs))
      rhs_work = reshape(rhs, [n * nrhs])

      solver%id%NRHS = nrhs
      solver%id%LRHS = n
      solver%id%RHS => rhs_work
      solver%id%JOB = 3
      call DMUMPS(solver%id)

      if (solver%id%INFOG(1) < 0) then
        deallocate(rhs_work)
        status = MUMPS_STATUS_SOLVE_FAILED
        call set_message(message, mumps_message("MUMPS solve failed", &
                                                solver%id%INFOG(1), solver%id%INFOG(2)))
        return
      end if

      x = reshape(rhs_work, shape(x))
      deallocate(rhs_work)
    end block

    status = MUMPS_STATUS_OK
    call set_message(message, "MUMPS solve completed.")
#else
    status = MUMPS_STATUS_NOT_BUILT
    call set_message(message, "MUMPS backend not built (configure with -DUSE_MUMPS=ON).")
#endif
  end subroutine mumps_solve_dense_rhs

  subroutine mumps_finalize(solver, status, message)
    type(mumps_solver_type), intent(inout) :: solver
    integer(ip), intent(out) :: status
    character(len=*), intent(out), optional :: message
    logical :: was_enabled

    was_enabled = solver%is_enabled
    solver%is_enabled = .false.
    solver%is_factorized = .false.
    solver%matrix_order = 0_ip
    solver%nnz = 0_ip
    solver%symmetry = 0_ip

#ifdef NASTRAN_HAVE_MUMPS
    if (was_enabled) then
      solver%id%JOB = -2
      call DMUMPS(solver%id)
    end if
    call clear_matrix_storage(solver)
#endif

    status = MUMPS_STATUS_OK
    call set_message(message, "MUMPS backend state cleared.")
  end subroutine mumps_finalize

  subroutine csr_to_coo(row_ptr, col_ind, irn, jcn, status, message)
    integer(ip), intent(in) :: row_ptr(:)
    integer(ip), intent(in) :: col_ind(:)
    integer(ip), intent(out) :: irn(:)
    integer(ip), intent(out) :: jcn(:)
    integer(ip), intent(out) :: status
    character(len=*), intent(out), optional :: message

    integer(ip) :: n, row_base, col_base
    integer(ip) :: i, k, k_start, k_end, nnz

    n = int(size(row_ptr) - 1, kind=ip)
    nnz = int(size(col_ind), kind=ip)

    if (size(irn) /= nnz .or. size(jcn) /= nnz) then
      status = MUMPS_STATUS_INVALID_ARGUMENT
      call set_message(message, "COO buffers must match CSR nnz.")
      return
    end if

    if (nnz == 0_ip) then
      status = MUMPS_STATUS_INVALID_ARGUMENT
      call set_message(message, "Empty CSR matrix is not supported.")
      return
    end if

    if (row_ptr(1) == 0_ip) then
      row_base = 0_ip
    else if (row_ptr(1) == 1_ip) then
      row_base = 1_ip
    else
      status = MUMPS_STATUS_INVALID_ARGUMENT
      call set_message(message, "Unsupported CSR row_ptr base; expected 0 or 1.")
      return
    end if

    if (minval(col_ind) == 0_ip) then
      col_base = 0_ip
    else if (minval(col_ind) >= 1_ip) then
      col_base = 1_ip
    else
      status = MUMPS_STATUS_INVALID_ARGUMENT
      call set_message(message, "Unsupported CSR col_ind base; expected 0 or 1.")
      return
    end if

    if (row_ptr(n + 1) - row_base /= nnz) then
      status = MUMPS_STATUS_INVALID_ARGUMENT
      call set_message(message, "CSR row_ptr terminator does not match nnz.")
      return
    end if

    do i = 1, n
      k_start = row_ptr(i) - row_base + 1_ip
      k_end = row_ptr(i + 1) - row_base

      if (k_start < 1_ip .or. k_end > nnz .or. k_end < k_start - 1_ip) then
        status = MUMPS_STATUS_INVALID_ARGUMENT
        call set_message(message, "Invalid CSR row range detected.")
        return
      end if

      do k = k_start, k_end
        irn(k) = i
        jcn(k) = col_ind(k) - col_base + 1_ip
      end do
    end do

    if (minval(jcn) < 1_ip .or. maxval(jcn) > n) then
      status = MUMPS_STATUS_INVALID_ARGUMENT
      call set_message(message, "CSR column indices out of bounds for square matrix.")
      return
    end if

    status = MUMPS_STATUS_OK
    call set_message(message, "CSR converted to COO triplets.")
  end subroutine csr_to_coo

  subroutine clear_matrix_storage(solver)
    type(mumps_solver_type), intent(inout) :: solver

#ifdef NASTRAN_HAVE_MUMPS
    if (associated(solver%irn)) deallocate(solver%irn)
    if (associated(solver%jcn)) deallocate(solver%jcn)
    if (associated(solver%aval)) deallocate(solver%aval)
#endif
  end subroutine clear_matrix_storage

  function mumps_message(prefix, infog1, infog2) result(msg)
    character(len=*), intent(in) :: prefix
    integer, intent(in) :: infog1
    integer, intent(in) :: infog2
    character(len=256) :: msg

    write(msg, '(A,": INFOG(1)=",I0,", INFOG(2)=",I0)') trim(prefix), infog1, infog2
  end function mumps_message

  subroutine set_message(message, text)
    character(len=*), intent(out), optional :: message
    character(len=*), intent(in) :: text

    if (present(message)) then
      message = ""
      message(1:min(len(message), len_trim(text))) = text(1:min(len(message), len_trim(text)))
    end if
  end subroutine set_message

end module linalg_backend_mumps_module
