module linear_regression
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none
  private
  public :: LinearRegression

  type :: LinearRegression
    ! Linear regression algorithm
    real(sp), allocatable :: weights(:, :)
    integer :: seq_len
    real(sp) :: bias
    integer :: iterations
    real(sp) :: learning_rate
  contains
    procedure :: forward
      procedure :: backward
    procedure :: fit
    final :: del
  end type LinearRegression

  interface LinearRegression
    module procedure :: init
  end interface

contains
  function init(seq_len, iterations, learning_rate) result(self)
    ! initialize parameters and allocate start weights
    ! seq_len: length of input sequences
    ! iterations: (optional, default 10000) amount of training epochs
    ! learning_rate: (optional, default 0.01) learning rate
    integer :: seq_len
    integer, optional :: iterations
    real(sp), optional :: learning_rate
    type(LinearRegression) :: self
    integer :: i
    self%seq_len = seq_len
    if (present(iterations)) then
      self%iterations = iterations
    else
      self%iterations = 10000
    end if
    if (present(learning_rate)) then
      self%learning_rate = learning_rate
    else
      self%learning_rate = 0.01
    end if
    allocate(self%weights(seq_len, 1))
    do i = 1, seq_len
      self%weights(i, 1) = 0
    end do
    self%bias = 0
  end function

  subroutine del(self)
    ! deallocate `weights`
    type(LinearRegression) :: self
    if (allocated(self%weights)) then
      deallocate(self%weights)
    end if
  end subroutine

  function forward(self, x) result(p)
    ! predict with regression
    ! x: (total_entries, seq_len)
    ! weights: w: (seq_len, 1)
    ! p: (total_entries, 1)
    class(LinearRegression) :: self
    real(sp), intent (in) :: x(:, :)
    real(sp) :: p(size(x(:, 1)), 1)
    p = matmul(x, self%weights) + self%bias
  end function

  subroutine backward(self, x, y, y_pred)
    ! back propagation
    ! x: (total_entries, 1)
    ! y: (total_entries, 1)
    ! y_pred: (total_entries, 1)
    class(LinearRegression) :: self
    real(sp), intent(in) :: x(:, :)
    real(sp), intent(in) :: y(size(x(:, 1)), 1)
    real(sp), intent(in) :: y_pred(size(x(:, 1)), 1)
    real(sp) :: dloss_dw(self%seq_len, 1)
    real(sp) :: dloss_db
    dloss_dw = - (2 * matmul(transpose(x), y - y_pred)) / size(x(:, 1))
    dloss_db = - (2 * sum(y - y_pred)) / size(x(:, 1))
    self%weights = self%weights - self%learning_rate * dloss_dw
    self%bias = self%bias - self%learning_rate * dloss_db
  end subroutine

  subroutine fit(self, x, y)
    ! fit regression
    ! x: (total_entries, seq_len)
    ! y: (total_entries)
    class(LinearRegression) :: self
    real(sp), intent(in) :: x(:, :)
    real(sp), intent(in) :: y(:, :)
    real(sp) :: y_pred(size(x(:, 1)), 1)
    integer :: i
    do i = 1, self%iterations
      y_pred(:, :) = self%forward(x)
      call self%backward(x, reshape(y, [size(x(:, 1)), 1]), y_pred)
    end do
  end subroutine
end module linear_regression
