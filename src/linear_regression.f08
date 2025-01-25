module linear_regression
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none

  type :: LinearRegression
    ! Linear regression algorithm
    real(sp), allocatable :: weights(:, :)
    integer :: seq_len
    integer :: n_features
    real(sp) :: bias
    integer :: iterations
    real(sp) :: learning_rate
  contains
    procedure :: predict
    procedure :: fit
    final :: del
  end type LinearRegression

  interface LinearRegression
    module procedure :: init
  end interface

contains
  function init(seq_len, n_features, iterations, learning_rate) result(self)
    ! initialize parameters and allocate start weights
    ! seq_len: length of input sequences
    ! n_features: (optional, default 1) amount of output features
    ! iterations: (optional, default 10000) amount of training epochs
    ! learning_rate: (optional, default 0.01) learning rate
    integer :: seq_len
    integer, optional :: n_features
    integer, optional :: iterations
    real(sp), optional :: learning_rate
    type(LinearRegression) :: self
    integer :: i, j
    self%seq_len = seq_len
    if (present(n_features)) then
      self%n_features = n_features
    else
      self%n_features = 1
    end if
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
    allocate(self%weights(seq_len, self%n_features))
    do j = 1, seq_len
      do i = 1, self%n_features
        self%weights(j, i) = 0
      end do
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

  function predict(self, x) result(p)
    ! predict with regression
    ! x: (seq_len, total_entries)
    ! weights: w: (seq_len, n_features)
    ! p: (n_features, total_entries)
    class(LinearRegression) :: self
    real(sp), intent (in) :: x(:, :)
    real(sp) :: p(self%n_features, size(x(1, :)))
    p = matmul(transpose(x), self%weights) + self%bias
  end function

  subroutine fit(self, x, y)
    ! fit regression
    ! x: (seq_len, total_entries)
    ! y: (total_entries)
    class(LinearRegression) :: self
    real(sp), intent(in) :: x(:, :)
    real(sp), intent(in) :: y(:, :)
    real(sp) :: y_pred(self%n_features, size(x(1, :)))
    real(sp) :: dw(self%seq_len, self%n_features)
    real(sp) :: db
    real(sp) :: y_reshaped(self%n_features, size(x(1, :)))
    integer :: i
    y_reshaped(:, :) = reshape([y, y, y], shape(y_reshaped))
    do i = 1, self%iterations
      y_pred(:, :) = predict(self, x)
      dw = - (2 * matmul(x, transpose(y_reshaped - y_pred))) / 100
      db = - (2 * sum(y_reshaped - y_pred)) / 100
      self%weights = self%weights - self%learning_rate * dw
      self%bias = self%bias - self%learning_rate * db
    end do
  end subroutine
end module linear_regression
