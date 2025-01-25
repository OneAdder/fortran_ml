module logistic_regression
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  use linear_regression
  implicit none
  private
  public :: LogisticRegression

  type, extends(LinearRegression) :: LogisticRegression
  contains
    procedure :: predict => predict
  end type LogisticRegression

  interface LogisticRegression
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
    type(LogisticRegression) :: self
    integer :: i
    self%seq_len = seq_len
    if (present(iterations)) then
      self%iterations = iterations
    else
      self%iterations = 1000
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

  function predict(self, x) result(p)
    ! predict with regression
    ! x: (seq_len, total_entries)
    ! weights: w: (seq_len, 1)
    ! p: (1, total_entries)
    class(LogisticRegression) :: self
    real(sp), intent (in) :: x(:, :)
    real(sp) :: z(1, size(x(1, :)))
    real(sp) :: p(1, size(x(1, :)))
    z = matmul(transpose(x), self%weights) + self%bias
    p = 1 / (1 + exp(-z))
  end function

end module logistic_regression
