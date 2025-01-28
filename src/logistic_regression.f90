module logistic_regression
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  use linear_regression, only: LinearRegression
  use functions, only: sigmoid
  implicit none
  private
  public :: LogisticRegression

  type, extends(LinearRegression) :: LogisticRegression
  contains
    procedure :: forward => forward
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

  function forward(self, x) result(p)
    ! predict with regression
    ! x: (total_entries, seq_len)
    ! weights: w: (seq_len, 1)
    ! p: (total_entries, 1)
    class(LogisticRegression) :: self
    real(sp), intent (in) :: x(:, :)
    real(sp) :: z(size(x(:, 1)), 1)
    real(sp) :: p(size(x(:, 1)), 1)
    z = matmul(x, self%weights) + self%bias
    p = sigmoid(z)
  end function

end module logistic_regression
