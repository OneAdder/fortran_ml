module functions
  use, intrinsic :: iso_fortran_env, only: sp=>real32
  implicit none

contains
  function sigmoid(x) result(y)
    real(sp), intent(in) :: x(:, :)
    real(sp) :: y(size(x(:, 1)), size(x(1, :)))
    y = 1 / (1 + exp(-x))
  end function
  
  function sigmoid_derivative(x) result(y)
    real(sp), intent(in) :: x(:, :)
    real(sp) :: y(size(x(:, 1)), size(x(1, :)))
    y = sigmoid(x) * (1 - sigmoid(x))
  end function
  
  function relu(x) result(y)
    real(sp), intent(in) :: x(:, :)
    real(sp) :: y(size(x(:, 1)), size(x(1, :)))
    real(sp) :: current
    integer :: i, j
    do i = 1, size(x, 1)
      do j = 1, size(x, 2)
        current = x(i, j)
        if (current >= 0) then
          y(i, j) = current
        else
          y(i, j) = 0
        end if
      end do
    end do
  end function

  function relu_derivative(x) result(y)
    real(sp), intent(in) :: x(:, :)
    real(sp) :: y(size(x(:, 1)), size(x(1, :)))
    real(sp) :: current
    integer :: i, j
    do i = 1, size(x, 1)
      do j = 1, size(x, 2)
        current = x(i, j)
        if (current >= 0) then
          y(i, j) = 1
        else
          y(i, j) = 0
        end if
      end do
    end do
  end function relu_derivative

  function tanh_derivative(x) result(y)
    real(sp), intent(in) :: x(:, :)
    real(sp) :: y(size(x(:, 1)), size(x(1, :)))
    y = 1 - (tanh(x) ** 2)
  end function
end module
