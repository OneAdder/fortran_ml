program test_functions
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  use functions, only: sigmoid, relu
  real(sp) :: test_case(2, 3) = reshape([-1.0, -2.0, -3.0, 4.0, 5.0, 6.0], [2, 3])
  call test_sigmoid(test_case, reshape(&
      [0.268941432, 0.119202919, 4.74258736E-02,&
      0.982013762, 0.993307173, 0.997527421], [2, 3]))
  call test_relu(test_case, reshape([0.0, 0.0, 0.0, 4.0, 5.0, 6.0], [2, 3]))

contains
  subroutine test_sigmoid(m, expected)
    real(sp), intent(in) :: m(:, :)
    real(sp), intent(in) :: expected(:, :)
    real(sp) :: actual(size(m(:, 1)), size(m(1, :)))
    actual = sigmoid(m)
    if (all(actual.eq.expected)) then
      print *, "test_sigmoid passed"
    else
      print *, "test_sigmoid failed"
    end if
  end subroutine

  subroutine test_relu(m, expected)
    real(sp), intent(in) :: m(:, :)
    real(sp), intent(in) :: expected(:, :)
    real(sp) :: actual(size(m(:, 1)), size(m(1, :)))
    actual = relu(m)
    if (all(actual.eq.expected)) then
      print *, "test_relu passed"
    else
      print *, "test_relu failed"
    end if
  end subroutine
end program
