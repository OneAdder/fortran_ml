program test
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  use linear_layer

  real(sp) :: x(3, 4) = reshape([1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], [3, 4])
  real(sp) :: grad(2, 3) = reshape([.1, .1, .1, .1, .1, .1, .1, .1, .1, .1, .1, .1], [2, 3])
  print *, "test"
  call test_linear_layer(x, grad)
contains
  subroutine test_linear_layer(x, grad)
    real(sp) :: x(3, 4)
    real(sp) :: grad(3, 2)
    real(sp) :: expected_forward(3, 2) = reshape([0.82, 0.82, 0.82, 0.82, 0.82, 0.82], [3, 2])
    real(sp) :: expected_grad(3, 4) = reshape(&
        [0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04], [3, 4]&
      )
    real(sp) :: expected_dw(4, 2) = reshape([0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3], [4, 2])
    real(sp) :: expected_db(2) = [0.3, 0.3]
    real(sp) :: actual_forward(3, 2)
    real(sp) :: actual_grad(3, 4)
    real(sp) :: actual_dw(4, 2)
    real(sp) :: actual_db(2)
    type(LinearLayer) :: linear

    linear = LinearLayer(in_features=4, out_features=2, default_weights_item=0.2, default_bias_item=0.02)
    actual_forward = linear%forward(x)
    actual_grad = linear%backward(x, grad)
    actual_dw = linear%dw
    actual_db = linear%db

    if (all(actual_forward.eq.expected_forward)) then
      print *, "test_linear_layer:forward passed"
    else
      print *, "test_linear_layer:forward failed"
    end if
    expected_grad = nint(expected_grad * 100)
    actual_grad = nint(actual_grad * 100)
    if (all(actual_grad.eq.expected_grad)) then
      print *, "test_linear_layer:backward:grad passed"
    else
      print *, "test_linear_layer:backward:grad failed"
    end if
    if (all(actual_dw.eq.expected_dw)) then
      print *, "test_linear_layer:backward:dw passed"
    else
      print *, "test_linear_layer:backward:dw failed"
    end if
    if (all(actual_db.eq.expected_db)) then
      print *, "test_linear_layer:backward:db passed"
    else
      print *, "test_linear_layer:backward:db failed"
    end if
  end subroutine
end program
