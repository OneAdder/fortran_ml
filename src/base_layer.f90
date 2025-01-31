module abstract_layer
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none
  private
  public :: Layer

  type, abstract :: Layer
    integer :: in_features, out_features
  contains
    procedure(abstract_forward), deferred :: forward
    procedure(abstract_backward), deferred :: backward
  end type

  interface
    function abstract_forward(self, x) result(p)
      use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
      import Layer
      class(Layer) :: self
      real(sp), intent(in) :: x(:, :)
      real(sp) :: p(size(x(:, 1)), self%out_features)
    end function
  end interface

  interface
    function abstract_backward(self, x, prev_grad) result(grad)
      use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
      import Layer
      class(Layer) :: self
      real(sp), intent(in) :: x(:, :)
      real(sp), intent(in) :: prev_grad(:, :)
      real(sp) :: grad(size(x(:, 1)), self%in_features)
    end function
  end interface
end module
