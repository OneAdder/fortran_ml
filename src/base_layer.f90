module abstract_layer
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none
  private
  public :: Layer

  type, abstract :: Layer
    integer :: in_features, out_features
  contains
    procedure(abstract_forward), deferred :: forward
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
end module
