module abstract_layer
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none
  private
  public :: Layer

  type, abstract :: Layer
    integer :: in_features, out_features
    real(sp), allocatable :: weights(:, :)
    real(sp), allocatable :: bias(:)
    real(sp), allocatable :: dw(:, :)
    real(sp), allocatable :: db(:)
  contains
    procedure(abstract_forward), deferred :: forward
    procedure(abstract_backward), deferred :: backward
    procedure :: init_weights
    procedure :: init_derivatives
    procedure :: del
  end type

  interface
    function abstract_forward(self, x) result(p)
      use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
      import Layer
      class(Layer) :: self
      real(sp), intent(in) :: x(:, :)
      real(sp) :: p(self%out_features, size(x(1, :)))
    end function
  end interface

  interface
    function abstract_backward(self, x, prev_grad) result(grad)
      use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
      import Layer
      class(Layer) :: self
      real(sp), intent(in) :: x(:, :)
      real(sp), intent(in) :: prev_grad(:, :)
      real(sp) :: grad(size(x(:, 1)), size(x(1, :)))
    end function
  end interface

contains
  subroutine init_weights(self)
    class(Layer) :: self
    integer i, j
    allocate(self%weights(self%in_features, self%out_features))
    do i = 1, self%in_features
      do j = 1, self%out_features
        self%weights(i, j) = 0.2
      end do
    end do
    allocate(self%bias(self%out_features))
    do i = 1, self%out_features
      self%bias(i) = 0
    end do
  end subroutine

  subroutine init_derivatives(self)
    class(Layer) :: self
    allocate(self%dw(self%in_features, self%out_features))
    allocate(self%db(self%out_features))
  end subroutine

  subroutine del(self)
    class(Layer) :: self
    if (allocated(self%weights)) then
      deallocate(self%weights)
    end if
    if (allocated(self%bias)) then
      deallocate(self%bias)
    end if
    if (allocated(self%dw)) then
      deallocate(self%dw)
    end if
    if (allocated(self%db)) then
      deallocate(self%db)
    end if
  end subroutine
end module



module linear_layer
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  use abstract_layer
  implicit none
  private
  public :: LinearLayer

  type, extends(Layer) :: LinearLayer
  contains
    procedure :: forward
    procedure :: backward
  end type

  interface LinearLayer
    module procedure :: init
  end interface
contains
  function init(in_features, out_features) result(self)
    integer, intent(in) :: in_features, out_features
    type(LinearLayer) :: self
    self%in_features = in_features
    self%out_features = out_features
    call self%init_weights()
    call self%init_derivatives()
  end function

  function forward(self, x) result(p)
    ! x: (in_features, total_entries)
    ! p: (out_features, total_entries)
    class(LinearLayer) :: self
    real(sp), intent(in) :: x(:, :)
    real(sp) :: p(self%out_features, size(x(1, :)))
    p = matmul(transpose(self%weights), x)
  end function

  function backward(self, x, prev_grad) result(grad)
    ! x: (in_features, total_entries)
    ! prev_grad: (out_features, in_features)
    ! grad: (out_features, in_features)
    class(LinearLayer) :: self
    real(4), intent(in) :: x(:, :)
    real(4), intent(in) :: prev_grad(:, :)
    real(4) :: grad(size(x(:, 1)), size(x(1, :)))
    self%dw = matmul(x, transpose(prev_grad))
    grad = matmul(self%weights, prev_grad)
  end function
end module
