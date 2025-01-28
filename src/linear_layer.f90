module linear_layer
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  use abstract_layer
  implicit none
  private
  public :: LinearLayer

  type, extends(Layer) :: LinearLayer
    real(sp), allocatable :: weights(:, :)
    ! (in_features, out_features)
    real(sp), allocatable :: bias(:)
    ! (out_features)
    real(sp), allocatable :: dw(:, :)
    ! (in_features, out_features)
    real(sp), allocatable :: db(:)
    ! (out_features)
  contains
    procedure, public :: forward
    procedure, public :: backward
    procedure, private :: init_weights
    procedure, private :: init_derivatives
    procedure, private :: del
  end type

  interface LinearLayer
    module procedure :: init
  end interface

contains
  function init(in_features, out_features, default_weights_item, default_bias_item) result(self)
    integer, intent(in) :: in_features, out_features
    real(sp), optional, value :: default_weights_item, default_bias_item
    type(LinearLayer) :: self
    if (.not. present(default_weights_item)) default_weights_item = 0
    if (.not. present(default_bias_item)) default_bias_item = 0
    self%in_features = in_features
    self%out_features = out_features
    call self%init_weights(default_weights_item, default_bias_item)
    call self%init_derivatives()
  end function

  subroutine init_weights(self, default_weights_item, default_bias_item)
    class(LinearLayer) :: self
    real(sp) :: default_weights_item, default_bias_item
    integer i, j
    allocate(self%weights(self%in_features, self%out_features))
    do i = 1, self%in_features
      do j = 1, self%out_features
        self%weights(i, j) = default_weights_item
      end do
    end do
    allocate(self%bias(self%out_features))
    do i = 1, self%out_features
      self%bias(i) = default_bias_item
    end do
  end subroutine

  subroutine init_derivatives(self)
    class(LinearLayer) :: self
    allocate(self%dw(self%in_features, self%out_features))
    allocate(self%db(self%out_features))
  end subroutine

  subroutine del(self)
    class(LinearLayer) :: self
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

  function forward(self, x) result(p)
    ! x: (total_entries, in_features)
    ! p: (total_entries, out_features)
    class(LinearLayer) :: self
    real(sp), intent(in) :: x(:, :)
    real(sp) :: p(size(x(:, 1)), self%out_features)
    integer :: i
    p = matmul(x, self%weights)
    do i = 1, size(x(:, 1))
      p(i, :) = p(i, :) + self%bias
    end do
  end function

  function backward(self, x, prev_grad) result(grad)
    ! x: (total_entries, in_features)
    ! prev_grad: (total_entries, out_features)
    ! grad: (total_entries, in_features)
    class(LinearLayer) :: self
    real(sp), intent(in) :: x(:, :)
    real(sp), intent(in) :: prev_grad(:, :)
    real(sp) :: grad(size(x(:, 1)), self%in_features)
    self%dw = matmul(transpose(x), prev_grad)
    self%db = sum(prev_grad, 1)
    grad = matmul(prev_grad, transpose(self%weights))
  end function
end module
