program test_ml
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  use :: linear_regression
  implicit none
  type :: iris_entry
    real(sp) :: p1, p2, p3, p4, label
  end type iris_entry
  type(iris_entry), allocatable :: iris_dataset(:)
  integer :: file_length
  real(sp), allocatable :: x(:, :)
  real(sp), allocatable :: y(:)
  integer :: i
  type(LinearRegression) :: lin_reg

  file_length = get_file_length("data/iris.csv")
  allocate(iris_dataset(file_length))
  call load_iris_csv("data/iris.csv", file_length, iris_dataset)
  allocate(x(4, size(iris_dataset)))
  do i = 1, size(iris_dataset)
    x(1, i) = iris_dataset(i)%p1
    x(2, i) = iris_dataset(i)%p2
    x(3, i) = iris_dataset(i)%p3
    x(4, i) = iris_dataset(i)%p4
  end do
  y = iris_dataset%label
  lin_reg = LinearRegression(4)
  call lin_reg%fit(x, reshape(y, (/size(y), 1/)))
  print *, 'Linear regression weights: ', lin_reg%weights
  deallocate(x)
  deallocate(iris_dataset)


contains
  function get_file_length(filename) result(n_lines)
    implicit none
    character(len=*), intent(in) :: filename
    integer :: n_lines
    integer :: io
    n_lines = 0
    open(1, file=filename)
    do
      read(1, *, iostat=io)
      if (io/=0) exit
      n_lines = n_lines + 1
    end do
    close(1)
  end function get_file_length

  subroutine load_iris_csv(filename, file_len, iris_dataset)
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(in) :: file_len
    type(iris_entry), intent(inout) :: iris_dataset(:)
    integer :: io
    integer :: i
    character(len=100) :: line
    open(1, file=filename)
    rewind(1)
    do i = 1, file_len
      read(1, '(A)', iostat=io) line
      if (io /= 0) exit
      read(line, *, iostat=io) iris_dataset(i)
      if (io /= 0) exit
    end do
    close(1)
  end subroutine load_iris_csv
end program
