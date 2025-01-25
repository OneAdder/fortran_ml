module functions
  use, intrinsic :: iso_fortran_env, only: sp=>real32
  implicit none

contains
  function sigmoid(matrix) result(p)
    real(sp), intent(in) :: matrix(:, :)
    real(sp) :: p(size(matrix(:, 1)), size(matrix(1, :)))
    p = 1 / (1 + exp(-matrix))
  end function
  
  function relu(matrix) result(p)
    real(sp), intent(in) :: matrix(:, :)
    real(sp) :: p(size(matrix(:, 1)), size(matrix(1, :)))
    real(sp) :: current
    integer :: i, j
    do i = 1, size(matrix, 1)
      do j = 1, size(matrix, 2)
        current = matrix(i, j)
        if (current >= 0) then
          p(i, j) = current
        else
          p(i, j) = 0
        end if
      end do
    end do
  end function
end module
