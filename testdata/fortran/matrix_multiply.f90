program matrix_multiply
    implicit none
    integer, parameter :: n = 3
    real :: A(n,n), B(n,n), C(n,n)
    integer :: i, j, k

    ! Initialize matrices
    call initialize_matrix(A, n)
    call initialize_matrix(B, n)

    ! Multiply
    call mat_mult(A, B, C, n)

    ! Print result
    call print_matrix(C, n)

contains

    subroutine initialize_matrix(M, size)
        integer, intent(in) :: size
        real, intent(out) :: M(size, size)
        integer :: i, j
        do i = 1, size
            do j = 1, size
                M(i,j) = real(i + j)
            end do
        end do
    end subroutine initialize_matrix

    subroutine mat_mult(A, B, C, size)
        integer, intent(in) :: size
        real, intent(in) :: A(size, size), B(size, size)
        real, intent(out) :: C(size, size)
        integer :: i, j, k
        C = 0.0
        do i = 1, size
            do j = 1, size
                do k = 1, size
                    C(i,j) = C(i,j) + A(i,k) * B(k,j)
                end do
            end do
        end do
    end subroutine mat_mult

    subroutine print_matrix(M, size)
        integer, intent(in) :: size
        real, intent(in) :: M(size, size)
        integer :: i
        do i = 1, size
            print *, M(i,:)
        end do
    end subroutine print_matrix

end program matrix_multiply
