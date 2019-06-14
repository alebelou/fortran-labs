module minmax
    use Environment
    implicit none

contains
    pure subroutine min_max(A, B, f2)
        implicit none
        integer   :: C, D
        real(R_), intent(in)  :: A(:,:)
        real(R_), intent(inout) :: B(:), f2
        real(R_) :: minmax, maxim
        integer  :: i, j
        
        C = ubound(A,1)
        D = ubound(A,2)
        do i = 1, C
        maxim = A(i,1)
           do j = 1, D
              if (A(i,j) > maxim) then
                 maxim = A(i,j)
              end if
           end do
           B(i) = maxim
        end do
        minmax = B(1)
        do i = 1, C
           if (B(i) < minmax) then
              minmax = B(i)
           end if
        end do
        f2 = minmax
    end subroutine min_max
end module minmax
