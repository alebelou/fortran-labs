module maxmin
    use Environment
    implicit none

contains
    pure subroutine max_min(A, B, f1)
        implicit none
        integer   :: C, D
        real(R_), intent(in)  :: A(:,:)
        real(R_), intent(inout) :: B(:), f1
        real(R_) :: maxmin, minim
        integer  :: i, j
        
        C = ubound(A,1)
        D = ubound(A,2)
        do i = 1, C
        minim = A(i,1)
           do j = 1, D
              if (A(i,j) < minim) then
                 minim = A(i,j)                
              end if
           end do
           B(i) = minim
        end do
        maxmin = B(1)
        do i = 1, C
           if (B(i) > maxmin) then
              maxmin = B(i)
           end if
           !print*,maxmin
        end do
        f1 = maxmin
    end subroutine max_min
end module maxmin
