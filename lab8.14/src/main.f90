program exercise_8
    use Environment
    use maxmin 
    use minmax

    implicit none
    character(*), parameter  :: output_file = "output"
    integer(I_), parameter   :: N = 10, M = 5
    real(R_)                 :: A(M,N), B(M), r, f1, f2
    integer(I_)              :: j, i, out, t = 1, d = 30, p

    call random_seed
    do i = 1, M
       do j = 1, N
         call random_number(r)
          p = (d - t + 1) * r
          p = t + p
          A(i,j) = p
       end do
    end do
    call max_min(A,B,f1)
    call min_max(A,B,f2)
    open (file = output_file, newunit = out)

      write (out, "("//N//"(f6.1))") (A(i,:), i = 1, M)
      write (out, "(a, T7, ' = ', f6.2)") "Maxmin = ", f1
      write (out, "(a, T7, ' = ', f6.2)") "Minmax = ", f2
      if(f1 == f2) then
         write (out, "(a, T7, ' ', f6.2)") "Minmax и maxmin равны"
      else
         write (out, "(a, T7, ' ', f6.2)") "Minmax и maxmin не равны"
      end if
    close (out)

end program exercise_8
