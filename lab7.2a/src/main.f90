program exercise_7_2a
   use Environment

   implicit none
   character(*), parameter                :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                                :: In = 0, Out = 0, M = 0
   real(R_) ,dimension(:), allocatable         :: A, Negatives
   logical, allocatable                   :: Neg(:)

   open (file=input_file, newunit=In)
      read (In, *) M
      allocate (A(M))
      read (In, *) A
   close (In)


   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(/"//M//"f6.2)") A
   close(out)

   allocate(Neg(M))
   allocate(Negatives(M))
   Neg = .false.

   call SortNegatives(A, Neg, Negatives)

   open (file=output_file, encoding=E_, newunit=Out, position = "append")
      write (Out, "(/"//M//"f6.2)") A
      !write (Out, *) Positives
   close (Out)
contains
   pure subroutine SortNegatives(A, Neg,Negatives)
      real, intent(inout)      :: A(:), Negatives(:)
      logical, intent(inout)   :: Neg(:)
      real                     :: tmp
      integer                  :: i, MinInd

      Neg = (A <= 0)
      A(:) = [Pack(A,Neg),Pack(A,.not. Neg)]
      do i = 1, Count(Neg)
         MinInd = MinLoc(A(i:), 1) + i-1
         if (i /= MinInd) then
            tmp       = A(i)
            A(i)      = A(MinInd)
            A(MinInd) = tmp
         end if
      end do
  end subroutine SortNegatives

end program exercise_7_2a
