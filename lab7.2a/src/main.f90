program exercise_7_2a
   use Environment

   implicit none
   character(*), parameter                :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                                :: In = 0, Out = 0, M = 0, i, MinInd
   real(R_)                               :: tmp
   real(R_),dimension(:), allocatable     :: A

   open (file=input_file, newunit=In)
      read (In, *) M
      allocate (A(M))
      read (In, *) A
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(/"//M//"f6.2)") A

   do i = 1, Size(A)-1
      MinInd = MinLoc(A(i:), 1) + i-1
      if (i /= MinInd .and. A(MinInd) <= 0) then
         tmp       = A(i)
         A(i)      = A(MinInd)
         A(MinInd) = tmp
      end if
   end do

      write (Out, "(/"//M//"f6.2)") A
   close (Out)

end program exercise_7_2a
