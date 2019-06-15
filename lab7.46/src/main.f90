program lab7_46 
   use Environment
   implicit none
   character(*), parameter                :: input_file = "../data/input.txt" , output_file = "output.txt"
   integer                                :: In = 0, Out = 0, i, length
   real(R_), dimension(:),allocatable     :: X,Y,R,res
   
   open (file=input_file, newunit=In)
      read(In, *) length 
      allocate(X(length), Y(length))
      read(In, *) X
      read(In, *) Y
   close (In) 

   open (file=output_file, encoding=e_, newunit=out)
      write (out,*)"Точки :"
      do i = 1, length
         write (out,'(a,f6.1,a,f6.1,a)')"(", X(i), " , ", Y(i), ")"
      end do
   close (out)
   R = X ** 2 + Y ** 2
   !print*, R

   res = MaxLoc(R)

   open (file=output_file, encoding=e_, newunit=out, position="append") 
      write (out,*)"Индекс искомой точки"
      write (out,"(f3.0)") res
   close (out)


end program lab7_46 
