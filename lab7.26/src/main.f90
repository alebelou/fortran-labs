program lab7_26 
   use Environment
   implicit none
   character(*), parameter                :: input_file = "../data/input.txt" , output_file = "output.txt"
   integer                                :: In = 0, Out = 0, cols, rows, k, l, i
   real(R_), dimension(:,:),allocatable   :: A
   real(R_), dimension(:),allocatable     :: X,Y
   
   open (file=input_file, newunit=In)
      read(In, *) cols, rows
      allocate(A(rows,cols), X(rows), Y(cols))
      read(In, *) (A(i,:), i = 1,cols)
      read(In, *) X
      read(In, *) Y
      read(In, *) k,l
   close (In) 

   open (file=output_file, encoding=e_, newunit=out)
      write (out,*)"Изначальный массив"
      write (out, "("//cols//"f6.1)")A
   close (out)

   A(k,:)=X
   A(:,l)=Y 

   open (file=output_file, encoding=e_, newunit=out, position="append") 
      write (out,*)"Измененный массив"
      write (out, "("//cols//"f6.1)")A
   close (out)


end program lab7_26 
