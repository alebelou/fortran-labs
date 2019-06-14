program lab3_12 
   use Environment
   implicit none
   character(*), parameter                 :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                                 :: In = 0, Out = 0, rows = 0, cols = 0, i = 0, j = 0, lb = 0, ub = 100
   real(R_)                                :: r = 0, alpha= 0, p
   real(R_), dimension(:,:), allocatable   :: A,B


   open (file=input_file, newunit=In)
      read (In, *) cols, rows, alpha
   close (In)
   
   allocate(A(cols,rows),B(cols,rows))
   
   call random_seed;

   do i=1,cols
      do j=1,rows
      call random_number(r)
      p=(ub-lb+1)*r
      p=lb+p
      a(i,j)=p
      end do
   end do
   
   call Replace(A,B,alpha)

   open (file=output_file, encoding=e_, newunit=out)
      write (out, "("//rows//"f6.1)") A 
      write (out,*) ''
   close (out)

   open (file=output_file, encoding=e_, newunit=out, position="append")
      write (out, "("//rows//"f6.1)") B
   close (out)
contains
   pure subroutine Replace(a,b,alpha)
      real(R_), dimension(:,:), allocatable :: a,b
      integer i,j
      real(R_) alpha
      intent(inout) a,b
      intent(in) alpha

      B = A * alpha
   do i=1,cols
      do j=1,rows
         b(i,j)=a(i,j)*alpha
      end do
   end do
   end subroutine Replace
end program lab3_12 
