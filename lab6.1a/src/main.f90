program exercise_6_1a
   use Environment
   use IEEE_Arithmetic
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: sin_x = 0, x = 0

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)

   sin_x = Sin_func(x)

   open (file=output_file, encoding=E_, newunit=Out)
   write (Out, '(4(a, T19, "=", e13.6/))') 'x', x, 'Sin(x)', sin_x, 'Fortran sin(x)', sin(x), 'Error', sin_x - sin(x)
   close (Out)

contains
   real(R_) function Sin_func(x)
      real(R_), intent(in) :: x
      real(R_)             :: Items(3) = 0
      integer              :: i     
      
      Items(1) = x

      do i = 2,3
         Items(i) = Items(i-1) * (x ** 2)
      end do

      Items = Items / [1, -2*3, 2*3*4*5] 

      Sin_func = sum(Items)

   end function Sin_func

end program exercise_6_1a
