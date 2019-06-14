program lab2_15 
   use Environment
   use ieee_arithmetic 
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0
   real(R_)                   :: Pi = 3.14, x = 0, y = 0 
   open (file=input_file, newunit=In)
      read (In, *) x, y
   close (In)

   open (file=output_file, encoding=e_, newunit=out)
      write (out, "(2(a, f0.2/))") "x = ", x, "y = ", y
   close (out)

   call Calculations(x,y)

   open (file=output_file, encoding=e_, newunit=out, position='append')
      write (out, "(2(a, f0.2/))") "x = ", x, "y = ", y
   close (out)

contains   
   pure subroutine Calculations(x,y)

   real(R_) r,F,x,y
   intent(out) x,y

   r = SQRT((x*x)+(y*y))
   
   if (x==0 .and. y==0) then
      F = 0
   end if
   if (x>0 .and. y>=0) then
      F = ATAN(x/y)
   end if
   if (x==0 .and. y>0) then 
      F = Pi/2
   end if
   if (x<0) then
      F = Pi+ATAN((y/x))
   end if
   if (x==0 .and. y<0) then 
      F = Pi*1.5 
   end if
   if (x>0 .and. y<0) then
      F = 2*Pi+ATAN((y/x))
   end if
   
   x = r*COS(F)
   y = r*SIN(F)
   end subroutine Calculations
end program lab2_15 
