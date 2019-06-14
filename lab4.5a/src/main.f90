program lab4_5a
   use Environment
   use ieee_arithmetic

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In=0, Out=1, i, N
   real(R_)                :: a = 0, b = 1             !границы интегрирования
   real(R_)                :: h                        !шаг  
   real(R_)                :: buf
   real(R_),allocatable    :: X(:)                     !Массив точек вектора
   real(R_)                :: Integral

   open(newunit=In, file=input_file)
      read(In, *) h
   close(In)

   N = Nint((b-a)/h)
   allocate(X(N))

   X=[(a+h/2+h*(i-1), i = 1,N)]

   Integral = h*Sum(0.8*(X)*(-exp(X**2+0.5)))

   open(file=output_file, encoding=E_, newunit=Out)
      write(Out,*) "Интеграл равен", Integral
   close(Out)

end program lab4_5a 
