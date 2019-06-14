program lab1_7b
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0 
   real(R_)                   :: num1 = 0, num2 = 0 
   open (file=input_file, newunit=In)
      read (In, *) num1, num2                                              ! Чтение значений переменных из файла
   close (In)

   open (file=output_file, encoding=e_, newunit=out)
      write (out, "(2(a, f0.2/))") "num1 = ", num1, "num2 = ", num2          
   close (out)                                                             ! Вывод считанных значений в файл 
   
   call Switch(num1,num2)                                                  ! Вызов подпрограммы 

   open (file=output_file, encoding=e_, newunit=out, position='append')
       write (out, "(2(a, f0.2/))") "num1 = ", num1, "num2 = ", num2
   close (out)
  
contains
   pure subroutine Switch(num1,num2)
      real(R_) num1,num2
      intent(out) num1,num2

   num1 = num1+num2
   num2 = num1-num2                                                        ! Замена друг с другом значений двух переменных 
   num1 = num1-num2 

   end subroutine Switch
end program lab1_7b 
