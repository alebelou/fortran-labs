program lab7_15a 
   use Environment
   implicit none
   character(*), parameter                :: input_file = "../data/input.txt" , output_file = "output.txt"
   integer                                :: In = 0, Out = 0, cols, rows,i
   real(R_)                               :: Res
   real(R_), dimension(:,:),allocatable   :: A
   
   open (file=input_file, newunit=In)
      read(In, *) cols,rows
      allocate(A(cols,rows))                                                !Чтение массива из файла
      read(In, *) (A(i,:), i = 1,cols)                                      !!правильно прочитать массив конструктором
   close (In) 

   open (file=output_file, encoding=e_, newunit=out)
      write (out,*)"Изначальный массив"
      write (out, "("//cols//"f6.1)") (A(i,:), i = 1,cols)  
      close (out)


   !Res=Minval(Maxval(A,dim=2))
   call MinMaxElement(A,Res)                                                !Вызов подпрограммы

   open (file=output_file, encoding=e_, newunit=out, position="append") 
      write (out,*)"Значение минимаксного элемента по строкам = "           !Вывод результата в файл
      write (out,"(f6.1)") Res
   close (out)

contains 
   pure subroutine MinMaxElement(A,Res)
      real(R_) A(:,:)
      real(R_) Res
      intent(in) A 
      intent(out) Res

   Res=Minval(Maxval(A,dim=2))

   end subroutine MinMaxElement

end program lab7_15a 
