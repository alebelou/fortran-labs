program lab5_15
   use Environment
   implicit none
   character(*), parameter                :: input_file = "../data/input.txt" , output_file = "output.txt"
   integer                                :: In = 0, Out = 0, elements!, lb = 0, ub = 100 
   real(R_)                               :: MaxValue!, r = 0, p
   real(R_), dimension(:),allocatable     :: Input

  ! open (file=input_file, newunit=In)
  !    read (In, *) elements
  ! close (In)
  ! allocate(Input(elements))
  
  ! call random_seed;                                                !Заполнение массива случайными значениями

  ! do i=1,elements
  !    call random_number(r)
  !    p=(ub-lb+1)*r
  !    p=lb+p
  !    Input(i)=p
  ! end do
   
   open (file=input_file, newunit=In)
      read(In, *) elements
      allocate(Input(elements))                                      !Чтение массива из файла
      read(In, *) Input
   close (In) 

   open (file=output_file, encoding=e_, newunit=out)
      write (out,*)"Изначальный массив"
      write (out, "("//elements//"f6.1)")Input
   close (out)

   MaxValue = MAXVAL(Input)                                          !Нахождение максимального значения элемента

   if(ALL(Input==0)) then

   open (file=output_file, encoding=e_, newunit=out, position="append")
      write (out,*)"Все элементы массива равны нулю"                 !Обработка вырожденного случая
   close (out)

   else

   call MaxToZero(Input, MaxValue, elements)                         !Вызов подпрограммы

   open (file=output_file, encoding=e_, newunit=out, position="append") 
      write (out,*)"Массив после обработки"                          !Вывод результата в файл
      write (out, "("//elements//"f6.1)") Input
   close (out)

   end if

contains 
   pure subroutine MaxToZero(Input, MaxValue, elements)
      real(R_) Input(:), MaxValue
      integer i, j, elements
      intent(in) MaxValue, elements
      intent(out) Input

   do i=1,elements                                                   !Цикл движения по массиву
      if (Input(i) == MaxValue) then                                 !Случай нахождения элемента с максимальным значением
      Input(i:)=Eoshift(Input(i:),1)
      end if

   end do

   end subroutine MaxToZero

end program lab5_15
