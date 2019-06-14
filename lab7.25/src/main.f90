program lab7_25 
   use Environment
   implicit none
   character(*), parameter                :: input_file = "../data/input.txt" , output_file = "output.txt"
   integer                                :: In = 0, Out = 0, cols, rows, i, j, zoz = 0
   real(R_), dimension(:,:),allocatable   :: A
   
   open (file=input_file, newunit=In)
      read(In, *) rows,cols
      allocate(A(rows,cols))                                      !Чтение массива из файла
      read(In, *) (A(i,:), i = 1,rows)
   close (In) 

   open (file=output_file, encoding=e_, newunit=out)
      write (out,*)"Изначальный массив"
      write (out, "("//cols//"f6.1)")(A(i,:), i = 1,rows)
   close (out)

   open (file=output_file, encoding=e_, newunit=out, position="append") 

   do i = 1, rows
      do j = i+1, rows
         if (ALL(A(i,:) == A(j,:))) then
            if (zoz==0) then
               write(out,*)"Совпадающие строки: "
               write(out,*)i
               zoz=1
            end if
            write(out,*)j
         end if
      end do
      zoz=0
   end do

   do i = 1, cols
      do j = i+1, cols
         if (ALL(A(:,i) == A(:,j))) then
            if(zoz==0) then
               write(out,*)"Совпадающие столбцы: "
               write(out,*)i
               zoz=1
            end if
            write(out,*)j
         end if
      end do
      zoz=0
   end do
   close (out)

end program lab7_25 
