program lab_1_4_v15
    use Environment
    use ISO_Fortran_Env
    use IO
    implicit none

    character(*),       parameter   ::  file_input  = "../data/input.txt"
    character(*),       parameter   ::  file_output = "../bin/output.txt"
    character(*),       parameter   ::  file_data   = "../bin/students.dat"

    type(student)                   ::  students(SIZE_)

    call Create_data_file(file_input, file_data)
    students = Read_class_list(file_data)

    call Output_class_list(file_output, students, "Исходный список:", "rewind")
    call sort_data(students, LBound(students, 1), SIZE_)
    call Output_class_list(file_output, students, "Отсортированный список:", "append")

    contains

        pure recursive subroutine sort_data(students, i, SIZE_)
            type(student),  intent(inout)   ::  students(:)
            integer(I_),    intent(in)      ::  SIZE_
            integer(I_),    intent(in)      ::  i
            integer(I_)                     ::  j
            do j = i+1, SIZE_, 1
                if(students(i)%name == students(j)%name) students(j)%uniq = .FALSE.
            end do
            if(i < SIZE_) call sort_data(students, i+1, SIZE_)
        end subroutine sort_data
end program lab_1_4_v15