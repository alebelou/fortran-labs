program lab_1_3_v15
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
    call sort_data(students, SIZE_)
    call Output_class_list(file_output, students, "Отсортированный список:", "append")
    contains

    pure subroutine sort_data(students, SIZE_)
        type(student),  intent(inout)   ::  students(:)
        integer(I_),    intent(in)      ::  SIZE_
        integer(I_)                     ::  i, j

        do i = 1, SIZE_, 1
            do j = i+1, SIZE_, 1
                if(students(i)%name == students(j)%name) students(j)%uniq = .FALSE.
            end do
        end do

    end subroutine sort_data
end program lab_1_3_v15
