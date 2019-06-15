program lab_1_5_v15
    use Environment
    use ISO_Fortran_Env
    use IO
    implicit none

    character(*),       parameter   ::  file_input  = "../data/input.txt"
    character(*),       parameter   ::  file_output = "../bin/output.txt"

    type(student),      pointer     ::  students=>Null()

    students => Read_class_list(file_input)
    if (Associated(students)) then

        call Output_class_list(file_output, students, "Исходный список:", "rewind")
        call sort_data(students)
        call Output_class_list(file_output, students, "Отсортированный список:", "append")
    endif

    contains

        pure recursive subroutine sort_data(students)
            type(student),  pointer, intent(inout)   ::  students

            if (Associated(students%next)) then
                call checkUniq(students, students%next)
                call sort_data(students%next)
            endif
        end subroutine sort_data

        pure recursive subroutine checkUniq(current_val, compair_val)
            type(student), pointer :: current_val
            type(student), pointer :: compair_val
            if(current_val%name == compair_val%name) compair_val%uniq = .FALSE.
            if(Associated(compair_val%next)) call checkUniq(current_val, compair_val%next)
        end subroutine checkUniq
end program lab_1_5_v15
