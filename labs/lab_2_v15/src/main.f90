program lab_2_v15
    use Environment
    use ISO_Fortran_Env
    use IO
    implicit none

    character(*),       parameter   ::  file_input  = "../data/input.txt"
    character(*),       parameter   ::  file_output = "../bin/output.txt"
    type(str),          pointer     ::  str_text=>Null()

    str_text => Read_class_list(file_input)
    if (Associated(str_text)) then
        call Output_class_list(file_output, str_text, " Исходный список:", "rewind")
        call removeSpaces(str_text)
        call Output_class_list(file_output, str_text, " Отцентрированный список:", "append")
    endif

    contains

        recursive subroutine removeSpaces(str_text)
            type(str), pointer, intent(inout)   ::  str_text

            str_text%row = ADJUSTL(str_text%row)
            str_text%count_spaces = int(STRING_LEN - len_trim(str_text%row))
            if(Associated(str_text%next)) call removeSpaces(str_text%next)
        end subroutine removeSpaces
end program lab_2_v15
