module IO
    use Environment

    integer(I_),        parameter   ::  STRING_LEN      = 40
    type str

        character(STRING_LEN, kind=CH_)     ::  row          = ""
        integer(I_)                         ::  count_spaces = 1
        type(str), pointer                  ::  next         => Null()

    end type str

contains
    function Read_class_list(file_input) result(str_text)
        type(str), pointer      :: str_text
        character(*),  intent(in)   :: file_input
        integer IN

        open (file = file_input, encoding=E_, newunit = IN)
             str_text => Read_student(IN)
        close (IN)
    end function Read_class_list

    recursive function Read_student(IN) result(str_text)
        type(str),      pointer    :: str_text
        integer,        intent(in) :: In
        integer  IO
        character(:),   allocatable:: format

        allocate (str_text)
        format = '(a)'
        read (In, format, iostat=IO) str_text%row
        if (IO == 0) then
            str_text%next => Read_student(IN)
        else
            deallocate (str_text)
            nullify (str_text)
        end if
    end function Read_student

    subroutine Output_class_list(file_output, str_text, List_name, Position)
        character(*),  intent(in)       :: file_output, Position, List_name
        type(str),     intent(in)       :: str_text

        integer :: Out

        open (file = file_output, encoding = E_, position = Position, newunit = Out)
            write (out, '(/a)') List_name
            call Output_person(Out, str_text)
        close (Out)
    end subroutine Output_class_list

    recursive subroutine Output_person(Out, str_text)
        integer, intent(in)        :: Out
        type(str), intent(in)  :: str_text

        integer  :: IO
        character(:), allocatable  :: format

        format = '('//(str_text%count_spaces/2)+1//'x,a)'
        write (Out, format, iostat = IO) trim(str_text%row)

        if (Associated(str_text%next)) &
            call Output_person(Out, str_text%next)
    end subroutine Output_person
end module IO
