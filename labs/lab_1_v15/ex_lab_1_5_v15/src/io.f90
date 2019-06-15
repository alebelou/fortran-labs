module IO
    use Environment

    integer(I_),        parameter   ::  SIZE_ = 7
    integer(I_),        parameter   ::  LASTNAME_LEN    = 15,  &
                                        NAME_LEN        = 10,  &
                                        OTCH_LEN        = 15

    type student

        character(LASTNAME_LEN, kind=CH_)    ::  lastname = ""
        character(NAME_LEN, kind=CH_)        ::  name     = ""
        character(OTCH_LEN, kind=CH_)        ::  otch     = ""
        logical(1)                           ::  uniq     = .TRUE.
        type(student), pointer               ::  next     => Null()

    end type student

contains
        function Read_class_list(file_input) result(students)
        type(student), pointer      :: students
        character(*), intent(in)    :: file_input

        integer IN

        open (file = file_input, encoding=E_, newunit = IN)
             students => Relen_ad_student(IN)
        close (IN)
    end function Read_class_list

    recursive function Relen_ad_student(In) result(stud)
        type(student),  pointer    :: stud
        integer,        intent(in) :: In
        integer  IO
        character(:),   allocatable:: format

        allocate (stud)
        format = '(3(a, 1x))'
        read (In, format, iostat=IO) stud%lastname, stud%name, stud%otch
        if (IO == 0) then
            stud%next => Relen_ad_student(In)
        else
            deallocate (stud)
            nullify (stud)
        end if
    end function Relen_ad_student

    subroutine Output_class_list(file_output, students, List_name, Position)
        character(*),  intent(in) :: file_output, Position, List_name
        type(student), intent(in) :: students

        integer :: Out

        open (file = file_output, encoding = E_, position = Position, newunit = Out)
            write (out, '(/a)') List_name
            call Output_person(Out, students)
        close (Out)
    end subroutine Output_class_list

    recursive subroutine Output_person(Out, stud)
        integer, intent(in)        :: Out
        type(student), intent(in)  :: stud

        integer  :: IO
        character(:), allocatable  :: format


        format = '(3(a, 1x))'
        If(stud%uniq) then
           write (Out, format, iostat = IO) stud%lastname, &
                                            stud%name, &
                                            stud%otch
        endif
        if (Associated(stud%next)) &
            call Output_person(Out, stud%next)
    end subroutine Output_person
end module IO
