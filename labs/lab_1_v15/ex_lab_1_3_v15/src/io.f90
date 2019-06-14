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

    end type student

contains
    ! Создание неформатированного файла данных.
    subroutine Create_data_file(file_input, file_data)
        character(*), intent(in)    :: file_input, file_data
        type(student)               :: stud
        integer(I_)                 :: In, Out, IO, i, recl
        character(:), allocatable   :: format

        open (file = file_input, encoding = E_, newunit = In)
            recl = (LASTNAME_LEN + NAME_LEN + OTCH_LEN ) * CH_ + 1
            open (file = file_data, form = 'unformatted', newunit = Out, access = 'direct', recl = recl)
                format = '(3(a, 1x))'
                do i = 1, SIZE_
                    read (In, format, iostat = IO) stud%lastname, stud%name, stud%otch
                    write (Out, iostat = IO, rec = i) stud
                end do
            close (Out)
        close (In)
    end subroutine Create_data_file

    function Read_class_list(file_data) result(Students)
        type(student)               :: Students(SIZE_)
        character(*), intent(in)    :: file_data

        integer In, IO, recl

        recl = ((LASTNAME_LEN + NAME_LEN + OTCH_LEN ) * CH_ + 1)*SIZE_
        open (file = file_data, form = 'unformatted', newunit = In, access = 'direct', recl = recl)
            read (In, iostat = IO, rec = 1) Students
        close (In)
    end function Read_class_list

    ! Вывод списка класса.
    subroutine Output_class_list(file_output, students, List_name, Position)
        character(*), intent(in) :: file_output, Position, List_name
        type(student), intent(in) :: students(:)

        integer :: Out, IO, i
        character(:), allocatable :: format

        open (file = file_output, encoding = E_, position = Position, newunit = Out)
            write (out, '(/a)') List_name
            format = '(3(a, 1x))'
            do concurrent(i=1:SIZE_, students(i)%uniq)
                write (Out, format, iostat = IO) students(i)%lastname, students(i)%name, &
                                                 students(i)%otch
            end do
        close (Out)
    end subroutine Output_class_list
end module IO
