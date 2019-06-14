program lab_1_1_v15
    use Environment
    use ISO_Fortran_Env
    implicit none

    character(*),       parameter   ::  file_input = "../data/input.txt"
    character(*),       parameter   ::  file_output = "../bin/output.txt"
    integer(I_),        parameter   ::  LASTNAME_LEN    = 15,  &
                                        NAME_LEN        = 10,  &
                                        OTCH_LEN        = 15
    integer(I_),        parameter   ::  SIZE_ = 7

    integer(I_)                     ::  In = 0, Out = 0
    integer(I_)                     ::  i, IO

    character(LASTNAME_LEN, kind=CH_),  allocatable ::  stud_lastnames(:)
    character(NAME_LEN, kind=CH_),      allocatable ::  stud_names(:)
    character(OTCH_LEN, kind=CH_),      allocatable ::  stud_otch(:)
    logical,                            allocatable ::  mask(:)

    call readData(stud_lastnames, stud_names, stud_otch, SIZE_)
    call outFirstData(stud_lastnames, stud_names, stud_otch, SIZE_)
    call createMask(mask, stud_names, SIZE_)
    call outSecondData(stud_lastnames, stud_names, stud_otch, mask, SIZE_)

    deallocate(stud_lastnames, stud_otch, stud_names, mask)

    contains

    subroutine readData(stud_lastnames, stud_names, stud_otch, SIZE_)
        character(:),                       allocatable ::  format
        character(LASTNAME_LEN, kind=CH_),  allocatable ::  stud_lastnames(:)
        character(NAME_LEN, kind=CH_),      allocatable ::  stud_names(:)
        character(OTCH_LEN, kind=CH_),      allocatable ::  stud_otch(:)

        integer(I_), intent(in)                         :: SIZE_

        intent(out) stud_lastnames, stud_names, stud_otch
        allocate(stud_lastnames(SIZE_), stud_names(SIZE_), stud_otch(SIZE_), mask(SIZE_))

        format = '(3(a, 1x))'
        open (file=file_input, encoding=E_, newunit=In)
            read (In, format, iostat=IO) (stud_lastnames(i), stud_names(i), &
                                          stud_otch(i), i = 1, SIZE_)
        close (In)
    end subroutine readData

    subroutine outFirstData(stud_lastnames, stud_names, stud_otch, SIZE_)
        character(:),         allocatable ::  format
        character(LASTNAME_LEN, kind=CH_) ::  stud_lastnames(:)
        character(NAME_LEN, kind=CH_)     ::  stud_names(:)
        character(OTCH_LEN, kind=CH_)     ::  stud_otch(:)
        integer(I_)                       ::  SIZE_

        intent(in) stud_lastnames, stud_names, stud_otch, SIZE_


        format = '(3(a, 1x))'
        open (file=file_output, encoding=E_, newunit=Out)
            write (Out, '(a)') "Исходный список:"
            write (Out, format, iostat=IO) (stud_lastnames(i), stud_names(i), &
                                            stud_otch(i), i = 1, SIZE_)
        close(Out)
    end subroutine outFirstData

    pure subroutine createMask (mask, stud_names, SIZE_)
        logical,                        intent(out) ::  mask(:)
        character(NAME_LEN, kind=CH_),  intent(in)  ::  stud_names(:)
        integer(I_),                    intent(in)  ::  SIZE_
        integer(I_)                                 ::  i, j

        mask = [(.TRUE., i=1, SIZE_)]
        do i = 1, SIZE_, 1
            do j = i+1, SIZE_, 1
                if(stud_names(i) == stud_names(j)) mask(j) = .FALSE.
            end do
        end do
    end subroutine createMask

    subroutine outSecondData(stud_lastnames, stud_names, stud_otch, mask, SIZE_)
        character(:),         allocatable ::  format
        logical                           ::  mask(:)
        character(LASTNAME_LEN, kind=CH_) ::  stud_lastnames(:)
        character(NAME_LEN, kind=CH_)     ::  stud_names(:)
        character(OTCH_LEN, kind=CH_)     ::  stud_otch(:)
        integer(I_)                       ::  SIZE_
        integer(I_)                       ::  i

        intent(in) stud_lastnames, stud_names, stud_otch, SIZE_, mask


        format = '(3(a, 1x))'
        open (file=file_output, encoding=E_, position="append", newunit=Out)
            write (Out, '(/,a)') "Отсортированный список:"

            do concurrent(i=1:SIZE_, mask(i))
            write (Out, format, iostat=IO)  stud_lastnames(i), &
                                            stud_names(i), &
                                            stud_otch(i)
            end do
        close(Out)

    end subroutine outSecondData
end program lab_1_1_v15