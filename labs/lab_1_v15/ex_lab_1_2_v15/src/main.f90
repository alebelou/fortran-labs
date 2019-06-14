program lab_1_2_v15
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
    integer(I_)                     ::  i, j, IO

    character(:),                       allocatable ::  format
    character(LASTNAME_LEN, kind=CH_),  allocatable ::  stud_lastnames(:)
    character(NAME_LEN, kind=CH_),      allocatable ::  stud_names(:)
    character(OTCH_LEN, kind=CH_),      allocatable ::  stud_otch(:)
    logical,                            allocatable ::  mask(:)

    format = '(3(a, 1x))'
    write(*, *) format
    open (file=file_input, encoding=E_, newunit=In)
        allocate(stud_lastnames(SIZE_), stud_names(SIZE_), stud_otch(SIZE_), mask(SIZE_))
        read (In, format, iostat=IO) (stud_lastnames(i), stud_names(i), &
                                      stud_otch(i), i = 1, SIZE_)
    close (In)

    open (file=file_output, encoding=E_, newunit=Out)
        write (Out, '(a)') "Исходный список:"
        write (Out, format, iostat=IO) (stud_lastnames(i), stud_names(i), &
                                      stud_otch(i), i = 1, SIZE_)
    close(Out)

    mask = [(.TRUE., i=1, SIZE_)]
    do i = 1, SIZE_, 1
        do j = i+1, SIZE_, 1
            if(stud_names(i) == stud_names(j)) mask(j) = .FALSE.
        end do
    end do

    open (file=file_output, encoding=E_, position="append", newunit=Out)
        write (Out, '(/,a)') "Отсортированный список:"

        do concurrent(i=1:SIZE_, mask(i))
        write (Out, format, iostat=IO) stud_lastnames(i), &
                                        stud_names(i), &
                                        stud_otch(i)
        end do
    close(Out)

    deallocate(stud_lastnames, stud_otch, stud_names, mask)
end program lab_1_2_v15