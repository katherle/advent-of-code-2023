PROGRAM day_3
    IMPLICIT NONE

    !find each number in the input file (up to 3 digits)
    !check if adjacent to a symbol
    !if so, add it to the sum

    integer :: status
    character(80) :: msg

    integer :: line = 1

    integer :: num
    character(3) :: num_cha
    bool, dimension(12) :: symbol_check

    integer :: sum
    character, dimension(140, 3) :: input !read in 3 lines at a time

    OPEN(UNIT = 1, FILE = "input/day_3.txt", STATUS = "OLD", ACTION = "READ", IOSTAT = status, IOMSG = msg)
        status_check: IF (status == 0) THEN
            ! read in line of interest and the 2 surrounding
            line_iter: DO
                IF (line = 1) THEN
                    READ(1, 'A140', IOSTAT = status) input
            END DO line_iter
        END IF status_check
    CLOSE(UNIT = 1)
    
END PROGRAM