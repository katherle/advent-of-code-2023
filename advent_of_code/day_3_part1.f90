PROGRAM day_3
    ! finds all numbers in input file that are adjacent to symbols
    ! adds them all up and outputs the result
    ! a period does not count as a symbol

    IMPLICIT NONE

    integer :: status
    character(80) :: msg

    integer :: line ! line number

    integer :: num ! sum of all numbers on each line adjacent to symbols
    integer :: sum = 0 ! sum of all numbers in input file adjacent to symbols

    character(140) :: input_1 !read in 3 lines at a time
    character(140) :: input_2 
    character(140) :: input_3 

    OPEN(UNIT = 1, FILE = "input/day_3.txt", STATUS = "OLD", ACTION = "READ", IOSTAT = status, IOMSG = msg)
        status_check: IF (status == 0) THEN
            ! read in line of interest and the 2 surrounding
            ! start with a line containing no symbols so subroutine can check line 1
            input_1 = "............................................................................................................................................"
            read(1, '(A140)', IOSTAT = status) input_2
            read(1, '(A140)', IOSTAT = status) input_3

            file_iter: DO line = 1, 140
                num = 0
                print *,"line: ",line
                CALL num_if_symbol(input_1, input_2, input_3, num) ! subroutine sums all numbers on the line that are adjacent to symbols
                print *,"Num on this line: ", num
            
                sum = sum + num ! add result to existing sum

                input_1 = input_2
                input_2 = input_3
                IF (line == 140) THEN
                    ! insert line at end containing no symbols so subroutine can check the last line of the input
                    input_3 = "............................................................................................................................................"
                ELSE
                    read(1, '(A140)', IOSTAT = status) input_3
                END IF
            END DO file_iter

            print *,"Sum = ", sum

        END IF status_check
    CLOSE(UNIT = 1)

END PROGRAM day_3

SUBROUTINE num_if_symbol(input_1, input_2, input_3, num)
    IMPLICIT NONE

    ! finds all numbers on a given line (input_2) that are adjacent to symbols (even diagonally)
    ! outputs their sum
    ! numbers can be at the beginning or end of lines
    ! and can be up to 3 digits
    ! a period does not count as a symbol

    character(*), intent(in) :: input_1, input_2, input_3 ! input lines
    integer, intent(out) :: num ! sum output
    
    logical, dimension(5) :: symbol_1, symbol_3 = [0,0,0,0,0] ! is this a symbol?
    logical, dimension(2) :: symbol_2 = [0,0]
    integer :: ind_a, ind_b, temp ! indices of the number being checked + the number itself
    character(3) :: temp_cha ! character variable containing the number being checked
    integer :: index ! iterable, goes inside a do statement

    ind_a = scan(input_2, "0123456789")
    IF (ind_a == 0) THEN 
        ! no numbers in the line
        RETURN
    END IF

    ind_b = scan(input_2(ind_a:ind_a+2), "0123456789", .true.) + ind_a - 1 
    IF (ind_b < ind_a) THEN 
        ! this number is at the end of the line
        ind_b = 0
        temp_cha = input_2(ind_a:)
    ELSE
        temp_cha = input_2(ind_a:ind_b)
    END IF

    line_iter: DO
        symbol_check: IF (ind_a == 1) THEN
            ! number at beginning of line
            DO index = 2, len(trim(temp_cha)) + 2
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789") ! search for symbols in line above
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789") ! symboles in line below
            END DO
            symbol_2(2) = verify(input_2(ind_b+1:ind_b+1), ".") ! symbols in the same line
        ELSE IF (ind_b == 0) THEN
            ! number at end of line
            DO index = 1, len(trim(temp_cha)) + 1
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
            END DO
            symbol_2(1) = verify(input_2(ind_a-1:ind_a-1), ".")
        ELSE
            ! number within line
            DO index = 1, len(trim(temp_cha))+2
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
            END DO
            symbol_2(1) = verify(input_2(ind_a-1:ind_a-1), ".")
            symbol_2(2) = verify(input_2(ind_b+1:ind_b+1), ".")
        END IF symbol_check
    
        adjacent: IF (any(symbol_1) .or. any(symbol_2) .or. any(symbol_3)) THEN
            ! number is adjacent to a symbol
            read (temp_cha,*) temp !convert character variable to integer
            num = num + temp
        END IF adjacent
    
        ind_a = scan(input_2(ind_b+1:), "0123456789") + ind_b ! search for more numbers in the line
        get_next: IF (ind_a == ind_b .or. ind_b == 0) THEN
            ! no more numbers
            ! reset boolean array 
            symbol_1 = [0,0,0,0,0]
            symbol_2 = [0,0]
            symbol_3 = [0,0,0,0,0]
            RETURN
        ELSE
            ind_b = scan(input_2(ind_a:ind_a+2), "0123456789", .true.) + ind_a - 1 ! find the end of the next number
            IF (ind_b == len(input_2)) THEN
                ! this number is the last one in the line
                ind_b = 0
                temp_cha = input_2(ind_a:)
            ELSE
                temp_cha = input_2(ind_a:ind_b)
            END IF
            ! reset boolean array
            symbol_1 = [0,0,0,0,0]
            symbol_2 = [0,0]
            symbol_3 = [0,0,0,0,0]
        END IF get_next 
    END DO line_iter

END SUBROUTINE num_if_symbol