PROGRAM day_3
    ! finds all numbers in input file that are adjacent to symbols
    ! adds them all up and outputs the result
    ! a period does not count as a symbol
    ! haven't finished solving this one yet

    IMPLICIT NONE

    integer :: status
    character(80) :: msg

    integer :: line ! line number

    integer :: num ! sum of all numbers on each line adjacent to symbols
    integer :: sum = 0 ! sum of all numbers in input file adjacent to symbols

    character(12) :: input_1 !read in 3 lines at a time
    character(12) :: input_2 
    character(12) :: input_3 

    OPEN(UNIT = 1, FILE = "input/day_3_test2.txt", STATUS = "OLD", ACTION = "READ", IOSTAT = status, IOMSG = msg)
        status_check: IF (status == 0) THEN
            ! read in line of interest and the 2 surrounding
            ! start with a line containing no symbols so subroutine can check line 1
            !input_1 = "............................................................................................................................................"
            input_1 = "............"
            read(1, '(A12)', IOSTAT = status) input_2
            read(1, '(A12)', IOSTAT = status) input_3
            !print *,input_1
            !print *,input_2
            !print *,input_3

            DO line = 1, 12
                num = 0
                !print *,input_1
                !print *,input_2
                !print *,input_3
                print *,"line: ",line
                CALL num_if_symbol(input_1, input_2, input_3, num) ! subroutine sums all numbers on the line that are adjacent to symbols
                print *,"Num on this line: ", num
            
                sum = sum + num ! add result of subroutine to existing sum

                input_1 = input_2
                input_2 = input_3
                IF (line == 11) THEN
                    ! insert line at end containing no symbols so subroutine can check the last line of the input
                    !input_3 = "............................................................................................................................................"
                    input_3 = "............"
                ELSE
                    read(1, '(A12)', IOSTAT = status) input_3
                END IF
            END DO

            !CALL num_if_symbol(input_1, input_2, input_3, num)
            !sum = sum + num

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
        print *,"no numbers on this line"
        RETURN
    ELSE
        print *,"numbers exist"
    END IF

    ind_b = scan(input_2(ind_a:ind_a+3), ".") + ind_a - 2 
    IF (ind_b < ind_a) THEN
        ind_b = 0
        temp_cha = input_2(ind_a:)
    ELSE
        temp_cha = input_2(ind_a:ind_b)
    END IF
    !print *,temp_cha

    DO
        !print *,input_1
        !print *,input_2
        !print *,input_3
        !print *,ind_a, ind_b

        IF (ind_a == 1) THEN
            print *,"number at beginning of line"
            DO index = 2, len(trim(temp_cha)) + 2
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
            END DO
            symbol_2(2) = verify(input_2(ind_b+1:ind_b+1), ".")
        ELSE IF (ind_b == 0) THEN
            print *,"number at end of line"
            print *,ind_a,ind_b
            DO index = 1, len(trim(temp_cha)) + 2
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
                !print *,input_3(ind_a+index-2:ind_a+index-2)
            END DO
            symbol_2(1) = verify(input_2(ind_a-1:ind_a-1), ".")
        ELSE
            print *,"number within line"
            !print *,len(trim(temp_cha))
            !print *,ind_a,ind_b
            DO index = 1, len(trim(temp_cha))+2
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
            END DO
            symbol_2(1) = verify(input_2(ind_a-1:ind_a-1), ".")
            symbol_2(2) = verify(input_2(ind_b+1:ind_b+1), ".")
            !print *,input_2(ind_b:ind_b)
        END IF
    
        IF (any(symbol_1) .or. any(symbol_2) .or. any(symbol_3)) THEN
            print *,"Number adjacent to a symbol!"
            print *,temp_cha
            read (temp_cha,*) temp
            num = num + temp
            !print *,ind_a,ind_b
        ELSE
            print *,"Number not adjacent to a symbol."
            print *,temp_cha
            num = num
        END IF
        !print *,num
    
        ind_a = scan(input_2(ind_b+1:), "0123456789") + ind_b
        IF (ind_a == ind_b .or. ind_b == 0) THEN
            print *,"Exiting"
            symbol_1 = [0,0,0,0,0]
            symbol_2 = [0,0]
            symbol_3 = [0,0,0,0,0]
            RETURN
        ELSE
            ind_b = scan(input_2(ind_a:ind_a+2), ".") + ind_a - 2
            IF (ind_b < ind_a) THEN
                ind_b = 0
                temp_cha = input_2(ind_a:)
            ELSE
                temp_cha = input_2(ind_a:ind_b)
            END IF
            symbol_1 = [0,0,0,0,0]
            symbol_2 = [0,0]
            symbol_3 = [0,0,0,0,0]
            temp_cha = trim(input_2(ind_a:ind_b))
            !temp_cha = trim(temp_cha)
        END IF
    END DO

END SUBROUTINE num_if_symbol