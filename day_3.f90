PROGRAM day_3
    !finds all numbers in input file that are adjacent to symbols
    !adds them all up and outputs the result

    IMPLICIT NONE

    integer :: status
    character(80) :: msg

    integer :: line

    integer :: num = 0
    character(3) :: num_cha
    integer :: sum = 0

    character(10) :: input_1 !read in 3 lines at a time
    character(10) :: input_2 
    character(10) :: input_3 

    integer :: ind_a, ind_b

    OPEN(UNIT = 1, FILE = "input/day_3_test.txt", STATUS = "OLD", ACTION = "READ", IOSTAT = status, IOMSG = msg)
        status_check: IF (status == 0) THEN
            ! read in line of interest and the 2 surrounding
            input_1 = ".........."
            read(1, '(A10)', IOSTAT = status) input_2
            read(1, '(A10)', IOSTAT = status) input_3
            !print *,input_1
            !print *,input_2
            !print *,input_3

            DO line = 1, 10
                CALL num_if_symbol(input_1, input_2, input_3, num)
            
                sum = sum + num

                input_1 = input_2
                input_2 = input_3
                IF (line == 10) THEN
                    input_3 = ".........."
                ELSE
                    read(1, '(A10)', IOSTAT = status) input_3
                END IF

                print *,input_1
                print *,input_2
                print *,input_3
            END DO

            !CALL num_if_symbol(input_1, input_2, input_3, num)
            !sum = sum + num

            print *,"Sum = ", sum

        END IF status_check
    CLOSE(UNIT = 1)

END PROGRAM day_3

SUBROUTINE num_if_symbol(input_1, input_2, input_3, num)
    IMPLICIT NONE

    character(*), intent(in) :: input_1, input_2, input_3
    integer, intent(out) :: num
    
    logical, dimension(5) :: symbol_1, symbol_3 = [0,0,0,0,0]
    logical, dimension(2) :: symbol_2 = [0,0]
    integer :: ind_a, ind_b, temp
    character(3) :: temp_cha
    integer :: index

    ind_a = scan(input_2, "0123456789")
    ind_b = scan(input_2(ind_a:ind_a+3), "0123456789", .true.)
    temp_cha = input_2(ind_a:ind_b)

    DO
        !print *,input_1
        !print *,input_2
        !print *,input_3

        IF (ind_a == 1) THEN
            DO index = 2, len(temp_cha) + 2
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
            END DO
            symbol_2(2) = verify(input_2(ind_b+1:ind_b+1), ".")
        ELSE IF (ind_b == 10) THEN
            DO index = 1, len(temp_cha) + 1
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
            END DO
            symbol_2(1) = verify(input_2(ind_a-1:ind_a-1), ".")
        ELSE
            DO index = 1, len(temp_cha)+2
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
            END DO
            symbol_2(1) = verify(input_2(ind_a-1:ind_a-1), ".")
            symbol_2(2) = verify(input_2(ind_b+1:ind_b+1), ".")
        END IF
        !print *,symbol_1
        !print *,symbol_2
        !print *,symbol_3
    
        IF (any(symbol_1) .or. any(symbol_2) .or. any(symbol_3)) THEN
            !print *,"Number adjacent to a symbol!"
            read (temp_cha,*) temp
            num = num + temp
            !print *,temp_cha
        ELSE
            !print *,"Number not adjacent to a symbol."
            !print *,temp_cha
            num = num
        END IF
    
        ind_a = scan(input_2(ind_b+1:), "0123456789") + ind_b
        IF (ind_a == ind_b) THEN
            !print *,"Exiting"
            EXIT
        ELSE
            ind_b = scan(input_2(ind_a:ind_a+3), "0123456789", .true.) + ind_a
            symbol_1 = [0,0,0,0,0]
            symbol_2 = [0,0]
            symbol_3 = [0,0,0,0,0]
            temp_cha = input_2(ind_a:ind_b)
        END IF
    END DO

END SUBROUTINE num_if_symbol