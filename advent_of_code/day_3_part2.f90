PROGRAM day_3
    ! finds all asterisks in input file and checks whether they're adjacent to exactly 2 numbers
    ! if so, multiplies the numbers
    ! adds the result of all of these products to produce final answer

    IMPLICIT NONE

    integer :: status
    character(80) :: msg

    integer :: line ! line number

    integer :: ratio ! sum of all gear ratios on each line
    integer :: sum = 0 ! sum of all gear ratios in input file

    character(10) :: input_1 !read in 3 lines at a time
    character(10) :: input_2 
    character(10) :: input_3 

    OPEN(UNIT = 1, FILE = "input/day_3_test.txt", STATUS = "OLD", ACTION = "READ", IOSTAT = status, IOMSG = msg)
        status_check: IF (status == 0) THEN
            ! read in line of interest and the 2 surrounding
            ! start with a line containing no symbols so subroutine can check line 1
            input_1 = "............................................................................................................................................"
            read(1, '(A10)', IOSTAT = status) input_2
            read(1, '(A10)', IOSTAT = status) input_3

            file_iter: DO line = 1, 10
                ratio = 0
                print *,"line: ",line
                CALL gear_ratio(input_1, input_2, input_3, ratio) ! subroutine sums all numbers on the line that are adjacent to symbols
                print *,"Sum of ratios on this line: ", ratio
            
                sum = sum + ratio ! add result to existing sum

                input_1 = input_2
                input_2 = input_3
                IF (line == 10) THEN
                    ! insert line at end containing no symbols so subroutine can check the last line of the input
                    input_3 = "............................................................................................................................................"
                ELSE
                    read(1, '(A10)', IOSTAT = status) input_3
                END IF
            END DO file_iter

            print *,"Sum = ", sum

        END IF status_check
    CLOSE(UNIT = 1)

END PROGRAM day_3

SUBROUTINE gear_ratio(input_1, input_2, input_3, ratio)
    IMPLICIT NONE

    ! checks for asterisks (*) on line input_2
    ! if present, checks whether each is adjacent to exactly 2 numbers
    ! if both conditions are met, multiply the numbers together to get the gear ratio
    ! if multiple asterisks on the line are adjacent to exactly 2 numbers, sum their gear ratios

    character(*), intent(in) :: input_1, input_2, input_3 ! input lines
    integer, intent(out) :: ratio ! sum of gear ratios on this line
    
    logical, dimension(8) :: number = [0,0,0,0,0,0,0,0] ! is this a number?

    integer :: ind_gear

    integer :: ind_a, ind_b, temp ! indices of the number being checked + the number itself
    character(3) :: temp_cha ! character variable containing the number being checked
    integer :: index ! iterable, goes inside a do statement

    ind_gear = scan(input_2, "*")
    IF (ind_gear == 0) THEN 
        ! no gears in the line
        RETURN
    END IF

    line_iter: DO
        number_check: IF (ind_gear == 1) THEN
            ! gear at beginning of line
            DO index = 2, 3
                number(index) = scan(input_1(index-1:index-1), "0123456789") ! checks whether number present in line above
                number(index+5) = scan(input_3(index-1:index-1), "0123456789") ! number in line below
            END DO
            number(5) = scan(input_2(2:2), "0123456789") ! numbers in the same line
        ELSE IF (ind_gear == len(input_2)) THEN
            ! gear at end of line
            DO index = 1, 2
                number(index) = scan(input_1(ind_gear+index-2:ind_gear+index-2), "0123456789")
                number(index+5) = scan(input_3(ind_gear+index-2:ind_gear+index-2), "0123456789")
            END DO
            number(4) = scan(input_2(ind_gear-1:ind_gear-1), "0123456789")
        ELSE
            ! gear within line
            DO index = 1, 3
                number(index) = scan(input_1(ind_gear+index-2:ind_gear+index-2), "0123456789")
                number(index+5) = scan(input_3(ind_gear+index-2:ind_gear+index-2), "0123456789")
            END DO
            number(4) = scan(input_2(ind_a-1:ind_a-1), "0123456789")
            number(5) = scan(input_2(ind_b+1:ind_b+1), "0123456789")
        END IF number_check

        number_1 = scan(number, .true.)
        
    END DO line_iter

END SUBROUTINE num_if_symbol