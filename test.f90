PROGRAM test
    IMPLICIT NONE

    integer :: status
    character(80) :: msg

    integer :: line = 1

    integer :: num
    character(3) :: num_cha
    integer :: sum = 0

    character(10) :: input_1 = ".........."!read in 3 lines at a time
    character(10) :: input_2 = ".........."
    character(10) :: input_3 = ".........."
    !integer :: ind_a
    !integer :: ind_b

    integer :: index
    logical, dimension(12) :: symbols = [0,0,0,0,0,0,0,0,0,0,0,0]
    logical, dimension(12) :: symbol_check

    OPEN(UNIT = 1, FILE = "input/day_3_test.txt", STATUS = "OLD", ACTION = "READ", IOSTAT = status, IOMSG = msg)
        status_check: IF (status == 0) THEN
            ! read in line of interest and the 2 surrounding
            !READ(1, '(A10)', IOSTAT = status) input_1
            READ(1, '(A10)', IOSTAT = status) input_2
            READ(1, '(A10)', IOSTAT = status) input_3
            print *,input_1
            print *,input_2
            print *,input_3

            symbols = symbol_check(input_1, input_2, input_3)

            IF (any(symbol_check)) THEN
                print *,"Number adjacent to a symbol!"
                read (num_cha,*) num
                sum = sum + num
            END IF

            print *,"Sum = ", sum

        END IF status_check
    CLOSE(UNIT = 1)

END PROGRAM

FUNCTION symbol_check(input_1, input_2, input_3)
    IMPLICIT NONE
    logical, dimension(12):: symbol_check

    character(10), intent(in) :: input_1, input_2, input_3
    
    integer :: ind_a, ind_b
    character(3) :: num_cha
    integer :: index
    !logical, dimension(12) :: symbol_check = [0,0,0,0,0,0,0,0,0,0,0,0]

    ind_a = scan(input_2, "0123456789")
    ind_b = scan(input_2(ind_a:ind_a+3), "0123456789", .true.)
    num_cha = input_2(ind_a:ind_b)
    print *,num_cha

    IF (ind_a == 1) THEN
        DO index = 2, 5
            symbol_check(index) = verify(input_1(index-ind_a:index-ind_a), ".")
            symbol_check(index+7) = verify(input_3(index-ind_a:index-ind_a), ".")
        END DO
        symbol_check(7) = verify(input_2(ind_b+1:ind_b+1), ".")
    ELSE IF (ind_b == 10) THEN
        DO index = 1, 4
            symbol_check(index) = verify(input_1(index-ind_a:index-ind_a), ".")
            symbol_check(index+7) = verify(input_3(index-ind_a:index-ind_a), ".")
        END DO
        symbol_check(6) = verify(input_2(ind_a-1:ind_a-1), ".")
    ELSE
        DO index = 1, 5
            symbol_check(index) = verify(input_1(index-ind_a:index-ind_a), ".")
            symbol_check(index+7) = verify(input_3(index-ind_a:index-ind_a), ".")
        END DO
        symbol_check(6) = verify(input_2(ind_a-1:ind_a-1), ".")
        symbol_check(7) = verify(input_2(ind_b+1:ind_b+1), ".")
    END IF
    print *,symbol_check

    RETURN
END FUNCTION symbol_check

