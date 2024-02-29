PROGRAM test
    IMPLICIT NONE

    character(140) :: input_1 = "......81$.....544..67...............*.....159....*............209.747*29........./..........812.........430.232...................199*587..."
    character(140) :: input_2 = "760.................*...#........331.................%...158...................#.....................29.................596...477..........." 
    character(140) :: input_3 = "..........%./....922...388..970...............510...68......*....590....545...621...-..977.593..889...*.........................=..879..*389"

    integer :: ind_a, ind_b, index, temp
    character(3) :: temp_cha
    integer ::  num = 0

    logical, dimension(5) :: symbol_1, symbol_3 = [0,0,0,0,0]
    logical, dimension(2) :: symbol_2 = [0,0]

    ind_a = scan(input_2, "0123456789")
    ind_b = scan(input_2(ind_a:ind_a+2), "0123456789", .true.) + ind_a - 1
    print *,ind_a
    print *,ind_b

    IF (ind_b < ind_a) THEN
        ind_b = 0
        temp_cha = input_2(ind_a:)
    ELSE
        temp_cha = input_2(ind_a:ind_b)
    END IF

    print *,temp_cha

    DO 
        IF (ind_a==1) THEN
            print *,"Number at beginning of line"
            DO index = 2, len(trim(temp_cha)) + 2
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
            END DO
            symbol_2(2) = verify(input_2(ind_b+1:ind_b+1), ".")
        ELSE IF (ind_b==0) THEN
            print *,"Number at end of line"
            DO index = 1, len(trim(temp_cha)) + 1
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
            END DO
            symbol_2(1) = verify(input_2(ind_a-1:ind_a-1), ".")
        ELSE
            print *,"Number within line"
            DO index = 1, len(trim(temp_cha)) + 2
                symbol_1(index) = verify(input_1(ind_a+index-2:ind_a+index-2), ".0123456789")
                symbol_3(index) = verify(input_3(ind_a+index-2:ind_a+index-2), ".0123456789")
            END DO
            symbol_2(1) = verify(input_2(ind_a-1:ind_a-1), ".")
            symbol_2(2) = verify(input_2(ind_b+1:ind_b+1), ".")
        END IF

        IF (any(symbol_1) .or. any(symbol_2) .or. any(symbol_3)) THEN
            print *,"Number adjacent to a symbol!"
            read (temp_cha,*) temp
            num = num + temp
        ELSE
            print *,"Number not adjacent to a symbol."
        END IF

        ind_a = scan(input_2(ind_b+1:), "0123456789") + ind_b
        IF (ind_a == ind_b .or. ind_b == 0) THEN
            print *,"Exiting"
            EXIT
        ELSE
            ind_b = scan(input_2(ind_a:ind_a+2), "0123456789", .true.) + ind_a - 1
            IF (ind_b == 140) THEN
                ind_b = 0
                temp_cha = input_2(ind_a:)
            ELSE
                temp_cha = input_2(ind_a:ind_b)
            END IF
            print *,temp_cha
            symbol_1 = [0,0,0,0,0]
            symbol_2 = [0,0]
            symbol_3 = [0,0,0,0,0]
        END IF
    END DO

    print *,"Num on this line: ",num

END PROGRAM test
