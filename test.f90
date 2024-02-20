PROGRAM test
    IMPLICIT NONE

    character(10) :: input_1 = ".........."
    character(10) :: input_2 = "467..114.." 
    character(10) :: input_3 = "...*......"

    integer ind_a, ind_b

    ind_a = scan(input_2, "0123456789")
    ind_b = scan(input_2(ind_a:ind_a+3), "0123456789", .true.)
    print *,ind_a
    print *,ind_b

    print *,input_2

    DO
        ind_a = scan(input_2(ind_b+1:), "0123456789") + ind_b
        IF (ind_a == ind_b) THEN
            print *,"No other numbers in the line"
            EXIT
        ELSE
            ind_b = scan(input_2(ind_a:ind_a+3), "0123456789", .true.) + ind_a
            print *,ind_a
            print *,ind_b
        END IF
    END DO

END PROGRAM test
