PROGRAM test
    IMPLICIT NONE

    character(10) :: input_1 = "467..114.."
    character(10) :: input_2 = "...*......" 
    character(10) :: input_3 = "..35..633."

    integer :: ind_gear, number_1
    
    integer :: ind_a, ind_b, index, temp
    character(3) :: temp_cha
    integer ::  ratio = 0

    logical, dimension(8) :: number = [0,0,0,0,0,0,0,0]

    ind_gear = scan(input_2, "*")

    number_check IF (ind_gear == 1) THEN
        ! gear at beginning of line
        
    ELSE IF (ind_gear == len(input_2)) THEN
        ! gear at end of line
        
    ELSE
        ! gear within line
        if (any(input_1(ind_gear-1:ind_gear+1) ))
        
    END IF number_check

    print *,number

END PROGRAM test
