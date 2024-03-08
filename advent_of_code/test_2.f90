PROGRAM test
    IMPLICIT NONE

    character(10) :: input_1 = "467..114.."
    character(10) :: input_2 = "...*......" 
    character(10) :: input_3 = "..35..633."

    character, dimension(3, 10) :: input

    integer :: i

    do i = 1, len(input_2)
        input(1, i) = input_1(i:i)
        input(2, i) = input_2(i:i)
        input(3, i) = input_3(i:i)
    end do

    print *, findloc(input, "*")

END PROGRAM test