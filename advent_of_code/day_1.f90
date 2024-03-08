PROGRAM day_1
    IMPLICIT NONE

    ! read in a txt file
    ! on each line is a string of numbers and letters
    ! not all strings are the same length
    ! the first and last integer in each line make a 2-digit number
    ! find the sum of all these numbers
    ! careful–some integers are spelled out in letters and some lines only have one integer

    INTEGER i ! line in calibration file
    INTEGER j ! element of nums
    CHARACTER(100) :: calib
    INTEGER :: ind_a ! index of first integer in calib
    INTEGER :: ind_b ! index of last integer in calib
    CHARACTER(5), DIMENSION(9) :: nums
    CHARACTER(1), DIMENSION(9) :: int_nums
    INTEGER :: ind_nums_a ! index of element in nums, searching from front
    INTEGER :: ind_nums_b ! index of element in nums, searching from back
    CHARACTER(1) :: calib_a ! value of first integer in calib
    CHARACTER(1) :: calib_b ! value of last integer in calib
    CHARACTER(2) :: c ! concatenation of calib_a and calib_b
    INTEGER :: int_c ! integer version of c
    INTEGER :: s = 0 ! sum over all c
    INTEGER :: status
    CHARACTER(80) :: msg

    nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    int_nums = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

    OPEN (UNIT = 1, FILE = "input/day_1.txt", STATUS = "OLD", ACTION = "READ", IOSTAT = status, IOMSG = msg)
        IF (status == 0) THEN
            file_iter: DO i = 1, 1000
                READ (1, *, IOSTAT = status) calib ! get next line in text file
                IF (status /= 0) EXIT ! if not valid then exit

                ind_a = SCAN(calib, "0123456789") ! scan through calib finding the first digit
                ind_b = SCAN(calib, "0123456789", BACK = .TRUE.) ! scan through calib finding the last digit
            
                calib_a = calib(ind_a:ind_a)
                calib_b = calib(ind_b:ind_b)

                !search through the line for a number
                number_iter: DO j = 1, 9
                    ind_nums_a = INDEX(calib, TRIM(nums(j)))
                    ind_nums_b = INDEX(calib, TRIM(nums(j)), .TRUE.)
                    IF (ind_a == 0 .AND. ind_nums_a /= 0) THEN
                        !there is no number in the line, but there is at least one spelled-out integer
                        ind_a = ind_nums_a
                        ind_b = ind_nums_b
                        calib_a = int_nums(j)
                        calib_b = int_nums(j)
                    END IF
                    IF (ind_nums_a < ind_a .AND. ind_nums_a /= 0) THEN
                        !the first integer is spelled out, but there is a number in the line
                        ind_a = ind_nums_a
                        calib_a = int_nums(j)
                    END IF
                    IF (ind_nums_b > ind_b .AND. ind_nums_b /= 0) THEN 
                        !the last integer is spelled out, but there is a number in the line
                        ind_b = ind_nums_b
                        calib_b = int_nums(j)
                    END IF
                END DO number_iter

                c = calib_a // calib_b !concatenate the first and last integers
            
                read (c, '(I2)') int_c !convert character to integer type

                s = s + int_c !add to sum
            END DO file_iter
            WRITE(*,*) "result: ", s
        END IF
    CLOSE(UNIT = 1)

END PROGRAM day_1