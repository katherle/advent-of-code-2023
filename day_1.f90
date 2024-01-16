PROGRAM day_1
    IMPLICIT NONE

    ! read in a txt file
    ! on each line is a string of integers and letters
    ! not all strings are the same length
    ! the first and last integer in each line make a 2-digit number
    ! find the sum of all these numbers
    ! WRITE (*,*) 'Result = ', sum

    INTEGER i ! line in calibration file
    INTEGER :: ind_a ! index of first integer in calib
    INTEGER :: ind_b ! index of last integer in calib
    CHARACTER(2) :: c ! concatenation 
    INTEGER :: int_c ! integer version of c
    INTEGER :: s = 0 ! sum over all c
    INTEGER :: status
    CHARACTER(80) :: msg
    CHARACTER(100) :: calib

    OPEN (UNIT = 1, FILE = "input/day_1.txt", STATUS = "OLD", ACTION = "READ", IOSTAT = status, IOMSG = msg)
        IF (status == 0) THEN
            DO i = 1, 1000
                READ (1, *, IOSTAT = status) calib ! get next line in text file
                IF (status /= 0) EXIT ! if not valid then exit

                ind_a = SCAN(calib, "0123456789") ! scan through calib finding the first digit
                ind_b = SCAN(calib, "0123456789", .TRUE.) ! scan through calib finding the last digit

                c = calib(ind_a:ind_a) // calib(ind_b:ind_b)
                read (c, '(I2)') int_c

                s = s + int_c
            END DO
            WRITE(*,*) "result: ", s
        END IF
    CLOSE(UNIT = 1)

END PROGRAM day_1