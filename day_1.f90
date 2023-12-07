PROGRAM day_1
    IMPLICIT NONE

    ! read in a txt file
    ! on each line is a string of integers and letters
    ! not all strings are the same length
    ! the first and last integer in each line make a 2-digit number
    ! find the sum of all these numbers
    ! WRITE (*,*) 'Result = ', sum

    INTEGER :: status, 
    CHARACTER(*) :: ind_a, ind_b, a, b 
    INTEGER :: c
    INTEGER :: sum = 0
    CHARACTER(80) :: msg
    CHARACTER(len = *) :: calib

    OPEN (UNIT = 1, FILE = "input/day_1.txt", STATUS = "OLD", ACTION = "READ", IOSTAT = status, IOMSG = msg)
        IF (status == 0) THEN
            DO i = 1, 1000
                READ (1, *, IOSTAT = status) calib ! get next line in text file
                IF (status /= 0) EXIT ! if not valid then exit
                ind_a = SCAN(calib, "0123456789") ! scan through calib finding the first digit
                ind_b = SCAN(calib, "0123456789", .TRUE.) ! scan through calib finding the last digit

                a = calib(iachar(ind_a))
                b = calib(iachar(ind_b))
                c = iachar(a//b) ! concatenate a and b and convert to type integer
                sum = sum + c
            END DO
    CLOSE(UNIT = 1)
    WRITE(*, *) 'Result = '‚ c
END PROGRAM day_1