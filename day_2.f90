PROGRAM day_2
    IMPLICIT NONE

    ! each line is a string containing a game id number 
    ! plus a semicolon-separated list of subsets of cubes removed from a bag 
    ! the number of cubes in the bag of each color is unknown, but each game contains several draws
    ! part 1: find which games would have been possible given a maximum number of cubes in the bag
    ! and sum their IDs
    ! part 2: we want to find the fewest number of cubes of each color in the bag that makes the game possible
    ! multiply the minimum number of each color together and sum the result over all games

    integer :: status
    character(80) :: msg

    integer :: i! iterate over lines in file
    character(200) :: game ! content of one line
    integer, dimension(3) :: cubes = [0, 0, 0] ! order is red, green, blue
    !integer, dimension(3) :: max = [12, 13, 14] ! maximum number of cubes in the bag (part 1; no longer relevant to part 2)
    
    integer :: red_i
    integer :: green_i
    integer :: blue_i
    character(3) :: temp
    integer :: temp_int
    !integer :: id_start
    !integer :: id_end
    logical :: possible = .true. !only relevant to part 1

    integer :: p_a
    integer :: p_b
    integer :: power
    integer :: power_sum = 0

    OPEN(UNIT = 1, FILE = "input/day_2.txt", STATUS = "OLD", ACTION = "READ", IOSTAT = status, IOMSG = msg)
        status_check: IF (status == 0) THEN
            file_iter: DO i = 1, 100 !for each line in the input file
                READ(1, '(A200)', IOSTAT = status) game
                WRITE(*,*) game !check we read it in correctly
                IF (STATUS /= 0) EXIT
                p_a = 1

                game_loop: DO
                    p_b = index(game(p_a:), ";") + p_a - 1
                    iterate: IF (p_b > p_a) THEN
                        !there is another semicolon after this one
                        !find red green and blue indices
                        red_i = INDEX(game(p_a:p_b), "red") + p_a - 1
                        green_i = INDEX(game(p_a:p_b), "green") + p_a - 1
                        blue_i = INDEX(game(p_a:p_b), "blue") + p_a - 1

                        red: IF (red_i /= p_a - 1) THEN
                            !red cubes were drawn; find the number
                            temp = trim(game(red_i-3:red_i-1))
                            read (temp,*) temp_int
                            if (temp_int > cubes(1)) then
                                cubes(1) = temp_int !assign the integer number of red cubes to the first element of cubes array
                            end if
                        END IF red

                        green: IF (green_i /= p_a - 1) THEN
                            temp = trim(game(green_i-3:green_i-1))
                            read (temp,*) temp_int
                            if (temp_int > cubes(2)) then
                                cubes(2) = temp_int
                            end if
                        END IF green

                        blue: IF (blue_i /= p_a - 1) THEN
                            temp = trim(game(blue_i-3:blue_i-1))
                            read (temp,*) temp_int
                            if (temp_int > cubes(3)) then
                                cubes(3) = temp_int
                            end if
                        END IF blue

                        !print *,cubes

                        p_a = p_b + 1

                    ELSE
                        !last draw; repeat procedure
                        red_i = INDEX(game(p_b:), "red") + p_b - 1
                        green_i = INDEX(game(p_b:), "green") + p_b - 1
                        blue_i = INDEX(game(p_b:), "blue") + p_b - 1

                        red_last: IF (red_i /= p_b - 1) THEN
                            temp = trim(game(red_i-3:red_i-1))
                            read (temp,*) temp_int
                            if (temp_int > cubes(1)) then
                                cubes(1) = temp_int
                            end if
                        END IF red_last

                        green_last: IF (green_i /= p_b - 1) THEN
                            temp = trim(game(green_i-3:green_i-1))
                            read (temp,*) temp_int
                            if (temp_int > cubes(2)) then
                                cubes(2) = temp_int
                            end if
                        END IF green_last

                        blue_last: IF (blue_i /= p_b - 1) THEN
                            temp = trim(game(blue_i-3:blue_i-1))
                            read (temp,*) temp_int
                            if (temp_int > cubes(3)) then
                                cubes(3) = temp_int
                            end if
                        END IF blue_last

                        print *,cubes
                        exit

                    END IF iterate
                END DO game_loop

                power = product(cubes)
                power_sum = power_sum + power
                cubes = [0, 0, 0]

            END DO file_iter
        
        END IF status_check
            
    CLOSE(UNIT = 1)

    WRITE(*,*) "Result: ", power_sum

END PROGRAM