module mod_robot
    use mod_table

    implicit none

    private
    public :: robot, new_robot, robot_place, robot_move, robot_left, robot_right, robot_report

    type robot
          type(table) :: table
          integer :: x, y
          real :: facing
    end type robot

    contains

    pure function new_robot(x, y, f, t)
            implicit none
            integer, intent(in) :: x , y
            real, intent(in) :: f
            type(table), intent(in) :: t
            type(robot) :: new_robot

            new_robot%x = x
            new_robot%y = y
            new_robot%table = t
            new_robot%facing = f
    end function new_robot

    pure function robot_place(r, x, y, f, t)
        implicit none
        integer, intent(in) :: x , y
        real, intent(in) :: f
        type(table), intent(in) :: t
        type(robot), intent(in) :: r
        type(robot) :: robot_place

        if (is_on(t, x, y)) then
            robot_place = new_robot(x, y, f, t)
        else
            robot_place = r
        endif
    end function robot_place

    pure function robot_move(r)
        implicit none
        type(robot), intent(in) :: r
        type(robot) :: robot_move
        robot_move = robot_place(r, r%x + floor(sind(r%facing)), r%y + floor(cosd(r%facing)), r%facing, r%table)
    end function robot_move

    pure function robot_left(r)
        implicit none
        type(robot), intent(in) :: r
        type(robot) :: robot_left
        robot_left = robot_place(r, r%x, r%y, fmod(r%facing - 90, 360.0), r%table)
    end function robot_left

    pure function robot_right(r)
        implicit none
        type(robot), intent(in) :: r
        type(robot) :: robot_right
        robot_right = robot_place(r, r%x, r%y, fmod(r%facing + 90, 360.0), r%table)
    end function robot_right

    function robot_report(r)
        implicit none
        type(robot), intent(in) :: r
        type(robot) :: robot_report
        if (is_on(r%table, r%x, r%y)) then
            print "(i0,',',i0,',',a)", r%x,r%y,degree_to_cardinal(r%facing)
        end if
        robot_report = r
    end function robot_report

    pure function fmod(x, y)
        implicit none
        real, intent(in) :: x, y
        real :: fmod
        fmod = x - y * floor(x/y)
    end function fmod

    pure function degree_to_cardinal(facing)
        implicit none
        real, intent(in) :: facing
        character(5) :: degree_to_cardinal
        select case (floor(facing))
            case (0)
                degree_to_cardinal = "NORTH"
            case (90)
                degree_to_cardinal = "EAST"
            case (180)
                degree_to_cardinal = "SOUTH"
            case (270)
                degree_to_cardinal = "WEST"
        end select
    end function degree_to_cardinal

end module mod_robot