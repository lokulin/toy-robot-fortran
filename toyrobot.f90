program toyrobot
      use mod_table
      use mod_point
      use mod_robot

      implicit none
      type(table) :: mytable
      type(robot) :: myrobot
      
      integer, parameter :: strlen = 100
      integer :: status, i = 1
      character(len=strlen) :: cmd = ""
      character(len=strlen) :: args = ""
       
      integer :: sep_pos

      integer :: x,y = -2
      real :: facing = -1.0

      mytable = new_table(5, 5)
      myrobot = new_robot(-2,-2,90.0,mytable)  

      do
            cmd = ""
            args = ""
            i = 1
            sep_pos = 0
            x = -2
            y = -2
            do
                  call fget(cmd(i:i), status)
                  if (status /= 0 .or. i > strlen .or. cmd(i:i) == new_line('')) exit
                  i = i + 1
            end do

            cmd = cmd(1:i-1)

            ! find command
            sep_pos = index(cmd, " ")
            if (sep_pos .gt. 0 .and. sep_pos .ne. i) then
                  args = cmd(sep_pos+1:i-1)
                  i = sep_pos
                  cmd = cmd(1:i)
            end if

            select case (cmd)
                  case ("PLACE")
                        ! find x argument
                        sep_pos = index(args, ",")
                        if (sep_pos .gt. 0 .and. sep_pos .ne. len(args)) then
                              if (is_int(args(1:sep_pos-1))) read(args(1:sep_pos-1),*) x
                              args = args(sep_pos+1:)
                              ! find y argument
                              sep_pos = index(args, ",")
                              if (sep_pos .gt. 0 .and. sep_pos .ne. len(args)) then
                                    if (is_int(args(1:sep_pos-1))) read(args(1:sep_pos-1),*) y
                                    args = args(sep_pos+1:)
                                    ! find facing
                                    facing = cardinal_to_degree(args(1:5))
                                    if (facing .ne. -1) myrobot = robot_place(myrobot,x,y,facing, mytable)
                              end if
                        end if
                  case ("MOVE")
                        myrobot = robot_move(myrobot)
                  case ("LEFT")
                        myrobot = robot_left(myrobot)
                  case ("RIGHT")
                        myrobot = robot_right(myrobot)
                  case ("REPORT")
                        myrobot = robot_report(myrobot)
                  case ("EXIT")
                        exit
                  case default
                        !noop
            end select
      end do

      contains

      pure function is_int(string)
            implicit none
            character(len=*), intent(in) :: string
            logical :: is_int
            integer :: x
            integer :: e

            is_int = .false.
            read(string, *, iostat=e) x
            is_int = e == 0
      end function is_int

      pure function cardinal_to_degree(string)
            implicit none
            character(len=*), intent(in) :: string
            real :: cardinal_to_degree

            select case (string)
                  case ("NORTH")
                        cardinal_to_degree = 0.0
                  case ("EAST")
                        cardinal_to_degree = 90.0
                  case ("SOUTH")
                        cardinal_to_degree = 180.0
                  case ("WEST")
                        cardinal_to_degree = 270.0
                  case default
                        cardinal_to_degree = -1.0
            end select
      end function cardinal_to_degree

end program toyrobot
