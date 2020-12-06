
module mod_point
    implicit none
    private
    public :: point, new_point

    type point
          integer :: x, y
    end type point

    contains

    pure function new_point(x, y)
            implicit none
            integer, intent(in) :: x , y
            type(point) :: new_point

            new_point%x = x
            new_point%y = y
    end function new_point  

end module mod_point