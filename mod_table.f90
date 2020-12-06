module mod_table

      implicit none
      private
      public :: table, new_table, is_on

      type table
            integer :: x, y, xx, yy
      end type table

      contains

      pure function new_table(x, y)
            integer, intent(in) :: x , y
            type(table) :: new_table

            new_table%x = 0
            new_table%y = 0
            new_table%xx = x
            new_table%yy = y
      end function new_table

      pure function is_on(t, x, y)
            integer, intent(in) :: x , y
            type(table), intent(in) :: t
            logical :: is_on

            if ( (x >= t%x .and. y >= t%y) .and. (x <= t%xx .and. y <= t%yy)) then
                  is_on = .true.
            else
                  is_on = .false.
            end if
      end function is_on

end module mod_table