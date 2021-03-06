        module vec2_module
            implicit none        
            
            type vec2
                real :: x
                real :: y
            end type vec2
        contains                        
            subroutine vec2_set(obj, x, y)
                type(vec2), intent(out) :: obj
                real, optional, value :: x
                real, optional, value :: y                
                
                if (present(x)) then
                    obj%x = x
                else
                    obj%x = 0.0
                endif
                
                if (present(y)) then
                    obj%y = y
                else
                    obj%y = 0.0
                endif
            end subroutine vec2_set

            function vec2_add(u, v)                
                type(vec2) :: vec2_add
                type(vec2), intent(in) :: u
                type(vec2), intent(in) :: v
                
                vec2_add%x = u%x + v%x
                vec2_add%y = u%y + v%y
            end function vec2_add
            
            function vec2_subtract(u, v)                
                type(vec2) :: vec2_subtract
                type(vec2), intent(in) :: u
                type(vec2), intent(in) :: v
                
                vec2_subtract%x = u%x - v%x
                vec2_subtract%y = u%y - v%y
            end function vec2_subtract
            
            function vec2_dot(u, v)
                real :: vec2_dot
                type(vec2), intent(in) :: u
                type(vec2), intent(in) :: v
                
                vec2_dot = u%x*v%x + u%y*v%y
            end function vec2_dot
            
            function vec2_magnitude(u)
                real :: vec2_magnitude
                type(vec2), intent(in) :: u
                
                vec2_magnitude = sqrt(u%x**2 + u%y**2)
            end function vec2_magnitude
            
            subroutine vec2_dump(v)
                type(vec2), intent(in) :: v
            
                print *, v%x, v%y
            end subroutine vec2_dump
        end module vec2_module