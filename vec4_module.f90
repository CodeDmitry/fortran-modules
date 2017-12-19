        module vec4_module
            implicit none        
            
            type vec4
                real :: x
                real :: y
                real :: z
                real :: w
            end type vec4
            
        contains                        
            subroutine vec4_set(obj, x, y, z, w)
                type(vec4), intent(out) :: obj
                real, optional, value :: x
                real, optional, value :: y                
                real, optional, value :: z
                real, optional, value :: w                
                
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
                
                if (present(z)) then               
                    obj%z = z
                else
                    obj%z = 0.0
                endif
                
                if (present(w)) then               
                    obj%w = w
                else
                    obj%w = 0.0
                endif                
            end subroutine vec4_set

            function vec4_add(u, v)                
                type(vec4) :: vec4_add
                type(vec4), intent(in) :: u
                type(vec4), intent(in) :: v
                
                vec4_add%x = u%x + v%x
                vec4_add%y = u%y + v%y
                vec4_add%z = u%z + v%z
                vec4_add%w = u%w + v%w
            end function vec4_add
            
            function vec4_subtract(u, v)                
                type(vec4) :: vec4_subtract
                type(vec4), intent(in) :: u
                type(vec4), intent(in) :: v
                
                vec4_subtract%x = u%x - v%x
                vec4_subtract%y = u%y - v%y
                vec4_subtract%z = u%z - v%z
                vec4_subtract%w = u%w - v%w
            end function vec4_subtract

            function vec4_dot(u, v)
                real :: vec4_dot
                type(vec4), intent(in) :: u
                type(vec4), intent(in) :: v
                
                vec4_dot = u%x*v%x + u%y*v%y + u%z*v%z + u%w*v%w
            end function vec4_dot
            
            function vec4_magnitude(u)
                real :: vec4_magnitude
                type(vec4), intent(in) :: u
                
                vec4_magnitude = sqrt(u%x**2 + u%y**2 + u%z**2 + u%w**2)
            end function vec4_magnitude
            
            subroutine vec4_dump(v)
                type(vec4), intent(in) :: v
            
                print *, v%x, v%y, v%z, v%w
            end subroutine vec4_dump
        end module vec4_module