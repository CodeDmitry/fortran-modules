        module vec3_module
            implicit none        
            
            type vec3
                real :: x
                real :: y
                real :: z
            end type vec3
            
        contains                        
            subroutine vec3_set(obj, x, y, z)
                type(vec3), intent(out) :: obj
                real, optional, value :: x
                real, optional, value :: y                
                real, optional, value :: z
                
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
            end subroutine vec3_set

            function vec3_add(u, v)                
                type(vec3) :: vec3_add
                type(vec3), intent(in) :: u
                type(vec3), intent(in) :: v
                
                vec3_add%x = u%x + v%x
                vec3_add%y = u%y + v%y
                vec3_add%z = u%z + v%z
            end function vec3_add
            
            function vec3_subtract(u, v)                
                type(vec3) :: vec3_subtract
                type(vec3), intent(in) :: u
                type(vec3), intent(in) :: v
                
                vec3_subtract%x = u%x - v%x
                vec3_subtract%y = u%y - v%y
                vec3_subtract%z = u%z - v%z
            end function vec3_subtract

            function vec3_dot(u, v)
                real :: vec3_dot
                type(vec3), intent(in) :: u
                type(vec3), intent(in) :: v
                
                vec3_dot = u%x*v%x + u%y*v%y + u%z*v%z
            end function vec3_dot
            
            function vec3_magnitude(u)
                real :: vec3_magnitude
                type(vec3), intent(in) :: u
                
                vec3_magnitude = sqrt(u%x**2 + u%y**2 + u%z**2)
            end function vec3_magnitude
            
            subroutine vec3_dump(v)
                type(vec3), intent(in) :: v
            
                print *, v%x, v%y, v%z
            end subroutine vec3_dump
        end module vec3_module