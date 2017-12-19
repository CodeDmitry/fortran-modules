        module quat_module
            implicit none        
            
            type quat
                real :: x
                real :: y
                real :: z
                real :: w
            end type quat            
        contains                        
            function quat_create(x, y, z, w)
                type(quat) :: quat_create
                real, optional, value :: x
                real, optional, value :: y                
                real, optional, value :: z
                real, optional, value :: w                
                
                if (present(x)) then               
                    quat_create%x = x
                else
                    quat_create%x = 0.0
                endif
                
                if (present(y)) then               
                    quat_create%y = y
                else
                    quat_create%y = 0.0
                endif
                
                if (present(z)) then               
                    quat_create%z = z
                else
                    quat_create%z = 0.0
                endif
                
                if (present(w)) then               
                    quat_create%w = w
                else
                    quat_create%w = 0.0
                endif                
            end function quat_create
        end module quat_module