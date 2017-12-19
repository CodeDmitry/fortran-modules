        module mat4_module
            use vec4_module
            implicit none        
            
            type mat4
                type(vec4), dimension(4) :: columns
            end type mat4
        contains                        
            function mat4_create(c1, c2, c3, c4)
                type(mat4) :: mat4_create
                type(vec4), optional :: c1
                type(vec4), optional :: c2
                type(vec4), optional :: c3
                type(vec4), optional :: c4
                
                if (present(c1)) then
                    mat4_create%columns(1) = c1                    
                else
                    mat4_create%columns(1)%x = 0.0
                    mat4_create%columns(1)%y = 0.0
                    mat4_create%columns(1)%z = 0.0
                    mat4_create%columns(1)%w = 0.0
                endif
                
                if (present(c2)) then
                    mat4_create%columns(2) = c2                    
                else
                    mat4_create%columns(2)%x = 0.0
                    mat4_create%columns(2)%y = 0.0
                    mat4_create%columns(2)%z = 0.0
                    mat4_create%columns(2)%w = 0.0
                endif

                if (present(c3)) then
                    mat4_create%columns(3) = c3                    
                else
                    mat4_create%columns(3)%x = 0.0
                    mat4_create%columns(3)%y = 0.0
                    mat4_create%columns(3)%z = 0.0
                    mat4_create%columns(3)%w = 0.0
                endif

                if (present(c4)) then
                    mat4_create%columns(4) = c1                    
                else
                    mat4_create%columns(4)%x = 0.0
                    mat4_create%columns(4)%y = 0.0
                    mat4_create%columns(4)%z = 0.0
                    mat4_create%columns(4)%w = 0.0
                endif                
            end function mat4_create
        end module mat4_module