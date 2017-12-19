        module mat3_module
            use vec3_module
            implicit none        
            
            type mat3
                type(vec3), dimension(3) :: columns
            end type mat3
        contains                        
            function mat3_create(c1, c2, c3)
                type(mat3) :: mat3_create
                type(vec3), optional :: c1
                type(vec3), optional :: c2
                type(vec3), optional :: c3
                
                if (present(c1)) then
                    mat3_create%columns(1) = c1                    
                else
                    mat3_create%columns(1)%x = 0.0
                    mat3_create%columns(1)%y = 0.0
                    mat3_create%columns(1)%z = 0.0
                endif

                if (present(c2)) then
                    mat3_create%columns(2) = c2                    
                else
                    mat3_create%columns(2)%x = 0.0
                    mat3_create%columns(2)%y = 0.0
                    mat3_create%columns(2)%z = 0.0
                endif

                if (present(c3)) then
                    mat3_create%columns(3) = c3                    
                else
                    mat3_create%columns(3)%x = 0.0
                    mat3_create%columns(3)%y = 0.0
                    mat3_create%columns(3)%z = 0.0
                endif
            end function mat3_create
        end module mat3_module