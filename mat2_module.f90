        module mat2_module
            use vec2_module
            implicit none        
            
            type mat2
                type(vec2), dimension(2) :: columns
            end type mat2
        contains                        
            function mat2_create(c1, c2)
                type(mat2) :: mat2_create
                type(vec2), optional :: c1
                type(vec2), optional :: c2
                
                if (present(c1)) then
                    mat2_create%columns(1) = c1                    
                else
                    mat2_create%columns(1)%x = 0.0
                    mat2_create%columns(1)%y = 0.0
                endif
                                
                if (present(c2)) then
                    mat2_create%columns(2) = c2                    
                else
                    mat2_create%columns(2)%x = 0.0
                    mat2_create%columns(2)%y = 0.0
                endif
            end function mat2_create
        end module mat2_module