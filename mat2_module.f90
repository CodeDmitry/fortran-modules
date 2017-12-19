        module mat2_module
            use vec2_module
            implicit none        
            
            type mat2
                type(vec2), dimension(2) :: columns
            end type mat2
        contains                        
            subroutine mat2_set(obj, c1, c2)
                type(mat2), intent(out) :: obj
                type(vec2) :: c1
                type(vec2) :: c2
                
                obj%columns(1) = c1                    
                obj%columns(2) = c2                    
            end subroutine mat2_set
            
            subroutine mat2_dump(obj)
                type(mat2), intent(in) :: obj
            
                call vec2_dump(obj%columns(1))
                call vec2_dump(obj%columns(2))                
            end subroutine mat2_dump
        end module mat2_module