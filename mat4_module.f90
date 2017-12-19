        module mat4_module
            use vec4_module
            implicit none        
            
            type mat4
                type(vec4), dimension(4) :: columns
            end type mat4
        contains                        
            subroutine mat4_set(obj, c1, c2, c3, c4)
                type(mat4) :: obj
                type(vec4), intent(in) :: c1
                type(vec4), intent(in) :: c2
                type(vec4), intent(in) :: c3
                type(vec4), intent(in) :: c4
                
                obj%columns(1) = c1                    
                obj%columns(2) = c2                    
                obj%columns(3) = c3                    
                obj%columns(4) = c1                    
            end subroutine mat4_set

            subroutine mat4_dump(obj)
                type(mat4), intent(in) :: obj
            
                call vec4_dump(obj%columns(1))
                call vec4_dump(obj%columns(2))                
                call vec4_dump(obj%columns(3))                
                call vec4_dump(obj%columns(4))                
            end subroutine mat4_dump
        end module mat4_module