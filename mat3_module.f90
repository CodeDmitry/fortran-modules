        module mat3_module
            use vec3_module
            implicit none        
            
            type mat3
                type(vec3), dimension(3) :: columns
            end type mat3
        contains                        
            subroutine mat3_set(obj, c1, c2, c3)
                type(mat3), intent(out) :: obj
                type(vec3), intent(in) :: c1
                type(vec3), intent(in) :: c2
                type(vec3), intent(in) :: c3
                
                obj%columns(1) = c1                    
                obj%columns(2) = c2                    
                obj%columns(3) = c3                    
            end subroutine mat3_set

            subroutine mat3_dump(obj)
                type(mat3), intent(in) :: obj
            
                call vec3_dump(obj%columns(1))
                call vec3_dump(obj%columns(2))                
                call vec3_dump(obj%columns(3))                
            end subroutine mat3_dump
        end module mat3_module