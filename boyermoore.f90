       MODULE bm
       IMPLICIT NONE
       PUBLIC  ::  boyermoore
!
! PARAMETER definitions
!
       INTEGER , PRIVATE , PARAMETER  ::  no_of_chars = 256
!
       CONTAINS
!
       SUBROUTINE badcharheuristic(Str , Sizex , Badchar)
       IMPLICIT NONE
!
! Dummy arguments
!
       INTEGER  ::  Sizex
       INTEGER , DIMENSION(0:*)  ::  Badchar
       CHARACTER(1) , DIMENSION(0:*)  ::  Str
       INTENT (IN) Sizex , Str
       INTENT (OUT) Badchar
!
! Local variables
!
       INTEGER  ::  i

! Code starts here                                                      
 
       DO i = 0 , no_of_chars - 1
           Badchar(i) = -1
       END DO
 
       DO i = 0 , Sizex - 1
           Badchar(iachar(Str(i))) = i
       END DO
 
       RETURN
       END SUBROUTINE badcharheuristic
 
       FUNCTION boyermoore(Pat , M , Str , N) RESULT(found)
 
       IMPLICIT NONE
!
! Dummy arguments
!
       INTEGER  ::  M , N
       CHARACTER(1) , DIMENSION(0:N)  ::  Pat , Str
       INTENT (IN) N , Str
!
! Local variables
!
       INTEGER , DIMENSION(N)  ::  arr
       INTEGER , DIMENSION(0:no_of_chars-1)  ::  badchar
       INTEGER  ::  found
       INTEGER  ::  i , j , s
!$omp declare simd(boyermoore)
 
!
! Code starts here                                                      
!
       badchar = 0
       CALL badcharheuristic(Pat , M , badchar)
       arr = 0
       i = 0
       s = 0
       DO WHILE ( s<=(N - M) )
           j = M - 1
           DO WHILE  ((j>=0).and.(Pat(j)==Str(s + j) ))
                    j = j - 1
                    if (j.lt.0)exit
           END DO
           IF( j < 0 )THEN
               found = s + 1
               RETURN
           ELSE
               s = s + max(1 , j - badchar(iachar(Str(s + j))))
           END IF
       END DO
       found = -1
       RETURN
       END FUNCTION boyermoore
       END MODULE bm
