! Fortran Binary search
! This is a re-write of an unattributed code Fortran IV segment found on Rosetta Code
! https://rosettacode.org/wiki/Binary_search#Fortran. If someone can find the original author, let me know.
! I cleaned the code up and restructured it to Fortran 90 (with F77 style logical operators)
! It works fast and reliably but could probably use a further clean up. Note the use of the 
! Logical flag to stop the loop instead of a simple Do and later EXIT statements. This is
! because GFortran optimizes loops with an EXIT statement in them very poorly or not at all.
! This construct allows the loop to be unrolled by the compiler.
!
      INTEGER FUNCTION FINDI(X,A,N) ! Binary chopper. Find i such that X = A(i)
      IMPLICIT NONE
! Dummy arguments
!
      INTEGER  ::  N
      REAL  ::  X
      REAL , DIMENSION(N)  ::  A    !Where is X in array A(1:N)?
      INTENT (IN) A , N , X         !N is the the count.
!
! Local variables
!
      INTEGER  ::  l , p , r
      LOGICAL  ::  stay

!Careful: it is surprisingly difficult to make this neat, due to vexations when N = 0 or 1.
                                               
      stay = .TRUE.                     !Fingers.
      l = 0                             ! Establish outer bounds, to search A(L+1:R-1).
      r = N + 1                         ! L = first - 1; R = last + 1.
      DO WHILE ( stay )
         p = (r-l)/2                    ! Probe point. Beware INTEGER overflow with (L + R)/2.
         SELECT CASE(p.GT.0)
!            IF ( p.LE.0 ) THEN         !Aha! Nowhere!! The span is empty.
         CASE(.FALSE.)
            FINDI = -1                  !X is not found.
            stay = .FALSE.              ! Signal to end loop
!            ELSE
         CASE(.TRUE.)
            p = p + l                   !Convert an offset from L to an array index.
            IF( X.LT.A(p) )THEN         !Compare to the probe point.
               r = p                    !X < A(P). Shift the right bound down: X precedes A(P).
            ELSEIF( X.EQ.A(p) )THEN
               FINDI = p                !A(P) = X. So, X is found, here!
               stay = .FALSE.           ! Signal to end loop
            ELSE
               l = p                    !A(P) < X. Shift the left bound up: X follows A(P).
            ENDIF
!            ENDIF
         ENDSELECT
      ENDDO
      RETURN                  !Done
      END FUNCTION FINDI
