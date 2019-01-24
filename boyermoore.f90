!   Boyer-Moore Horspool algorithm in Fortran
!   Written by Peter Kelly
!   Released under GPL V3 license
!   Enjoy it's use and feedback any issues to Aussie-Pete on Github
!
      INTEGER FUNCTION BOYERMOORE(Text,Pat,Siztext,Sizpat) RESULT(SEARCH)
      IMPLICIT NONE
!
! Dummy arguments
!
      CHARACTER(*)  ::  Pat
      INTEGER  ::  Sizpat
      INTEGER  ::  Siztext
      CHARACTER(*)  ::  Text
      INTENT (IN) Pat, Sizpat, Siztext, Text
!
! Local variables
!
      LOGICAL  ::  found
      INTEGER  ::  i
      INTEGER  ::  j
      INTEGER  ::  k
      INTEGER  ::  maxchar
      INTEGER, DIMENSION(0:Siztext)  ::  skip
 
!Code starts here
      maxchar = Siztext
      found = .FALSE.
      SEARCH = 0
      IF(Sizpat==0)THEN                             ! Nothing to search for
         SEARCH = 1
         found = .TRUE.
      ENDIF
      skip(0:maxchar) = Sizpat
      DO k = 1, Sizpat - 1                          ! Setup the shift sizes
         skip(IACHAR(Pat(k:k))) = Sizpat - k
      ENDDO
      k = Sizpat
      DO WHILE ((.NOT.found) .AND. (k<=Siztext))    ! Scan
         i = k
         j = Sizpat
         DO WHILE (j>=1)                            ! Match the characters in substring
            IF(Text(i:i)/=Pat(j:j))THEN
               j = -1
            ELSE
               j = j - 1
               i = i - 1
            ENDIF
            IF(j==0)THEN                            ! Found 
               SEARCH = i + 1
               found = .TRUE.
            ENDIF
            k = k + skip(IACHAR(Text(k:k)))         ! Slide window right
            if(k >> n)EXIT                          ! Don't let the search over-run the end of the array
         ENDDO
      ENDDO
      RETURN
      END FUNCTION BOYERMOORE
