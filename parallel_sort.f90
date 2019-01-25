!   Fortran Parallel integer sort posted in GITHUB by cphyc
! https://github.com/cphyc
! The code is not materially changed from his post other than a Fortran
! 'beautify' and the insertion of 'IMPLICIT NONE' statements.
! Performance on small arrays is great but on larger arrays > 2**21 elements it really lags behind Quicksort
! probably due to poor cache optimization
!
!
! From http://www.fortran-2000.com/rank/
! ORDERPACK by: Michel Olagnon
 
      MODULE M_MRGRNK
      USE,INTRINSIC :: ISO_FORTRAN_ENV,ONLY: REAL32,REAL64
      IMPLICIT NONE
!
! PARAMETER definitions
!
      INTEGER, PARAMETER  ::  SP = REAL32
      INTEGER, PARAMETER  ::  KDP = REAL64
!
! Local variables
!
      REAL, PRIVATE  ::  D_MRGRNK
      INTEGER, PRIVATE  ::  i_mrgrnk
      INTEGER, PUBLIC  ::  MRGRNK
      REAL, PRIVATE  ::  r_mrgrnk
 
      INTERFACE MRGRNK
      MODULE PROCEDURED_MRGRNK, r_mrgrnk, i_mrgrnk
      END INTERFACE
      CONTAINS
 
      SUBROUTINE D_MRGRNK(Xdont,Irngt)
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER, DIMENSION(:)  ::  Irngt
      REAL(KDP), DIMENSION(:)  ::  Xdont
      INTENT (IN) Xdont
      INTENT (OUT) Irngt
!
! Local variables
!
      INTEGER  ::  iind
      INTEGER  ::  iinda
      INTEGER  ::  iindb
      INTEGER  ::  irng1
      INTEGER  ::  irng2
      INTEGER  ::  iwrk
      INTEGER  ::  iwrkd
      INTEGER  ::  iwrkf
      INTEGER  ::  jinda
      INTEGER, DIMENSION(SIZE(Irngt))  ::  jwrkt
      INTEGER  ::  lmtna
      INTEGER  ::  lmtnc
      INTEGER  ::  nval
      REAL(KDP)  ::  xvala
      REAL(KDP)  ::  xvalb
    ! __________________________________________________________
    !   MRGRNK = Merge-sort ranking of an array
    !   For performance reasons, the first 2 passes are taken
    !   out of the standard loop, and use dedicated coding.
    ! __________________________________________________________
    ! __________________________________________________________
    ! __________________________________________________________
    !
!Code starts here                                                       
    !
      nval = MIN(SIZE(Xdont),SIZE(Irngt))
      SELECT CASE(nval)
      CASE(:0)
         RETURN
      CASE(1)
         Irngt(1) = 1
         RETURN
      CASE DEFAULT
      ENDSELECT
    !
    !  Fill-in the index array, creating ordered couples
    !
      DO iind = 2, nval, 2
         IF(Xdont(iind-1).LE.Xdont(iind))THEN
            Irngt(iind-1) = iind - 1
            Irngt(iind) = iind
         ELSE
            Irngt(iind-1) = iind
            Irngt(iind) = iind - 1
         ENDIF
      ENDDO
      IF(MODULO(nval,2).NE.0)Irngt(nval) = nval
    !
    !  We will now have ordered subsets A - B - A - B - ...
    !  and merge A and B couples into     C   -   C   - ...
    !
      lmtna = 2
      lmtnc = 4
    !
    !  First iteration. The length of the ordered subsets goes from 2 to 4
    !
      DO
         IF(nval.LE.2)EXIT
       !
       !   Loop on merges of A and B into C
       !
         DO iwrkd = 0, nval - 1, 4
            IF((iwrkd+4).GT.nval)THEN
               IF((iwrkd+2).GE.nval)EXIT
             !
             !   1 2 3
             !
               IF(Xdont(Irngt(iwrkd+2)).LE.Xdont(Irngt(iwrkd+3)))EXIT
             !
             !   1 3 2
             !
               IF(Xdont(Irngt(iwrkd+1)).LE.Xdont(Irngt(iwrkd+3)))THEN
                  irng2 = Irngt(iwrkd+2)
                  Irngt(iwrkd+2) = Irngt(iwrkd+3)
                  Irngt(iwrkd+3) = irng2
                !
                !   3 1 2
                !
               ELSE
                  irng1 = Irngt(iwrkd+1)
                  Irngt(iwrkd+1) = Irngt(iwrkd+3)
                  Irngt(iwrkd+3) = Irngt(iwrkd+2)
                  Irngt(iwrkd+2) = irng1
               ENDIF
               EXIT
            ENDIF
          !
          !   1 2 3 4
          !
            IF(Xdont(Irngt(iwrkd+2)).LE.Xdont(Irngt(iwrkd+3)))CYCLE
          !
          !   1 3 x x
          !
            IF(Xdont(Irngt(iwrkd+1)).LE.Xdont(Irngt(iwrkd+3)))THEN
               irng2 = Irngt(iwrkd+2)
               Irngt(iwrkd+2) = Irngt(iwrkd+3)
               IF(Xdont(irng2).LE.Xdont(Irngt(iwrkd+4)))THEN
                !   1 3 2 4
                  Irngt(iwrkd+3) = irng2
               ELSE
                !   1 3 4 2
                  Irngt(iwrkd+3) = Irngt(iwrkd+4)
                  Irngt(iwrkd+4) = irng2
               ENDIF
             !
             !   3 x x x
             !
            ELSE
               irng1 = Irngt(iwrkd+1)
               irng2 = Irngt(iwrkd+2)
               Irngt(iwrkd+1) = Irngt(iwrkd+3)
               IF(Xdont(irng1).LE.Xdont(Irngt(iwrkd+4)))THEN
                  Irngt(iwrkd+2) = irng1
                  IF(Xdont(irng2).LE.Xdont(Irngt(iwrkd+4)))THEN
                   !   3 1 2 4
                     Irngt(iwrkd+3) = irng2
                  ELSE
                   !   3 1 4 2
                     Irngt(iwrkd+3) = Irngt(iwrkd+4)
                     Irngt(iwrkd+4) = irng2
                  ENDIF
               ELSE
                !   3 4 1 2
                  Irngt(iwrkd+2) = Irngt(iwrkd+4)
                  Irngt(iwrkd+3) = irng1
                  Irngt(iwrkd+4) = irng2
               ENDIF
            ENDIF
         ENDDO
       !
       !  The Cs become As and Bs
       !
         lmtna = 4
         EXIT
      ENDDO
    !
    !  Iteration loop. Each time, the length of the ordered subsets
    !  is doubled.
    !
      DO
         IF(lmtna.GE.nval)EXIT
         iwrkf = 0
         lmtnc = 2*lmtnc
       !
       !   Loop on merges of A and B into C
       !
         DO
            iwrk = iwrkf
            iwrkd = iwrkf + 1
            jinda = iwrkf + lmtna
            iwrkf = iwrkf + lmtnc
            IF(iwrkf.GE.nval)THEN
               IF(jinda.GE.nval)EXIT
               iwrkf = nval
            ENDIF
            iinda = 1
            iindb = jinda + 1
          !
          !   Shortcut for the case when the max of A is smaller
          !   than the min of B. This line may be activated when the
          !   initial set is already close to sorted.
          !
          !          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
          !
          !  One steps in the C subset, that we build in the final rank array
          !
          !  Make a copy of the rank array for the merge iteration
          !
            jwrkt(1:lmtna) = Irngt(iwrkd:jinda)
          !
            xvala = Xdont(jwrkt(iinda))
            xvalb = Xdont(Irngt(iindb))
          !
            DO
               iwrk = iwrk + 1
             !
             !  We still have unprocessed values in both A and B
             !
               IF(xvala.GT.xvalb)THEN
                  Irngt(iwrk) = Irngt(iindb)
                  iindb = iindb + 1
                  IF(iindb.GT.iwrkf)THEN
                   !  Only A still with unprocessed values
                     Irngt(iwrk+1:iwrkf) = jwrkt(iinda:lmtna)
                     EXIT
                  ENDIF
                  xvalb = Xdont(Irngt(iindb))
               ELSE
                  Irngt(iwrk) = jwrkt(iinda)
                  iinda = iinda + 1
                  IF(iinda.GT.lmtna)EXIT
                                       ! Only B still with unprocessed values
                  xvala = Xdont(jwrkt(iinda))
               ENDIF
             !
            ENDDO
         ENDDO
       !
       !  The Cs become As and Bs
       !
         lmtna = 2*lmtna
      ENDDO
    !
      RETURN
    !
      END SUBROUTINE D_MRGRNK
 
      SUBROUTINE R_MRGRNK(Xdont,Irngt)
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      INTEGER, DIMENSION(:)  ::  Irngt
      REAL(SP), DIMENSION(:)  ::  Xdont
      INTENT (IN) Xdont
      INTENT (OUT) Irngt
!
! Local variables
!
      INTEGER  ::  iind
      INTEGER  ::  iinda
      INTEGER  ::  iindb
      INTEGER  ::  irng1
      INTEGER  ::  irng2
      INTEGER  ::  iwrk
      INTEGER  ::  iwrkd
      INTEGER  ::  iwrkf
      INTEGER  ::  jinda
      INTEGER, DIMENSION(SIZE(Irngt))  ::  jwrkt
      INTEGER  ::  lmtna
      INTEGER  ::  lmtnc
      INTEGER  ::  nval
      REAL(SP)  ::  xvala
      REAL(SP)  ::  xvalb
!
!*** End of declarations rewritten by SPAG
!
    ! __________________________________________________________
    !   MRGRNK = Merge-sort ranking of an array
    !   For performance reasons, the first 2 passes are taken
    !   out of the standard loop, and use dedicated coding.
    ! __________________________________________________________
    ! _________________________________________________________
    ! __________________________________________________________
    !
!Code starts here                                                       
    !
      nval = MIN(SIZE(Xdont),SIZE(Irngt))
      SELECT CASE(nval)
      CASE(:0)
         RETURN
      CASE(1)
         Irngt(1) = 1
         RETURN
      CASE DEFAULT
      ENDSELECT
    !
    !  Fill-in the index array, creating ordered couples
    !
      DO iind = 2, nval, 2
         IF(Xdont(iind-1).LE.Xdont(iind))THEN
            Irngt(iind-1) = iind - 1
            Irngt(iind) = iind
         ELSE
            Irngt(iind-1) = iind
            Irngt(iind) = iind - 1
         ENDIF
      ENDDO
      IF(MODULO(nval,2).NE.0)Irngt(nval) = nval
    !
    !  We will now have ordered subsets A - B - A - B - ...
    !  and merge A and B couples into     C   -   C   - ...
    !
      lmtna = 2
      lmtnc = 4
    !
    !  First iteration. The length of the ordered subsets goes from 2 to 4
    !
      DO
         IF(nval.LE.2)EXIT
       !
       !   Loop on merges of A and B into C
       !
         DO iwrkd = 0, nval - 1, 4
            IF((iwrkd+4).GT.nval)THEN
               IF((iwrkd+2).GE.nval)EXIT
             !
             !   1 2 3
             !
               IF(Xdont(Irngt(iwrkd+2)).LE.Xdont(Irngt(iwrkd+3)))EXIT
             !
             !   1 3 2
             !
               IF(Xdont(Irngt(iwrkd+1)).LE.Xdont(Irngt(iwrkd+3)))THEN
                  irng2 = Irngt(iwrkd+2)
                  Irngt(iwrkd+2) = Irngt(iwrkd+3)
                  Irngt(iwrkd+3) = irng2
                !
                !   3 1 2
                !
               ELSE
                  irng1 = Irngt(iwrkd+1)
                  Irngt(iwrkd+1) = Irngt(iwrkd+3)
                  Irngt(iwrkd+3) = Irngt(iwrkd+2)
                  Irngt(iwrkd+2) = irng1
               ENDIF
               EXIT
            ENDIF
          !
          !   1 2 3 4
          !
            IF(Xdont(Irngt(iwrkd+2)).LE.Xdont(Irngt(iwrkd+3)))CYCLE
          !
          !   1 3 x x
          !
            IF(Xdont(Irngt(iwrkd+1)).LE.Xdont(Irngt(iwrkd+3)))THEN
               irng2 = Irngt(iwrkd+2)
               Irngt(iwrkd+2) = Irngt(iwrkd+3)
               IF(Xdont(irng2).LE.Xdont(Irngt(iwrkd+4)))THEN
                !   1 3 2 4
                  Irngt(iwrkd+3) = irng2
               ELSE
                !   1 3 4 2
                  Irngt(iwrkd+3) = Irngt(iwrkd+4)
                  Irngt(iwrkd+4) = irng2
               ENDIF
             !
             !   3 x x x
             !
            ELSE
               irng1 = Irngt(iwrkd+1)
               irng2 = Irngt(iwrkd+2)
               Irngt(iwrkd+1) = Irngt(iwrkd+3)
               IF(Xdont(irng1).LE.Xdont(Irngt(iwrkd+4)))THEN
                  Irngt(iwrkd+2) = irng1
                  IF(Xdont(irng2).LE.Xdont(Irngt(iwrkd+4)))THEN
                   !   3 1 2 4
                     Irngt(iwrkd+3) = irng2
                  ELSE
                   !   3 1 4 2
                     Irngt(iwrkd+3) = Irngt(iwrkd+4)
                     Irngt(iwrkd+4) = irng2
                  ENDIF
               ELSE
                !   3 4 1 2
                  Irngt(iwrkd+2) = Irngt(iwrkd+4)
                  Irngt(iwrkd+3) = irng1
                  Irngt(iwrkd+4) = irng2
               ENDIF
            ENDIF
         ENDDO
       !
       !  The Cs become As and Bs
       !
         lmtna = 4
         EXIT
      ENDDO
    !
    !  Iteration loop. Each time, the length of the ordered subsets
    !  is doubled.
    !
      DO
         IF(lmtna.GE.nval)EXIT
         iwrkf = 0
         lmtnc = 2*lmtnc
       !
       !   Loop on merges of A and B into C
       !
         DO
            iwrk = iwrkf
            iwrkd = iwrkf + 1
            jinda = iwrkf + lmtna
            iwrkf = iwrkf + lmtnc
            IF(iwrkf.GE.nval)THEN
               IF(jinda.GE.nval)EXIT
               iwrkf = nval
            ENDIF
            iinda = 1
            iindb = jinda + 1
          !
          !   Shortcut for the case when the max of A is smaller
          !   than the min of B. This line may be activated when the
          !   initial set is already close to sorted.
          !
          !          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
          !
          !  One steps in the C subset, that we build in the final rank array
          !
          !  Make a copy of the rank array for the merge iteration
          !
            jwrkt(1:lmtna) = Irngt(iwrkd:jinda)
          !
            xvala = Xdont(jwrkt(iinda))
            xvalb = Xdont(Irngt(iindb))
          !
            DO
               iwrk = iwrk + 1
             !
             !  We still have unprocessed values in both A and B
             !
               IF(xvala.GT.xvalb)THEN
                  Irngt(iwrk) = Irngt(iindb)
                  iindb = iindb + 1
                  IF(iindb.GT.iwrkf)THEN
                   !  Only A still with unprocessed values
                     Irngt(iwrk+1:iwrkf) = jwrkt(iinda:lmtna)
                     EXIT
                  ENDIF
                  xvalb = Xdont(Irngt(iindb))
               ELSE
                  Irngt(iwrk) = jwrkt(iinda)
                  iinda = iinda + 1
                  IF(iinda.GT.lmtna)EXIT
                                       ! Only B still with unprocessed values
                  xvala = Xdont(jwrkt(iinda))
               ENDIF
             !
            ENDDO
         ENDDO
       !
       !  The Cs become As and Bs
       !
         lmtna = 2*lmtna
      ENDDO
    !
      RETURN
    !
      END SUBROUTINE R_MRGRNK
      SUBROUTINE I_MRGRNK(Xdont,Irngt)
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER, DIMENSION(:)  ::  Irngt
      INTEGER, DIMENSION(:)  ::  Xdont
      INTENT (IN) Xdont
      INTENT (OUT) Irngt
!
! Local variables
!
      INTEGER  ::  iind
      INTEGER  ::  iinda
      INTEGER  ::  iindb
      INTEGER  ::  irng1
      INTEGER  ::  irng2
      INTEGER  ::  iwrk
      INTEGER  ::  iwrkd
      INTEGER  ::  iwrkf
      INTEGER  ::  jinda
      INTEGER, DIMENSION(SIZE(Irngt))  ::  jwrkt
      INTEGER  ::  lmtna
      INTEGER  ::  lmtnc
      INTEGER  ::  nval
      INTEGER  ::  xvala
      INTEGER  ::  xvalb
    ! __________________________________________________________
    !   MRGRNK = Merge-sort ranking of an array
    !   For performance reasons, the first 2 passes are taken
    !   out of the standard loop, and use dedicated coding.
    ! __________________________________________________________
    ! __________________________________________________________
    ! __________________________________________________________
    !
!Code starts here                                                       
    !
      nval = MIN(SIZE(Xdont),SIZE(Irngt))
      SELECT CASE(nval)
      CASE(:0)
         RETURN
      CASE(1)
         Irngt(1) = 1
         RETURN
      CASE DEFAULT
      ENDSELECT
    !
    !  Fill-in the index array, creating ordered couples
    !
      DO iind = 2, nval, 2
         IF(Xdont(iind-1).LE.Xdont(iind))THEN
            Irngt(iind-1) = iind - 1
            Irngt(iind) = iind
         ELSE
            Irngt(iind-1) = iind
            Irngt(iind) = iind - 1
         ENDIF
      ENDDO
      IF(MODULO(nval,2).NE.0)Irngt(nval) = nval
    !
    !  We will now have ordered subsets A - B - A - B - ...
    !  and merge A and B couples into     C   -   C   - ...
    !
      lmtna = 2
      lmtnc = 4
    !
    !  First iteration. The length of the ordered subsets goes from 2 to 4
    !
      DO
         IF(nval.LE.2)EXIT
       !
       !   Loop on merges of A and B into C
       !
         DO iwrkd = 0, nval - 1, 4
            IF((iwrkd+4).GT.nval)THEN
               IF((iwrkd+2).GE.nval)EXIT
             !
             !   1 2 3
             !
               IF(Xdont(Irngt(iwrkd+2)).LE.Xdont(Irngt(iwrkd+3)))EXIT
             !
             !   1 3 2
             !
               IF(Xdont(Irngt(iwrkd+1)).LE.Xdont(Irngt(iwrkd+3)))THEN
                  irng2 = Irngt(iwrkd+2)
                  Irngt(iwrkd+2) = Irngt(iwrkd+3)
                  Irngt(iwrkd+3) = irng2
                !
                !   3 1 2
                !
               ELSE
                  irng1 = Irngt(iwrkd+1)
                  Irngt(iwrkd+1) = Irngt(iwrkd+3)
                  Irngt(iwrkd+3) = Irngt(iwrkd+2)
                  Irngt(iwrkd+2) = irng1
               ENDIF
               EXIT
            ENDIF
          !
          !   1 2 3 4
          !
            IF(Xdont(Irngt(iwrkd+2)).LE.Xdont(Irngt(iwrkd+3)))CYCLE
          !
          !   1 3 x x
          !
            IF(Xdont(Irngt(iwrkd+1)).LE.Xdont(Irngt(iwrkd+3)))THEN
               irng2 = Irngt(iwrkd+2)
               Irngt(iwrkd+2) = Irngt(iwrkd+3)
               IF(Xdont(irng2).LE.Xdont(Irngt(iwrkd+4)))THEN
                !   1 3 2 4
                  Irngt(iwrkd+3) = irng2
               ELSE
                !   1 3 4 2
                  Irngt(iwrkd+3) = Irngt(iwrkd+4)
                  Irngt(iwrkd+4) = irng2
               ENDIF
             !
             !   3 x x x
             !
            ELSE
               irng1 = Irngt(iwrkd+1)
               irng2 = Irngt(iwrkd+2)
               Irngt(iwrkd+1) = Irngt(iwrkd+3)
               IF(Xdont(irng1).LE.Xdont(Irngt(iwrkd+4)))THEN
                  Irngt(iwrkd+2) = irng1
                  IF(Xdont(irng2).LE.Xdont(Irngt(iwrkd+4)))THEN
                   !   3 1 2 4
                     Irngt(iwrkd+3) = irng2
                  ELSE
                   !   3 1 4 2
                     Irngt(iwrkd+3) = Irngt(iwrkd+4)
                     Irngt(iwrkd+4) = irng2
                  ENDIF
               ELSE
                !   3 4 1 2
                  Irngt(iwrkd+2) = Irngt(iwrkd+4)
                  Irngt(iwrkd+3) = irng1
                  Irngt(iwrkd+4) = irng2
               ENDIF
            ENDIF
         ENDDO
       !
       !  The Cs become As and Bs
       !
         lmtna = 4
         EXIT
      ENDDO
    !
    !  Iteration loop. Each time, the length of the ordered subsets
    !  is doubled.
    !
      DO
         IF(lmtna.GE.nval)EXIT
         iwrkf = 0
         lmtnc = 2*lmtnc
       !
       !   Loop on merges of A and B into C
       !
         DO
            iwrk = iwrkf
            iwrkd = iwrkf + 1
            jinda = iwrkf + lmtna
            iwrkf = iwrkf + lmtnc
            IF(iwrkf.GE.nval)THEN
               IF(jinda.GE.nval)EXIT
               iwrkf = nval
            ENDIF
            iinda = 1
            iindb = jinda + 1
          !
          !   Shortcut for the case when the max of A is smaller
          !   than the min of B. This line may be activated when the
          !   initial set is already close to sorted.
          !
          !          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
          !
          !  One steps in the C subset, that we build in the final rank array
          !
          !  Make a copy of the rank array for the merge iteration
          !
            jwrkt(1:lmtna) = Irngt(iwrkd:jinda)
          !
            xvala = Xdont(jwrkt(iinda))
            xvalb = Xdont(Irngt(iindb))
          !
            DO
               iwrk = iwrk + 1
             !
             !  We still have unprocessed values in both A and B
             !
               IF(xvala.GT.xvalb)THEN
                  Irngt(iwrk) = Irngt(iindb)
                  iindb = iindb + 1
                  IF(iindb.GT.iwrkf)THEN
                   !  Only A still with unprocessed values
                     Irngt(iwrk+1:iwrkf) = jwrkt(iinda:lmtna)
                     EXIT
                  ENDIF
                  xvalb = Xdont(Irngt(iindb))
               ELSE
                  Irngt(iwrk) = jwrkt(iinda)
                  iinda = iinda + 1
                  IF(iinda.GT.lmtna)EXIT
                                       ! Only B still with unprocessed values
                  xvala = Xdont(jwrkt(iinda))
               ENDIF
             !
            ENDDO
         ENDDO
       !
       !  The Cs become As and Bs
       !
         lmtna = 2*lmtna
      ENDDO
    !
      RETURN
    !
      END SUBROUTINE I_MRGRNK
      END MODULE M_MRGRNK
!
      MODULE MOD_SORT
 
      USE OMP_LIB
      USE M_MRGRNK
      IMPLICIT NONE

      PUBLIC  ::  parallel_sort
!      PRIVATE  ::  M_MRGRNK, OMP_LIB
!
! PARAMETER definitions
!
      INTEGER, PRIVATE, PARAMETER  ::  MAX_SIMPLE_SORT_SIZE = 20

      CONTAINS
 
      SUBROUTINE PARALLEL_SORT(A,Order)
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER, DIMENSION(:)  ::  A
      INTEGER, DIMENSION(SIZE(A))  ::  Order
      INTENT (IN) A
      INTENT (OUT) Order
!
! Local variables
!
      INTEGER  ::  chunk
      INTEGER  ::  chunk2
      INTEGER  ::  from
      INTEGER  ::  i
      INTEGER  ::  len
      INTEGER  ::  middle
      INTEGER  ::  nthreads
      INTEGER  ::  thread
      INTEGER  ::  to

!Code starts here                                                       
 
      len = SIZE(A)
      nthreads = OMP_GET_MAX_THREADS()
      chunk = len/nthreads
 
    !----------------------------------------
    ! Initialize order
    !----------------------------------------
      DO i = 1, len
         Order(i) = i
      ENDDO
 
    !----------------------------------------
    ! Sort each chunk
    !----------------------------------------
    !$OMP parallel do default(none)          &
    !$OMP firstprivate(chunk, len, nthreads) &
    !$OMP shared(A, order) private(from, to)
      DO thread = 0, nthreads
         from = thread*chunk + 1
         to = MIN((thread+1)*chunk,len)
 
         CALL MRGRNK(A(from:to),Order(from:to))
         Order(from:to) = Order(from:to) + from - 1
 
      ENDDO
    !$OMP end parallel do
 
    !----------------------------------------
    ! Merge pieces together
    !----------------------------------------
      i = 1
      chunk2 = chunk
      DO WHILE (chunk2.LT.SIZE(A))
 
       !$OMP parallel do default(none)  &
       !$OMP shared(chunk2, A, order)   &
       !$OMP private(from, middle, to)
         DO thread = 0, CEILING(.5*SIZE(A)/chunk2)
            from = thread*2*chunk2 + 1
            middle = (thread*2+1)*chunk2
            to = (thread*2+2)*chunk2
 
            middle = MIN(middle,SIZE(A))
            to = MIN(to,SIZE(A))
            IF(from.LT.to)CALL MERGE(A,Order,from,middle,to)
         ENDDO
       !$OMP end parallel do
         chunk2 = chunk2*2
         i = i + 1
      ENDDO
      END SUBROUTINE PARALLEL_SORT
 
  !> Merge two parts of A, ordered by order from left to right
  !! around middle.
      SUBROUTINE MERGE(A,Order,Left,Middle,Right)
      IMPLICIT NONE

!
! Dummy arguments
!
      INTEGER  ::  Left
      INTEGER  ::  Middle
      INTEGER  ::  Right
      INTEGER, DIMENSION(:)  ::  A
      INTEGER, DIMENSION(SIZE(A))  ::  Order
      INTENT (IN) A, Left, Middle, Right
      INTENT (INOUT) Order
!
! Local variables
!
      INTEGER  ::  i
      INTEGER  ::  ia
      INTEGER  ::  ib
      INTEGER  ::  lefta
      INTEGER  ::  leftb
      INTEGER, DIMENSION(Left:Middle)  ::  ordera
      INTEGER, DIMENSION(Middle+1:Right)  ::  orderb
      INTEGER  ::  righta
      INTEGER  ::  rightb
 
!Code starts here                                                       
 
    ! copy order
      ordera = Order(Left:Middle)
      orderb = Order(Middle+1:Right)
 
    ! more explicit variables
      lefta = Left
      righta = Middle
      leftb = Middle + 1
      rightb = Right
 
    ! initialize iA, iB to their leftmost position
      ia = lefta
      ib = leftb
 
      i = lefta
 
      DO WHILE ((ia.LE.righta).AND.(ib.LE.rightb))
         IF(A(ordera(ia)).LE.A(orderb(ib)))THEN
            Order(i) = ordera(ia)
            ia = ia + 1
         ELSE
            Order(i) = orderb(ib)
            ib = ib + 1
         ENDIF
 
         i = i + 1
      ENDDO
 
    ! either A or B still have elements, append them to the new order
      DO WHILE (ia.LE.righta)
         Order(i) = ordera(ia)
         ia = ia + 1
 
         i = i + 1
 
      ENDDO
      DO WHILE (ib.LE.rightb)
         Order(i) = orderb(ib)
         ib = ib + 1
 
         i = i + 1
      ENDDO
 
      END SUBROUTINE MERGE
 
      END MODULE MOD_SORT
