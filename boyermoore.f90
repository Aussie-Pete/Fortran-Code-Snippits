       module bm_fort
       implicit none
       private
       public  ::  bmfort
!
! PARAMETER definitions
!
       integer , private , parameter  ::  NO_OF_CHARS = 256, SIZEX=256
!
       contains
!
       pure subroutine badcharheuristic(Str,M,Badchar)
       implicit none
! Dummy arguments
!
       integer  ::  M
       integer , dimension(0:NO_OF_CHARS-1)  ::  Badchar
       character(1) , dimension(0:M-1)  ::  Str
       intent (in) M , Str
       intent (out) Badchar
!
! Local variables
!
       integer  ::  i
! Code starts here
        Badchar(0:NO_OF_CHARS - 1) = -1
 
       do i = 0 , M - 1
           Badchar(iachar(Str(i))) = i
       enddo
       return
       end subroutine badcharheuristic
 
        function bmfort(Pat,M,Str,N) result(found)
       implicit none
!
! Dummy arguments
!
       integer  ::  M
       integer  ::  N
       character(len=1),DIMENSION(0:M-1)   ::  Pat
       character(len=1),DIMENSION(0:N-1)   ::  Str
       intent (in) M , N , Pat , Str
!
! Local variables
!
       integer , dimension(0:NO_OF_CHARS-1)  ::  badchar
       integer  ::  found
       integer  ::  i
       integer  ::  j
       integer  ::  s
!
!$omp declare simd(bmfort)
!
! Code starts here
!
       found = -1
       if ( (M==0) .OR. (N==0) .OR. (M>N) ) return
       badchar = 0
       call badcharheuristic(Pat,M,badchar)
       i = 0
       s = 0
       do while ( s<=(N-M) )
           j = M-1
           do while ( (j >= 0) .AND. (Pat(j) == Str(s+j)) )
               j = j - 1
               if ( j < 1 ) then
                   found = s + 1
                   return
               endif
           enddo
           i = badchar(iachar(Str(s+j)))
           s = s + MAX(1,j-i)
       enddo
       found = -1
       return
       end function bmfort
       end module bm_fort
