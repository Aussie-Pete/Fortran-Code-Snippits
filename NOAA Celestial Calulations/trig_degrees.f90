      module trig_degs
      implicit none!
! PARAMETER definitions
!
      integer,private,parameter :: r8kind=selected_real_kind(15,307)

      real(r8kind), parameter,private  ::  pi = acos( - 1.D0)
      real(r8kind), parameter,private  ::  a2rad = pi/180.0D0
      real(r8kind), parameter,private  ::  rad2a = 180.0D0/pi
!
      contains
! for compilers that don't have the circular functions in degrees.
 
      pure function cosd(angle)
      implicit none
!
! Dummy arguments
!
      real(r8kind)  ::  angle
      real(r8kind)  ::  cosd
      intent (in) angle
!                                                                 
      cosd = cos(angle*a2rad)
      return
      end function cosd
 
      pure function sind(angle)
      implicit none
!
! Dummy arguments
!
      real(r8kind)  ::  angle
      real(r8kind)  ::  sind
      intent (in) angle
!*Code                                                                  
      sind = sin(angle*a2rad)
      return
      end function sind
 
      pure function tand(angle)
      implicit none
!
! Dummy arguments
!
      real(r8kind)  ::  angle
      real(r8kind)  ::  tand
      intent (in) angle

!*Code                                                                  
      tand = tan(angle*a2rad)
      return
      end function tand
 
      pure function acosd(rads)
      implicit none

! Dummy arguments
!
      real(r8kind)  ::  rads
      real(r8kind)  ::  acosd
      intent (in) rads
!*Code                                                                  
      acosd = acos(rads)*rad2a
      return
      end function acosd
 
      pure function asind(rads)
      implicit none
!
! Dummy arguments
!
      real(r8kind)  ::  rads
      real(r8kind)  ::  asind
      intent (in) rads
!
!*Code                                                                  
      asind = asin(rads)*rad2a
      return
      end function asind
 
      pure function atan2d(denom, numer)
      implicit none
!
! Dummy arguments
!
      real(r8kind)  ::  denom
      real(r8kind)  ::  numer
      real(r8kind)  ::  atan2d
      intent (in) denom, numer
!
!*Code                                                                  
      atan2d = atan2(denom, numer)*rad2a
      return
      end function atan2d
      
      pure function atand(rads) result(degs)
      implicit none
!
! Dummy arguments
!
      real(r8kind)  ::  rads
      real(r8kind)  ::  degs
      intent (in) rads
!
!*Code                                                                  
      degs = atan(rads)*rad2a
      return
      end function atand
      end module trig_degs
