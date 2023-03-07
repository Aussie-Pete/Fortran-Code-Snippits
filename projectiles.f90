
       PROGRAM projectile
       USE ISO_FORTRAN_ENV                        
       IMPLICIT NONE
!
! PARAMETER definitions
!
       REAL(REAL64) , PARAMETER  ::  g = 9.81D0 , &
        &  convert = 3.14159265358979323846264338327950288419716939937510D0/180.0D0 ,&
        &  table_height = 0.75
!
! Local variables
!
       REAL(REAL64)  ::  angle , bangle , dist_from_base , inc , l , maxheight ,          &
                       & old_dist , rdegs , rmax , time_to_land , time_to_maxh , v ,      &
                       & vcomp , vect
       INTEGER  ::  degrees
!
! First trial
       vect = 5.64d0
       WRITE(* , 12)'first vector =' , vect
       dist_from_base = 0.0
       old_dist = 0.0
       DO degrees = 0 , 90 , 1                              ! Loop over degrees, increment by 5 degrees 
           v = 0.0                                          ! Existing velocity
           rdegs = real(degrees , 8)                        ! convert degrees to floating point
           angle = (rdegs*convert)                          ! convert to radians                      
           WRITE(* , 12)'The angle was ' , rdegs            ! Inform the angle used
!
!       Calculate the max height reached
           vcomp = vect*sin(angle)                          ! Vertical component
           maxheight = abs(v**2 - vcomp**2)/(2.0*g)         ! Apply formula
           WRITE(* , 12)'Maxheight = ' , maxheight
!
           time_to_maxh = (v + (vect*sin(angle)))/g         ! Calc the time to max height
           WRITE(* , 12)'Time to max height' , time_to_maxh
!
           time_to_land = sqrt(((table_height + maxheight)*2.0)/g)      ! Calc time to land
           time_to_land = time_to_land + time_to_maxh       ! Add the 2 times (Time to fall from table + max height achieved)
           WRITE(* , 12)'Time of flight =' , time_to_land
! Now horizontal distance
           dist_from_base = (vect*cos(angle)*time_to_land)  ! Multiply horizontal force by time to get distance
           WRITE(* , 12)'Distance travelled =' , dist_from_base
           WRITE(* , *)' '
           IF( dist_from_base>old_dist )THEN                !Save the angle that gave the greatest distance
               old_dist = dist_from_base
               bangle = angle/convert                       ! Convert from radians to degrees
           END IF
       write(99,*)dist_from_base,',',angle/convert 
       END DO
       write(*,'(a,i0,a,f7.3,a)') 'Greatest travel at ' , nint(bangle) , ' degrees, distance was ' , old_dist,' meters'
!
! Now calculate the Theoretical maximum angle
       l = vect/g
       inc = sqrt((vect**2) + (2.0D0*g*table_height))
       rmax = inc*l
       write(*,'(a,f9.3,a,f12.3)') 'Theoretical best is = ' ,  atand(vect/inc) , ' degrees, distance of ' ,rmax
       print*,'alt R = ',((vect**2)/g)*cotan(atan(vect/inc)),((vect**2)/g)

       STOP 'Done'
 12    FORMAT(a , 2x , f7.3)
       END PROGRAM projectile
