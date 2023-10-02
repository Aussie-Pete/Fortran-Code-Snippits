      program driver
      use functions
      use solarazi
      use solarelev
      use aplot
      implicit none
!
! PARAMETER definitions
!
      integer, parameter  ::  r64 = selected_real_kind(15, 307)
      integer, parameter  ::  nday = 365
!
      INTEGER , PARAMETER  ::  SWITCH_YEAR = 1752 , GREGORIAN_EPOCH = 1 , JANUARY = 1 , FEBRUARY = JANUARY + 1 ,             &
                               MARCH = JANUARY + 2 , APRIL = JANUARY + 3 , MAY = JANUARY + 4 , JUNE = JANUARY + 5 ,          &
                               JULY = JANUARY + 6 , AUGUST = JANUARY + 7 , SEPTEMBER = JANUARY + 8 , OCTOBER = JANUARY + 9 , &
                               NOVEMBER = JANUARY + 10 , DECEMBER = JANUARY + 11 , MONDAY = 1 , TUESDAY = 2 , WEDNESDAY = 3 ,&
                               THURSDAY = 4 , FRIDAY = 5 , SATURDAY = 6 , SUNDAY = 0!
! Local variables
!
      real(r64), dimension(0:365)  ::  altitude
      real(r64), dimension(0:365)  ::  azimuth
      real(r64)  ::  day
      real(r64)  ::  dlstime
      real  ::  ends
      real(r64)  ::  hours
      integer  ::  ihrs, degrees
      integer  ::  imins
      integer  ::  isecs
      integer  ::  j
      real(r64)  ::  lat
      real(r64), dimension(0:365)  ::  line
      real(r64), dimension(0:365)  ::  line2
      real(r64)  ::  lon
      character(100)  ::  metime
      real(r64)  ::  minutes
      real(r64)  ::  month
      real(r64)  ::  oldday
      type(aplot_t)  ::  plot
      real(r64)  ::  retval
      real(r64)  ::  seconds
      real  ::  start
      character(100)  ::  string
      real(r64)  ::  t1
      real(r64)  ::  t2
      real(r64)  ::  timezone
      character(100)  ::  title
      integer, dimension(8)  ::  values
      real(r64)  ::  xer
      real(r64)  ::  year
      real(r64)  ::  yer
      character(10)  ::  zone
      enum,bind(c)
      enumerator :: cities=0,Sydney,Bethania,Bethnow,Jakarta
      endenum
      integer(kind(cities)) :: MyCity!
!*Code                                                                  
 
 100  continue
      write(*, '(A)', advance = 'NO') &
                &'Enter 1 for Sydney, 2 for Bethania, 3 for Bethania right now, 4 for Jakarta: '
      read(*, *)mycity
      call cpu_time(start)
      year = 2023.0D0
      month = 10.0D0
      day = 02.0D0
      oldday = day
      hours = 11.0D0
      minutes = 40.0D0
      seconds = 12.0D0
      timezone = 10.0D0
      dlstime = 0.0
      call date_and_time(zone = zone, values = values)
      title = "Annalema at Bethania"
      select case(mycity)
      case(sydney)
          dlstime = 0.0d0
          lon = 151.11361111
          lat = -33.78055556
          hours = 12.0D0
          minutes = 48.0D0
          seconds = 10.0D0
          timezone = 10.0D0
          year = 2023
!Automatically adjust daylight savings for Sydney
          if(month==10 .or. month == 4)then ! Calculate the first Sunday in Oct or First Sunday in April
            if (month==10)then
                j = 1
                do while (get_the_day(j,10,int(year)).ne.0)
                    j = j+1
                end do
                if(j <= nint(day))dlstime = 1.0d0
            else  ! Must be April
                j = 1
                do while (get_the_day(j,4,int(year)).ne.0)
                    j = j+1
                end do
                if(j < nint(day))dlstime = 1.0d0 
            endif
          endif
          title = "Annalema at Sydney"
          if(dlstime > 0.5)write(*,'(a,/)')'Daylight savings has been selected'
      case(bethania)
          lon = 153.15416666666666D0
          lat = -27.690277777777776D0
      case(bethnow)
          lon = 153.15416666666666D0
          lat = -27.690277777777776D0
          year = values(1)
          month = values(2)
          day = (values(3))
          hours = values(5)
          minutes = values(6)
          seconds = values(7)
          read(zone, *)timezone
          timezone = timezone/100.0D0
      case(jakarta)
          title = "Annalema at Jakarta"
          timezone = 7.0d0
          hours = 16.0D0
          minutes = 13.0D0
          seconds = 2.0D0
          lon = 106.82658966D0
          lat = -6.22494014D0 !-6.22494014,106.82658966
      case default
          go to 100
 
      end select
      write(metime, '(3(a,i2.2))')'Local maximum at ', int(hours), ':', &
                              & int(minutes), ':', int(seconds)
      write(*,'(a)') metime
      retval = solarazimuth(lat, lon, int(year), int(month), int(day), &
          int(hours), int(minutes), seconds,int(timezone), &
          int(dlstime))
      print 99001, 'Local Azimuth = ', retval
      retval = solarelevation(lat, lon, year, month, day, hours, minutes, &
             & seconds, timezone, dlstime)
      print 99001, 'Returned elevation = ', retval
 
!NOTE: Seven functions are available for use from Excel worksheets:
!   - sunrise(lat, lon, year, month, day, timezone, dlstime)
!   - solarnoon(lat, lon, year, month, day, timezone, dlstime)
!   - sunset(lat, lon, year, month, day, timezone, dlstime)
!   - dusk(lat, lon, year, month, day, timezone, dlstime, solardepression)
!   - solarazimuth(lat, lon, year, month, day, hour, minute, second, timezone, dlstime)
!   - solarelevation(lat, lon, year, month, day, hour, minute, second, timezone, dlstime)
!where
!   lat = latidude in degrees
!   lon = longitude in degrees
!   timezone = time zone in hours relative to GMT/UTC
!   dlstime = daylight savings time hours
!                 (0 for no daylight savings, or 1 for yes daylight savings)
!   solardepression = angle of the sun below the horizon in degrees
!                 for calculation of time of dawn and dusk
 
!The sign convention for inputs to the functions named sunrise, solarnoon, sunset, dawn, dusk, solarazimuth, and solarelevation is:
 
!   - positive latitude decimal degrees for northern hemisphere
!   - negative longitude degrees for western hemisphere
!   - negative time zone hours for western hemisphere!
!   - dawn(lat, lon, year, month, day, timezone, dlstime, solardepression)
!
      retval = dawn(lat, lon, year, month, day, timezone, dlstime, 18.0D0)
      call dectohrs(retval, ihrs, imins, isecs)
      print 99002, 'Astronomical dawn (sun is 18 degrees below horizon)', ihrs, &
           &':', imins, ':', isecs
      retval = dawn(lat, lon, year, month, day, timezone, dlstime, 12.0D0)
      call dectohrs(retval, ihrs, imins, isecs)
      print 99002, 'Nautical dawn (sun is 12 degrees below horizon)', ihrs, ':',&
          & imins, ':', isecs
      retval = dawn(lat, lon, year, month, day, timezone, dlstime, 6.0D0)
      call dectohrs(retval, ihrs, imins, isecs)
      print 99002, 'Civil dawn (sun is 6 degrees below horizon) ', ihrs, ':',   &
          & imins, ':', isecs
      retval = sunrise(lat, lon, year, month, day, timezone, dlstime)
      call dectohrs(retval, ihrs, imins, isecs)
      print 99002,                                                              &
       &'Sunrise (sun is 0.833 degrees below horizon to account for refraction)'&
      & , ihrs, ':', imins, ':', isecs
      retval = solarnoon(lat, lon, year, month, day, timezone, dlstime)
      call dectohrs(retval, ihrs, imins, isecs)
      print 99002,&
           &'Solar noon (sun is at its highest point in the sky for this day)', &
          & ihrs, ':', imins, ':', isecs
      retval = sunset(lat, lon, year, month, day, timezone, dlstime)
      call dectohrs(retval, ihrs, imins, isecs)
      print 99002,                                                              &
        &'Sunset (sun is 0.833 degrees below horizon to account for refraction)'&
       & , ihrs, ':', imins, ':', isecs
      retval = dusk(lat, lon, year, month, day, timezone, dlstime, 6.0D0)
      call dectohrs(retval, ihrs, imins, isecs)
      print 99002, 'Civil dusk (sun is 6 degrees below horizon)', ihrs, ':',    &
          & imins, ':', isecs
      retval = dusk(lat, lon, year, month, day, timezone, dlstime, 12.0D0)
      call dectohrs(retval, ihrs, imins, isecs)
      print 99002, 'Nautical dusk (sun is 12 degrees below horizon)', ihrs, ':',&
          & imins, ':', isecs
 
      retval = dusk(lat, lon, year, month, day, timezone, dlstime, 18.0D0)
      call dectohrs(retval, ihrs, imins, isecs)
      print 99002, 'Astronomical dusk (sun is 18 degrees below horizon)', ihrs, &
           &':', imins, ':', isecs
      call cpu_time(ends)
 
      write(*, '(a)')'Now writing out a year for an analemma plot'
      open(unit = 2, file = 'analemma.sat', status = 'Unknown')
      do j = 0, 365
          day = oldday + real(j, r64)
          t1 = solarazimuth(lat, lon, int(year), int(month), int(day), int(hours), &
          int(minutes), seconds, int(timezone), int(dlstime))
          if( (t1>300.0) )t1 = t1 - 360.0 
          azimuth(j) = t1
          t2 = solarelevation(lat, lon, year, month, day, hours, minutes,       &
             & seconds, timezone, dlstime)
          altitude(j) = t2
          line(j) = j
          if( j==0 )line2 = t1
          write(2, *)t1, t2
      end do
      close(unit = 2)
      plot = initialize_plot()
      call set_title(plot, title)
      call set_xlabel(plot, "Azimuth")
      call set_ylabel(plot, "Altitude")
      xer = minval(azimuth(1:nday))  !+1.0!0.1*minval(azimuth(1:nday))
 
      yer = minval(altitude(0:nday))  !+0.1*maxval(altitude(1:nday))
      call set_yscale(plot, yer, maxval(altitude(0:nday)*1.0001))
      call add_dataset(plot, azimuth(0:nday), altitude(0:nday))
      call set_seriescolor(plot, 0, 0, 0, 255)
      call set_serieslabel(plot, 0, metime)
      call set_seriestype(plot, 0, aplot_style_line)
 
      call add_dataset(plot, line2(0:nday), line(0:nday))
      call set_seriescolor(plot, 1, 255, 0, 0)
      call set_seriestype(plot, 1, aplot_style_line)
      write(string, '(i0,a,i0,a,i0)')values(1), '-', values(2), '-', values(3)
      call set_serieslabel(plot, 1, string)
      call display_plot(plot)
 
      call destroy_plot(plot)
      write(*, '(/,a,1x,f6.4,1x,a)')'Completed in', ends - start,               &
                                   &'milliseconds CPU time'
                                   
!      call DecimalToDMS(decimalValue, degrees, minutes, seconds)
      metime = '';string = ''
      write(metime,'(f12.6)')lat
      metime = trim(adjustl(metime)) //' = '
      call DecimalToDMS(lat, degrees, imins, seconds)
      write(string,'(i4.3,a1,i2.2,a1)')degrees,':',imins,':'
      metime = trim(metime)//' '//trim(string)
      write(string,'(F10.6)')seconds
      metime=trim(metime)//trim(adjustl(string))
      print*,trim(metime)
      call DecimalToDMS(lat, degrees, imins, seconds)
      print*, 'Latitude = ',degrees,imins,seconds
      call DecimalToDMS(lon, degrees, imins, seconds)
      print*, 'Longitude = ',degrees,imins,seconds
      print*,'october 1 2023 day',get_the_day(7,04,2024)
      stop ' Normal Finish' // char(10) // char(13) !Add Carriage return & Linefeed
99001 format(a, t40, f8.4)
99002 format(a, t80, i2.2, a, i2.2, a, i2.2)
      contains
      pure subroutine dectohrs(intim, hrs, mins, secs)   !Convert decimal time to hours,mins,secs
          use iso_fortran_env,only:int32
      implicit none
!
! Dummy arguments
!
      integer  ::  hrs
      real(r64)  ::  intim
      integer  ::  mins
      integer  ::  secs
      intent (in) intim
      intent (out) hrs, secs
      intent (out) mins
!
! Local variables
!
      real(r64)  ::  intim2
      real(r64)  ::  temp1
      real(r64)  ::  temp2
!
!*Code                                                                  
!
      intim2 = intim*24.0D0
      temp1 = aint(intim2, r64)
      hrs = int(temp1)
      temp1 = intim2 - temp1 !Ony minutes and sec now
      temp2 = temp1*60.0D0
      mins = int(temp2)
      secs = nint((temp1 - (real(mins, r64)/60.0D0))*60.0D0**2, int32) !Return nearest second
      return
      end subroutine dectohrs
!
    subroutine DecimalToDMS(dec, degrees, minutes, seconds)
    implicit none
    real(8) :: decimalValue
    integer, intent(out) :: degrees, minutes
    real(8), intent(out) :: seconds
    real(8), intent(in) :: dec
    real(8) :: tempValue
    real(8):: mesign

    mesign = sign(1.0d0,dec)
    decimalValue = abs(dec)
    ! Convert the decimal degrees to degrees, minutes, and seconds
    degrees = int(decimalValue)
    tempValue = (decimalValue - real(degrees, 8)) * 60.0d0
    minutes = int(tempValue)
    seconds = (tempValue - real(minutes, 8)) * 60.0d0
    degrees = int(mesign*degrees)
    end subroutine DecimalToDMS
!
      FUNCTION GET_ANCHOR_DAY(Year) RESULT(ANSWER)      ! Returns Anchor Days in doomsday calculation
!                                                         Note: The days start as Monday = 1, Tuesday =2, etc until Sunday = 7
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER  ::  Year
      INTENT (IN) Year
!
! Local variables
!
      INTEGER  ::  ANSWER
      INTEGER  ::  diffyear , div12 , numyears , temp1  ! Scratch variables
!
      numyears = MOD(Year,100)                          ! Get number of years greater than century
      temp1 = Year - numyears                           ! Turn into a century year
      temp1 = MOD(temp1,400)                            ! Now mod 400 to get base year for anchor day
      SELECT CASE(temp1)                                ! Select the base day
      CASE(0)
         ANSWER = TUESDAY
      CASE(100)
         ANSWER = SUNDAY
      CASE(200)
         ANSWER = FRIDAY
      CASE(300)
         ANSWER = WEDNESDAY
      CASE DEFAULT                                      ! Anything else is an error
         STOP 'Bad Anchor Day'                          ! Finish with error
      ENDSELECT
!
!Calculate the doomsday of any given year
!
      div12 = INT(numyears/12)                       ! Get number of times 12 goes into year
      temp1 = MOD(numyears,12)                       ! See the above lines
      diffyear = INT(temp1/4)                        ! Div 4 (magic algorithm)
      ANSWER = diffyear + div12 + ANSWER + temp1
      ANSWER = MOD(ANSWER,7)
      RETURN
      END FUNCTION GET_ANCHOR_DAY                    ! Note: The days start as Sunday = 0, Monday = 1, Tuesday =2, etc until Saturday = 6
!
      PURE FUNCTION GREGORIAN_LEAP_YEAR(Year) RESULT(ANSWER)       ! Tests if the year is a leap year
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER  ::  Year
      INTENT (IN) Year                                      ! Set year to be unmodifiable
!
! Local variables
!
      LOGICAL  ::  ANSWER
!
      ANSWER = .FALSE.                                      ! Set default to not a leap year
      IF( MOD(Year,4).EQ.0 )ANSWER = .TRUE.                 ! Year divisible by 4 = leap year
      IF( MOD(Year,100).EQ.0 )THEN
         IF( MOD(Year,400).EQ.0 )THEN                        ! Year divisible by 400 = century year that is leap year
            ANSWER = .TRUE.
         ELSE
            ANSWER = .FALSE.                                 ! Century years are not leap years
         ENDIF
      ENDIF
      RETURN
      END FUNCTION GREGORIAN_LEAP_YEAR
!
      FUNCTION GET_THE_DAY(Day,Month,Year) RESULT(ANSWER)     ! Note: The days start as Sunday = 0, Monday = 1, Tuesday =2, etc until Saturday = 6
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER  ::  Day , Month , Year
      INTENT (IN) Day , Month , Year
!
! Local variables
!
      INTEGER  ::  ANSWER
      INTEGER  ::  closest , doomsday , temp1 , temp2 , up_or_down
!
!
    ! There are doomsdays in every month, so we know what month it is ...
    ! We need to find the doomsday in the relevant month
      SELECT CASE(Month)
      CASE(JANUARY)
         IF( GREGORIAN_LEAP_YEAR(Year) )THEN            ! If leap year it's the 3rd no leap year = 4th
            closest = 4
         ELSE
            closest = 3
         ENDIF
      CASE(FEBRUARY)
         IF( GREGORIAN_LEAP_YEAR(Year) )THEN            ! Always the last day of February
            closest = 29
         ELSE
            closest = 28
         ENDIF
      CASE(MARCH)
         closest = 7
      CASE(APRIL)
         closest = 4
      CASE(MAY)
         closest = 9
      CASE(JUNE)
         closest = 6
      CASE(JULY)
         temp1 = ABS(4-Day)
         temp2 = ABS(11-Day)
         IF( temp1.LT.temp2 )THEN                       ! Choose closest, two days in this month
            closest = 4
         ELSE
            closest = 11
         ENDIF
      CASE(AUGUST)
         closest = 8
      CASE(SEPTEMBER)
         closest = 5
      CASE(OCTOBER)
         temp1 = ABS(10-Day)
         temp2 = ABS(31-Day)
         IF( temp1.LT.temp2 )THEN                       ! Choose closest, two days in this month
            closest = 10
         ELSE
            closest = 31
         ENDIF
      CASE(NOVEMBER)
         closest = 7
      CASE(DECEMBER)
         closest = 12
      CASE DEFAULT
         STOP 'Error in get the day'                   ! Stop on error
      ENDSELECT
!
     ! Ok now we get the doomsday in question - i.e. Monday, Tuesday for this year
      doomsday = GET_ANCHOR_DAY(Year)                   ! Get this years doomsday
    ! If closest day is less we need to count down, if it is bigger we count up
      IF( closest.GT.Day )THEN
         up_or_down = -7
      ELSEIF( closest.LT.Day )THEN
         up_or_down = 7
      ELSE
         up_or_down = 0                             ! The days are equal. Set to zero so no counting needed
      ENDIF
      temp1 = closest                               ! Set temp var to closest doomsday
      IF( up_or_down.GT.0 )THEN
         DO WHILE ( temp1.LE.Day )
            temp2 = temp1
            temp1 = temp1 + up_or_down              ! Count in sevens to the final
         ENDDO
         temp1 = Day - temp2
         temp1 = (doomsday+7) + temp1
      ELSEIF( up_or_down.LT.0 )THEN
         DO WHILE ( temp1.GE.Day )
            temp2 = temp1
            temp1 = temp1 + up_or_down              ! Count in sevens to the final
         ENDDO
         temp1 = temp2 - Day                        ! See how far away I am from this day
         temp1 = (doomsday+7) - temp1               ! Subtract the difference in days from the known doomsday
      ELSE
         temp1 = doomsday                           ! It fell on the doomsday
      ENDIF
      ANSWER = MOD(temp1,7)                         ! Turn Sundays into Zeros
      RETURN
      END FUNCTION GET_THE_DAY
      end program driver
