!The calculations in the NOAA Sunrise/Sunset and Solar Position Calculators are based on equations
!from Astronomical Algorithms, by Jean Meeus. NOAA also included atmospheric refraction effects. The sunrise and sunset results 
!were reported by NOAA to be accurate to within +/- 1 minute for locations between +/- 72° latitude, and within ten minutes 
!outside of those latitudes.

!This FORTRAN translation was tested for selected locations and found to provide results identical to the original NOAA &
!Excel VBA code.

!For sunrise and sunset calculations, we assume 0.833° of atmospheric refraction.
!In the solar position functions, atmospheric refraction is modeled using equations documented at 
!    http://www.srrb.noaa.gov/highlights/sunrise/calcdetails.html

!This FORTRAN translation does not include calculation of prior or next sunsets for locations
! above the Arctic Circle and below the Antarctic Circle, when a sunrise or sunset does not occur.

!Translated from NOAA's Javascript to Excel VBA by:

!Greg Pelletier
!Department of Ecology
!P.O. Box 47710
!Olympia, WA 98504-7710
!phone: 360-407-6485
!fax: 360-407-6884
!e-mail: gpel461@ecy.wa.gov
!
!Translation to Fortran 90 by:
!Peter Kelly
!e-mail: peter.kelly@acm.org
!Original comments are retained
!
      module smalls
      use trig_degs
      implicit none
      integer,private,parameter :: r64=selected_real_kind(15,307)
!
      contains    !All the very small functions from the NOAA calculator
!
      function radtodeg(anglerad) result(angledeg)
      implicit none
!
! PARAMETER definitions
!
      real(r64),parameter :: todegs=180.0D0/acos(-1.0D0)
!
! Dummy arguments
!
      real(r64),intent (in) :: anglerad
!
! Local variables
!
      real(r64) :: angledeg
!
!*Code
!
  ! Convert radian angle to degrees
!
      angledeg=(anglerad*todegs)
      return
      end function radtodeg         

      function degtorad(angledeg) result(anglerad)
      implicit none
!
! PARAMETER definitions
!
      real(r64),parameter :: torads=acos(-1.0D0)/180.0D0
!
! Dummy arguments
!
      real(r64),intent (in) :: angledeg
!
! Local variables
!
      real(r64) :: anglerad
!
!*Code
!
  ! Convert degree angle to radians
      anglerad=(torads*angledeg)
      return
      end function degtorad         

      function calcjd(yyear,mmonth,day) result(jd)
      implicit none
!
! Dummy arguments
!
      integer :: day
      integer :: mmonth
      integer :: yyear
      intent (in) day,mmonth,yyear
!
! Local variables
!
      real(r64) :: a
      real(r64) :: b
      real(r64) :: jd
      integer :: month
      integer :: year
!
  ! Julian day from calendar day
  ! Arguments:
  !   year : 4 digit year
  !   month: January = 1
  !   day  : 1 - 31
  ! Return value:
  !   The Julian day corresponding to the date
  ! Note:
  !   Number is returned for start of day.  Fractional days should be
  !   added later.
!
      month=mmonth
      year=yyear
      if( month<=2 )then
         year=year-1
         month=month+12
      end if
      a=aint(year/100.0D0,r64)
      b=2.0D0-a+aint(a/4.0D0,r64)
      jd=aint(365.25D0*(year+4716.0D0),r64) &
       & +aint(30.6001D0*(month+1.0D0),r64)+dble(day)+b-1524.5D0
      return
      end function calcjd
!
      function calctimejuliancent(jd) result(t)
      implicit none
!
! Dummy arguments
!
      real(r64),intent (in) :: jd
!
! Local variables
!
      real(r64) :: t
!
  ! Convert Julian Day to centuries since J2000.0
!
      t=(jd-2451545.0D0)/36525.0D0     !Calculate Julian centuries
      return
      end function calctimejuliancent

      function calcjdfromjuliancent(t) result(jd)
      implicit none
!
! Dummy arguments
!
      real(r64),intent (in) :: t
!
! Local variables
!
      real(r64) :: jd
!
  ! Convert centuries since J2000.0 to Julian Day
!
      jd=t*36525.0D0+2451545.0D0
      return
      end function calcjdfromjuliancent         

      function calcgeommeanlongsun(t) result(l0)
      implicit none
!
! Dummy arguments
!
      real(r64),intent (in) :: t
!
! Local variables
!
      real(r64) :: l0
!*Code
!
  ! Calculate the Geometric Mean Longitude of the Sun
!
      l0=280.46646D0+t*(36000.76983D0+0.0003032D0*t)
      do
         if( (l0<=360.0D0) .and. (l0>=0.0D0) )exit
         if( l0>360.0D0 )l0=l0-360.0D0
         if( l0<0.0D0 )l0=l0+360.0D0
      end do
      return
      end function calcgeommeanlongsun         

       function calcgeommeananomalysun(t) result(m)
      implicit none
!
! Dummy arguments
!
      real(r64),intent (in) :: t
!
! Local variables
!
      real(r64) :: m
! Calculate the Geometric Mean Anomaly of the Sun
!
      m=357.52911D0+t*(35999.05029D0-0.0001537D0*t)
      return
      end function calcgeommeananomalysun         

      function calceccentricityearthorbit(t) result(e)
      implicit none
!
! Dummy arguments
!
      real(r64),intent (in) :: t
!
! Local variables
!
      real(r64) :: e
!
  ! Calculate the eccentricity of Earth's orbit
!
      e=0.016708634D0-t*(0.000042037D0+0.0000001267D0*t)
      return
      end function calceccentricityearthorbit         

       function calcsuneqofcenter(t) result(c)
  ! Calculate the equation of center for the Sun
      !
      implicit none
!
! Dummy arguments
!
      real(r64),intent (in) :: t
!
! Local variables
!
      real(r64) :: c
      real(r64) :: m
      real(r64) :: sin2m
      real(r64) :: sin3m
      real(r64) :: sinm
!
      m=calcgeommeananomalysun(t)

      sinm=sind(m)
      sin2m=sind(2.0D0*m)
      sin3m=sind(3.0D0*m)
      c=sinm*(1.914602D0-t*(0.004817D0+0.000014D0*t))
      c=c+sin2m*(0.019993D0-0.000101D0*t)+(sin3m*0.000289D0)
      return
      end function calcsuneqofcenter         

      function calcsuntruelong(t) result(o)
      implicit none
!
! Dummy arguments
!
      real(r64) ,intent (in) :: t
!
! Local variables
!
      real(r64) :: c
      real(r64) :: l0
      real(r64) :: o
!
  ! Calculate the true longitude of the Sun
!
      l0=calcgeommeanlongsun(t)
      c=calcsuneqofcenter(t)
      o=l0+c
      return
      end function calcsuntruelong         

      function calcsuntrueanomaly(t) result(v)
      implicit none
!
! Dummy arguments
!
      real(r64) :: t
      intent (in) t
!
! Local variables
!
      real(r64) :: c
      real(r64) :: m
      real(r64) :: v
!
  ! Calculate the true anomaly of the Sun (not used by sunrise, solarnoon, sunset)
!
      m=calcgeommeananomalysun(t)
      c=calcsuneqofcenter(t)
      v=m+c
      return
      end function calcsuntrueanomaly         

      function calcsunradvector(t) result(r)
      implicit none
!
! Dummy arguments
!
      real(r64) :: t
      intent (in) t
!
! Local variables
!
      real(r64) :: e
      real(r64) :: r
      real(r64) :: temp
      real(r64) :: v
!
  ! Calculate the distance to the Sun in AU (not used by sunrise, solarnoon, sunset)
!
      v=calcsuntrueanomaly(t)
      e=calceccentricityearthorbit(t)
!
      r=(1.000001018D0*(1.0D0-e**2))/(1.0D0+e*cosd(v))
      return
      end function calcsunradvector         
!                                                function to calculate the apparent longitude of the sun
      function calcsunapparentlong(t)
      implicit none
!
! Dummy arguments
!
      real(r64) :: t
      real(r64) :: calcsunapparentlong
      intent (in) t
!
! Local variables
!
      real(r64) :: lambda
      real(r64) :: o
      real(r64) :: omega
      real(r64) :: temp1
!
      o=calcsuntruelong(t)
      omega=125.04D0-1934.136D0*t
!
      lambda=o-0.00569D0-0.00478D0*sind(omega)
      calcsunapparentlong=lambda
      return
      end function calcsunapparentlong         

!function to calculate the mean obliquity of the ecliptic
      function calcmeanobliquityofecliptic(t)
      implicit none
!
! Dummy arguments
!
      real(r64) :: t
      real(r64) :: calcmeanobliquityofecliptic
      intent (in) t
!
! Local variables
!
      real(r64) :: e0
      real(r64) :: seconds
!
      seconds=21.448D0-t*(46.815D0+t*(0.00059D0-t*(0.001813D0)))
      e0=23.0D0+(26.0D0+(seconds/60.0D0))/60.0D0
      calcmeanobliquityofecliptic=e0
      return
      end function calcmeanobliquityofecliptic         

!function to calculate the corrected obliquity of the ecliptic
      function calcobliquitycorrection(t) result(e)
      implicit none
!
! Dummy arguments
!
      real(r64) :: t
      intent (in) t
!
! Local variables
!
      real(r64) :: e
      real(r64) :: e0
      real(r64) :: omega
      real(r64) :: temper
!
      e0=calcmeanobliquityofecliptic(t)
      omega=125.04D0-1934.136D0*t
      e=e0+0.00256D0*cosd(omega)
      return
      end function calcobliquitycorrection
!function to calculate the right ascension of the sun
      function calcsunrtascension(t)
      implicit none
!
! Dummy arguments
!
      real(r64) :: t
      real(r64) :: calcsunrtascension
      intent (in) t
!
! Local variables
!
      real(r64) :: alpha
      real(r64) :: e
      real(r64) :: lambda
      real(r64) :: tanadenom
      real(r64) :: tananum
      real(r64) :: temp1
      real(r64) :: temp2
      real(r64) :: temp3
!
      e=calcobliquitycorrection(t)
      lambda=calcsunapparentlong(t)

      tananum=(cosd(e)*sind(lambda))
      tanadenom=(cosd(lambda))
      calcsunrtascension=atand(tananum/tanadenom)
      return
      end function calcsunrtascension         
!                                                function to calculate the declination of the sun
      function calcsundeclination(t) result(q)
      implicit none
!
! Dummy arguments
!
      real(r64) :: t
      intent (in) t
!
! Local variables
!
      real(r64) :: e
      real(r64) :: lambda
      real(r64) :: q
      real(r64) :: sint
      real(r64) :: theta

      e=calcobliquitycorrection(t)
      lambda=calcsunapparentlong(t)
      sint=sind(e)*sind(lambda)
      q=asind(sint)
      return
      end function calcsundeclination         

! function to calculate the equation of time
       function calcequationoftime(t)
      implicit none
!
! Dummy arguments
!
      real(r64) :: t
      real(r64) :: calcequationoftime
      intent (in) t
!
! Local variables
!
      real(r64) :: cos2l0
      real(r64) :: dd
      real(r64) :: e
      real(r64) :: epsilonx
      real(r64) :: etime
      real(r64) :: l0
      real(r64) :: m
      real(r64) :: sin2l0
      real(r64) :: sin2m
      real(r64) :: sin4l0
      real(r64) :: sinm
      real(r64) :: temp1
      real(r64) :: y

      epsilonx=calcobliquitycorrection(t)
      l0=calcgeommeanlongsun(t)
      e=calceccentricityearthorbit(t)
      m=calcgeommeananomalysun(t)
!
      y=tand(epsilonx/2.0D0)
      y=y**2
      sin2l0=sind(2.0D0*l0)
      sinm=sind(m)
      cos2l0=cosd(2.0D0*l0)
      sin4l0=sind(4.0D0*l0)
      sin2m=sind(2.0D0*m)

      etime=(y*sin2l0)-(2.0D0*e*sinm)+(4.0D0*e*y*sinm*cos2l0)           &
          & -(0.5D0*(y**2)*sin4l0)-(1.25D0*(e**2)*sin2m)
!
      calcequationoftime=radtodeg(etime)*4.0d0
      return
      end function calcequationoftime

!                                                function to calculate the hour angle of dawn
      function calchourangledawn(lat,solardec,solardepression) result(ha)
      implicit none
!
! Dummy arguments
!
      real(r64) :: lat
      real(r64) :: solardec
      real(r64) :: solardepression
      intent (in) lat,solardec,solardepression
!
! Local variables
!
      real(r64) :: ha
      real(r64) :: haarg
      real(r64) :: latrad
      real(r64) :: sdrad

      ha = 90.0D0+solardepression
      haarg= (cosd(ha)/(cosd(lat)*cosd(solardec))-tand(lat)*tand(solardec))
      ha=acosd(haarg)
      return
      end function calchourangledawn         

!                                                function to calculate the hour angle of sunrise
      function calchouranglesunrise(lat,solardec) result(ha)
      implicit none
!
! Dummy arguments
!
      real(r64) :: lat
      real(r64) :: solardec
      intent (in) lat,solardec
!
! Local variables
!
      real(r64) :: ha
!
      ha=acosd(cosd(90.833D0)/(cosd(lat)*cosd(solardec))-tand(lat)*tand(solardec))
      return
      end function calchouranglesunrise         

! function to calculate the hour angle of sunset
      function calchouranglesunset(lat,solardec) result(ha)
      implicit none
!
! Dummy arguments
!
      real(r64) :: lat
      real(r64) :: solardec
      intent (in) lat,solardec
!
! Local variables
!
      real(r64) :: ha
      real(r64) :: haarg
!
      haarg=(cosd(90.833D0)/(cosd(lat)*cosd(solardec))-tand(lat)*tand(solardec))
      ha=-acosd(haarg)
      return
      end function calchouranglesunset         

! function to calculate the hour angle of dusk
      function calchourangledusk(lat,solardec,solardepression) result(ha)
      implicit none
!
! Dummy arguments
!
      real(r64) :: lat
      real(r64) :: solardec
      real(r64) :: solardepression
      intent (in) lat,solardec,solardepression
!
! Local variables
!
      real(r64) :: ha
      real(r64) :: haarg
!
      haarg=(cosd(90.0D0+solardepression)/(cosd(lat)*cosd(solardec))-tand(lat)*tand(solardec))
      ha=-acos(haarg)
      return
      end function calchourangledusk         
      end module smalls
