!The calculations in the NOAA Sunrise/Sunset and Solar Position Calculators are based on equations
!from Astronomical Algorithms, by Jean Meeus. NOAA also included atmospheric refraction effects. The sunrise and sunset results 
!were reported by NOAA to be accurate to within +/- 1 minute for locations between +/- 72° latitude, and within ten minutes 
!outside of those latitudes.

!This FORTRAN translation was tested for selected locations and found to provide results identical the original NOAA &
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
        module solarazi
        use functions
        use trig_degs
        implicit none
!
! PARAMETER definitions
!
      integer, parameter,private  ::  r64 = selected_real_kind(15, 307)

      contains
      function solarazimuth(lat,lon,year,month,day,hours,minutes,       &
                          & seconds,timezone,dlstime)
      implicit none
!
! Dummy arguments
!
      integer :: day
      integer :: dlstime
      integer :: hours
      real(r64) :: lat
      real(r64) :: lon
      integer :: minutes
      integer:: month
      real(r64) :: seconds
      integer :: timezone
      integer :: year
      real(r64) :: solarazimuth
      intent (in) day,dlstime,hours,lat,lon,minutes,month,seconds,      &
                & timezone,year
!
! Local variables
!
      real(r64) :: alpha
      real(r64) :: azdenom
      real(r64) :: azimuth
      real(r64) :: azrad
      real(r64) :: csz
      real(r64) :: daysavings
      real(r64) :: earthradvec
      real(r64) :: eqtime
      real(r64) :: etime
      real(r64) :: exoatmelevation
      real(r64) :: harad,hh
      real(r64) :: hourangle
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude,mm
      real(r64) :: r
      real(r64) :: refractioncorrection
      real(r64) :: solardec
      real(r64) :: solartimefix
      real(r64) :: solarzen
      real(r64) :: step1
      real(r64) :: step2
      real(r64) :: step3,ss
      real(r64) :: t
      real(r64) :: te

      real(r64) :: theta
      real(r64) :: timenow
      real(r64) :: truesolartime
      real(r64) :: zenith
      real(r64) :: zone
!
      longitude=-lon
      latitude=lat
      if( latitude>89.8D0 )then
         latitude=89.8D0
      else if( latitude<-89.8D0 )then
         latitude=-89.8D0
      end if
!change time zone to ppositive hours in western hemisphere
        zone = -(timezone)
        if(dlstime.ne.0)then
            hh = hours-1
        else
            hh = hours
        endif        
        mm = minutes
        ss = seconds

!'//    timenow is GMT time for calculation in hours since 0Z
      timenow = hh + (mm / 60.0d0) + (ss / 3600.0d0) + zone
      jd=calcjd(int(year),int(month),int(day))
      t=calctimejuliancent((jd+(timenow/24.0D0)))
      r=calcsunradvector(t)
      alpha=calcsunrtascension(t)
      theta=calcsundeclination(t)
      etime=calcequationoftime(t)

      eqtime=etime
      solardec=theta  ! in degrees
      earthradvec=r

      solartimefix=eqtime-4.0D0*longitude+60.0D0*zone
      truesolartime=hh*60.0D0+minutes+seconds/60.0D0+solartimefix

!      do while ( truesolartime>1440.0D0 )
!         truesolartime=truesolartime-1440.0D0
!      end do
      if(truesolartime > 1440.0d0)truesolartime = mod(truesolartime,1440.0d0)
      
      hourangle=truesolartime/4.0D0-180.0D0
      if( hourangle<-180.0D0 )hourangle=hourangle+360.0D0

      csz=sind(latitude)*sind(solardec)+cosd(latitude)*cosd(solardec)*cosd(hourangle)
      if( csz>1.0D0 )csz=1.0D0
      if( csz<-1.0D0 )csz=-1.0D0
      zenith=acosd(csz)
      azdenom=cosd(latitude)*sin(acos(csz))
      if( abs(azdenom)>0.001D0 )then
         azrad=(sind(latitude)*csz-sind(solardec))/azdenom
         if( abs(azrad)>1.0D0 )then
            if( azrad<0.0D0 )then
               azrad=-1.0D0
            else
               azrad=1.0D0
            end if
         end if
         azimuth=180.0D0- acosd(azrad)
         if( hourangle>0.0D0 )azimuth=-azimuth
      else if( latitude>0.0D0 )then
         azimuth=180.0D0
      else
         azimuth=0.0D0
      end if

      if( azimuth<0.0D0 )azimuth=azimuth+360.0D0
!   Azimuth appears correct
      exoatmelevation=90.0D0-zenith
      if( exoatmelevation>85.0D0 )then
         refractioncorrection=0.0D0
      else
         te=tand(exoatmelevation)
         if( exoatmelevation>5.0D0 )then
            refractioncorrection=58.1D0/te-0.07D0/(te**3)+0.000086D0/(te**5)
         else if( exoatmelevation>-0.575D0 )then
            step1=(-12.79D0+exoatmelevation*0.711D0)
            step2=(103.4D0+exoatmelevation*(step1))
            step3=(-518.2D0+exoatmelevation*(step2))
            refractioncorrection=1735.0D0+exoatmelevation*(step3)
         else
            refractioncorrection=-20.774D0/te
         end if
         refractioncorrection=refractioncorrection/3600.0D0
      end if
      solarzen=zenith-refractioncorrection
      solarazimuth=azimuth
      return
      end function solarazimuth
      end module solarazi