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
!Translation from VBA to Fortran 90 by:
!Peter Kelly
!e-mail: peter.kelly@acm.org
!Original comments are retained
!
      module functions
      use smalls
      implicit none
!
! PARAMETER definitions
!
      integer,private,parameter :: r64=selected_real_kind(15,307)
!
      contains

! Function to calculate the Universal Coordinated Time (UTC) of dawn
      function calcdawnutc(jd,latitude,longitude,solardepression)
      implicit none
!
! Dummy arguments
!
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude
      real(r64) :: solardepression
      real(r64) :: calcdawnutc
      intent (in) jd,latitude,longitude,solardepression
!
! Local variables
!
      real(r64) :: delta
      real(r64) :: eqtime
      real(r64) :: hourangle
      real(r64) :: newt
      real(r64) :: solardec
      real(r64) :: t
      real(r64) :: temp1
      real(r64) :: timediff
      real(r64) :: timeutc

      t=calctimejuliancent(jd)

      eqtime=calcequationoftime(t)
      solardec=calcsundeclination(t)
      hourangle=calchouranglesunrise(latitude,solardec)

      delta=longitude-hourangle
      timediff=4.0D0*delta
      timeutc=720.0D0+timediff-eqtime
      temp1=calcjdfromjuliancent(t)+(timeutc/1440.0D0)
      newt=calctimejuliancent(temp1)
      eqtime=calcequationoftime(newt)
      solardec=calcsundeclination(newt)
      hourangle=calchourangledawn(latitude,solardec,solardepression)
      delta=longitude-hourangle
      timediff=4.0D0*delta
      timeutc=720.0D0+timediff-eqtime
      calcdawnutc=timeutc
      return
      end function calcdawnutc

! Function to calculate the Universal Coordinated Time (UTC) of sunrise
      function calcsunriseutc(jd,latitude,longitude)
      implicit none
!
! Dummy arguments
!
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude
      real(r64) :: calcsunriseutc
      intent (in) jd,latitude,longitude
!
! Local variables
!
      real(r64) :: delta
      real(r64) :: eqtime
      real(r64) :: hourangle
      real(r64) :: newt
      real(r64) :: solardec
      real(r64) :: t
      real(r64) :: timediff
      real(r64) :: timeutc
!
      t=calctimejuliancent(jd)

      eqtime=calcequationoftime(t)
      solardec=calcsundeclination(t)
      hourangle=calchouranglesunrise(latitude,solardec)

      delta=longitude-hourangle
      timediff=4.0D0*delta
      timeutc=720.0D0+timediff-eqtime

      newt=calctimejuliancent(calcjdfromjuliancent(t)+timeutc/1440.0D0)
      eqtime=calcequationoftime(newt)
      solardec=calcsundeclination(newt)
      hourangle=calchouranglesunrise(latitude,solardec)
      delta=longitude-hourangle
      timediff=4.0D0*delta
      timeutc=720.0D0+timediff-eqtime

      calcsunriseutc=timeutc
      return
      end function calcsunriseutc         
! Function to calculate the Universal Coordinated Time (UTC) of solar noon
      function calcsolnoonutc(t,longitude)
      implicit none
!
! Dummy arguments
!
      real(r64) :: longitude
      real(r64) :: t
      real(r64) :: calcsolnoonutc
      intent (in) longitude,t
!
! Local variables
!
      real(r64) :: eqtime
      real(r64) :: newt,temp
      real(r64) :: solarnoondec
      real(r64) :: solnoonutc
!
      temp = calcjdfromjuliancent(t)
      newt=calctimejuliancent(temp +0.5D0+longitude/360.0D0)

      eqtime=calcequationoftime(newt)
      solarnoondec=calcsundeclination(newt)
      solnoonutc=720.0D0+(longitude*4.0D0)-eqtime
      calcsolnoonutc=solnoonutc
      return
      end function calcsolnoonutc
      
      function calcsunsetutc(jd,latitude,longitude)
      implicit none
!
! Dummy arguments
!
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude
      real(r64) :: calcsunsetutc
      intent (in) jd,latitude,longitude
!
! Local variables
!
      real(r64) :: delta
      real(r64) :: eqtime
      real(r64) :: hourangle
      real(r64) :: newt
      real(r64) :: solardec
      real(r64) :: t
      real(r64) :: timediff
      real(r64) :: timeutc
!
      t=calctimejuliancent(jd)

      eqtime=calcequationoftime(t)
      solardec=calcsundeclination(t)
      hourangle=calchouranglesunset(latitude,solardec)

      delta=longitude-hourangle
      timediff=4.0D0*delta
      timeutc=720.0D0+timediff-eqtime
      newt=calcjdfromjuliancent(t)+timeutc/1440.0D0
      newt=calctimejuliancent(newt)
      eqtime=calcequationoftime(newt)
      solardec=calcsundeclination(newt)
      hourangle=calchouranglesunset(latitude,solardec)
      delta=longitude-hourangle
      timediff=4.0D0*delta
      timeutc=720.0D0+timediff-eqtime       ! In minutes

      calcsunsetutc=timeutc
      return
      end function calcsunsetutc         

      function calcduskutc(jd,latitude,longitude,solardepression)
      implicit none
!
! Dummy arguments
!
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude
      real(r64) :: solardepression
      real(r64) :: calcduskutc
      intent (in) jd,latitude,longitude,solardepression
!
! Local variables
!
      real(r64) :: delta
      real(r64) :: eqtime
      real(r64) :: hourangle
      real(r64) :: newt
      real(r64) :: solardec
      real(r64) :: t
      real(r64) :: timediff
      real(r64) :: timeutc

      t=calctimejuliancent(jd)

      eqtime=calcequationoftime(t)
      solardec=calcsundeclination(t)
      hourangle=calchouranglesunset(latitude,solardec)

      delta=longitude-hourangle
      timediff=4.0D0*delta
      timeutc=720.0D0+timediff-eqtime

      newt=calctimejuliancent(calcjdfromjuliancent(t)+timeutc/1440.0D0)
      eqtime=calcequationoftime(newt)
      solardec=calcsundeclination(newt)
      hourangle=calchourangledusk(latitude,solardec,solardepression)

      delta=longitude-radtodeg(hourangle)
      timediff=4.0D0*delta
      timeutc=720.0D0+timediff-eqtime      ! In minutes

      calcduskutc=timeutc
      return
      end function calcduskutc         

      function dawn(lat,lon,year,month,day,timezone,dlstime,solardepression)
      implicit none
!
! Dummy arguments
!
      real(r64) :: day
      real(r64) :: dlstime
      real(r64) :: lat
      real(r64) :: lon
      real(r64) :: month
      real(r64) :: solardepression
      real(r64) :: timezone
      real(r64) :: year
      real(r64) :: dawn
      intent (in) day,dlstime,lat,lon,month,solardepression,timezone, year
!
! Local variables
!
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude
      real(r64) :: risetimegmt
      real(r64) :: risetimelst
!
      longitude=-lon
      latitude=lat
      if( latitude>89.8 )latitude=89.8
      if( latitude<-89.8 )latitude=-89.8

      jd=calcjd(int(year),int(month),int(day))
      risetimegmt=calcdawnutc(jd,latitude,longitude,solardepression)
      risetimelst=risetimegmt+(60.0D0*timezone)+(dlstime*60.0D0)
      dawn=risetimelst/1440.0D0
      return
      end function dawn         

      function sunrise(lat,lon,year,month,day,timezone,dlstime)
      implicit none
!
! Dummy arguments
!
      real(r64) :: day
      real(r64) :: dlstime
      real(r64) :: lat
      real(r64) :: lon
      real(r64) :: month
      real(r64) :: timezone
      real(r64) :: year
      real(r64) :: sunrise
      intent (in) day,dlstime,lat,lon,month,timezone,year
!
! Local variables
!
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude
      real(r64) :: risetimegmt
      real(r64) :: risetimelst

      longitude=-lon
      latitude=lat
      if( latitude>89.8 )latitude=89.8
      if( latitude<-89.8 )latitude=-89.8

      jd=calcjd(int(year),int(month),int(day))
      risetimegmt=calcsunriseutc(jd,latitude,longitude)
      risetimelst=risetimegmt+(60.0D0*timezone)+(dlstime*60.0D0)
      sunrise=risetimelst/1440.0D0
      return
      end function sunrise         

      function solarnoon(lat,lon,year,month,day,timezone,dlstime)
      implicit none
!
! Dummy arguments
!
      real(r64) :: day
      real(r64) :: dlstime
      real(r64) :: lat
      real(r64) :: lon
      real(r64) :: month
      real(r64) :: timezone
      real(r64) :: year
      real(r64) :: solarnoon
      intent (in) day,dlstime,lat,lon,month,timezone,year
!
! Local variables
!
      real(r64) :: eqtime
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude
      real(r64) :: newt
      real(r64) :: solarnoondec
      real(r64) :: solnoonutc
      real(r64) :: t,temp
!
      longitude=-lon
      latitude=lat
      if( latitude>89.8d0 )latitude=89.8d0
      if( latitude<-89.8d0 )latitude=-89.8d0

      jd=calcjd(int(year),int(month),int(day))
      t=calctimejuliancent(jd)
      temp = calcjdfromjuliancent(t) +0.5D0 + (longitude/360.0D0)
      newt=calctimejuliancent(temp)
      eqtime=calcequationoftime(newt)
      solarnoondec=calcsundeclination(newt)
      solnoonutc=720.0D0+(longitude*4.0D0)-eqtime
      solarnoon=solnoonutc+(60.0D0*timezone)+(dlstime*60.0D0)
      solarnoon=solarnoon/1440.0D0
      return
      end function solarnoon         
!
      function sunset(lat,lon,year,month,day,timezone,dlstime)
      implicit none
!
! Dummy arguments
!
      real(r64) :: day
      real(r64) :: dlstime
      real(r64) :: lat
      real(r64) :: lon
      real(r64) :: month
      real(r64) :: timezone
      real(r64) :: year
      real(r64) :: sunset
      intent (in) day,dlstime,lat,lon,month,timezone,year
!
! Local variables
!
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude
      real(r64) :: settimegmt
      real(r64) :: settimelst
!
      longitude=-lon
      latitude=lat
      if( latitude>89.8 )latitude=89.8
      if( latitude<-89.8 )latitude=-89.8

      jd=calcjd(int(year),int(month),int(day))
      settimegmt=calcsunsetutc(jd,latitude,longitude)
      settimelst=settimegmt+(60.0D0*timezone)+(dlstime*60.0D0)
      sunset=settimelst/1440.0D0
      return
      end function sunset         

      function dusk(lat,lon,year,month,day,timezone,dlstime,solardepression)
      implicit none
!
! Dummy arguments
!
      real(r64) :: day
      real(r64) :: dlstime
      real(r64) :: lat
      real(r64) :: lon
      real(r64) :: month
      real(r64) :: solardepression
      real(r64) :: timezone
      real(r64) :: year
      real(r64) :: dusk
      intent (in) day,dlstime,lat,lon,month,solardepression,timezone,year
!
! Local variables
!
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude
      real(r64) :: settimegmt
      real(r64) :: settimelst
!
      longitude=-lon
      latitude=lat
      if( latitude>89.8 )latitude=89.8
      if( latitude<-89.8 )latitude=-89.8

      jd=calcjd(int(year),int(month),int(day))
      settimegmt=calcduskutc(jd,latitude,longitude,solardepression)
      settimelst=settimegmt+(60.0D0*timezone)+(dlstime*60.0D0)
      dusk=settimelst/1440.0D0
      return
      end function dusk           
!
       subroutine solarposition(lat,lon,year,month,day,hours,minutes,    &
                             & seconds,timezone,dlstime,solarazimuth,   &
                             & solarelevation)
      implicit none
!
! Dummy arguments
!
      real(r64) :: day
      real(r64) :: dlstime
      real(r64) :: hours
      real(r64) :: lat
      real(r64) :: lon
      real(r64) :: minutes
      real(r64) :: month
      real(r64) :: seconds
      real(r64) :: solarazimuth
      real(r64) :: solarelevation
      real(r64) :: timezone
      real(r64) :: year
      intent (in) day,dlstime,hours,lat,lon,minutes,month,seconds,timezone,year
      intent (out) solarazimuth,solarelevation
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
      real(r64) :: hourangle
      real(r64) :: jd
      real(r64) :: latitude
      real(r64) :: longitude
      real(r64) :: r
      real(r64) :: refractioncorrection
      real(r64) :: solardec
      real(r64) :: solartimefix
      real(r64) :: solarzen
      real(r64) :: step1
      real(r64) :: step2
      real(r64) :: step3
      real(r64) :: t
      real(r64) :: te

      real(r64) :: theta
      real(r64) :: timenow
      real(r64) :: truesolartime
      real(r64) :: zenith
      real(r64) :: zone
!*Code
!
      longitude=-lon
      latitude=lat
      if( latitude>89.8D0 )latitude=89.8D0
      if( latitude<-89.8D0 )latitude=-89.8D0

      zone=-timezone
      daysavings=dlstime*60.0D0
      timenow=hours-(daysavings/60.0D0)

      jd=calcjd(int(year),int(month),int(day))
      t=calctimejuliancent(jd+timenow/24.0D0)
      r=calcsunradvector(t)
      alpha=calcsunrtascension(t)
      theta=calcsundeclination(t)
      etime=calcequationoftime(t)

      eqtime=etime
      solardec=theta ! In degrees
      earthradvec=r

      solartimefix=eqtime-4.0D0*longitude+60.0D0*zone
      truesolartime=hours*60.0D0+minutes+seconds/60.0D0+solartimefix
!      do while ( truesolartime>1440.0D0 )
!         truesolartime=truesolartime-1440.0D0
!      end do
      if(truesolartime > 1440.0d0)truesolartime = mod(truesolartime,1440.0d0)    
      hourangle=truesolartime/4.0D0-180.0D0     ! In degrees
      if( hourangle < (-180.0D0) )hourangle=hourangle+360.0D0

      csz=sind(latitude)*sind(solardec)+cosd(latitude)*cosd(solardec)*cosd(hourangle)
      if( csz>1.0D0 )csz=1.0D0
      if( csz<-1.0D0 )csz=-1.0D0
!
      zenith= acosd(csz)!
      azdenom=cosd(latitude)*sind(zenith)

      if( abs(azdenom)>0.001D0 )then
         azrad=(sind(latitude)*cosd(zenith)-sind(solardec))/azdenom
         if( abs(azrad)>1.0D0 )then
!            if( azrad<0.0D0 )then
!               azrad=-1.0D0
!            else
!               azrad=1.0D0
!            end if
            azrad =  1.0d0*sign(1.0d0,azrad)
         end if

         azimuth=180.0D0-acosd(azrad)!
         if( hourangle>0.0D0 )azimuth=-azimuth
      else if( latitude>0.0D0 )then
         azimuth=180.0D0
      else
         azimuth=0.0D0
      end if

      if( azimuth<0.0D0 )azimuth=azimuth+360.0D0
      exoatmelevation=90.0D0-zenith

      if( exoatmelevation>85.0D0 )then
         refractioncorrection=0
      else
         te=tand(exoatmelevation)
         if( exoatmelevation>5.0D0 )then
            refractioncorrection=(58.1D0/te)-(0.07D0/(te**3)) +(0.000086D0/(te**5))
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
      if( solarzen<108.0D0 )then
         solarazimuth=azimuth
         solarelevation=90.0D0-solarzen
      else
         solarazimuth=-999999
         solarelevation=-999999
      end if
      return
      end subroutine solarposition
!
      end module functions
