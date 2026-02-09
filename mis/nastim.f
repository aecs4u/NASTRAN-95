      SUBROUTINE NASTIM (IHR, IMN, ISC, CPUSEC)
      REAL ARRAY(2)
      SECS = ETIME(ARRAY)
C     SECS now contains total CPU time (user+system)
      IHR    = SECS / 3600.  
      IMN    = ( SECS - 3600.*IHR ) / 60.
      ISC    = SECS - ( 3600.*IHR ) - ( 60.*IMN )
      CPUSEC = SECS
      RETURN
      END
