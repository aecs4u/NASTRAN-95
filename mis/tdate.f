      SUBROUTINE TDATE (DATE)        
C        
C     VAX VERSION        
C     ===========        
C     (ALSO SiliconGraphics, DEC/ultrix, and SUN.        
C      CRAY AND HP DO NOT HAVE IDATE)        
C        
C     THIS ROUTINE OBTAINS THE MONTH, DAY AND YEAR, IN INTEGER FORMAT   
C        
      INTEGER DATE(3), DATE1(3)        
C        
      CALL IDATE (DATE1)        
C                 DAY   MONTH     YEAR        
C     THESE DATES HAD TO BE INTERCHANGED FOR THE SUN
      DATE(1)=DATE1(2)
      DATE(2)=DATE1(1)
C     Use MOD to get last 2 digits of year (works for both 1900s and 2000s)
      DATE(3)=MOD(DATE1(3), 100)
      RETURN        
      END        
