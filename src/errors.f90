module errors
   implicit none
   public

   integer, parameter :: JME_OK = 0
   integer, parameter :: JME_YEAR_OUT_OF_RANGE = -1
   integer, parameter :: JME_MONTH_OUT_OF_RANGE = -2
   integer, parameter :: JME_DAY_OUT_OF_RANGE = -3
   integer, parameter :: JME_JULIAN_DAY_OUT_OF_RANGE = -4
   integer, parameter :: JME_GEO_POSITION_INVALID = -5
   integer, parameter :: JME_INTERNAL = -16383
end module errors
