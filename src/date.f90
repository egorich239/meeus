module date
   use iso_c_binding, only: c_int, c_float, c_double
   implicit none
   private

   type, bind(c), public :: jd
      real(c_double) :: rep
   end type jd

   integer, parameter :: CAL_JULIAN = 0
   integer, parameter :: CAL_GREGORIAN = 1

   integer, parameter :: JME_OK = 0
   integer, parameter :: JME_YEAR_OUT_OF_RANGE = -1
   integer, parameter :: JME_MONTH_OUT_OF_RANGE = -2
   integer, parameter :: JME_DAY_OUT_OF_RANGE = -3

   public :: make_jd_gregorian
   public :: make_jd_julian

contains
   function make_jd_julian(y, m, d, out) result(err) bind(c)
      integer(c_int), value :: y, m
      real(c_float), value :: d
      type(jd), intent(out) :: out
      integer(c_int) :: err

      err = create_jd_(y, m, d, CAL_JULIAN, out)
   end function make_jd_julian

   function make_jd_gregorian(y, m, d, out) result(err) bind(c)
      integer(c_int), value :: y, m
      real(c_float), value :: d
      type(jd), intent(out) :: out
      integer(c_int) :: err

      err = create_jd_(y, m, d, CAL_GREGORIAN, out)
   end function make_jd_gregorian

   function create_jd_(y, m, d, c, out) result(err)
      integer(c_int), value :: y, m
      real(c_float), value :: d
      integer, intent(in) :: c
      type(jd), intent(out) :: out
      integer :: err

      integer(c_int) :: A, B, JD_AUX
      integer, parameter :: JD_YEAR_MIN = -4712

      select case (y)
      case (:JD_YEAR_MIN-2, 0)
         err = JME_YEAR_OUT_OF_RANGE
         return
      case (JD_YEAR_MIN-1:-1)
         y = y + 1
      end select

      select case (m)
      case (1:2)
         y = y - 1
         m = m + 12
      case (3:12)
      case default
         err = JME_MONTH_OUT_OF_RANGE
         return
      end select

      ! JM formulae:
      ! A = floor(y / 100)
      ! B = 2 - A + floor(A / 4)
      !
      ! We use integer division here instead, but have to shift values into the non-negative part:
      ! A' = A + 48 = floor((y + 4800) / 100)  > 0
      ! B = 2 - (A' - 48) + floor((A' - 48) / 4) = 38 - A' + floor(A' / 4)
      A = (Y + 4800) / 100
      select case (c)
      case (CAL_JULIAN)
         B = 0
      case (CAL_GREGORIAN)
         B = 38 - A + (A / 4)
      end select

      ! JD = (1) floor(365.25 * (Y + 4716)) + (2) floor(30.6001 * (M + 1)) + D + B - 1524.5
      ! (1) (Y + 4716 > 0) => 1461 * (Y + 4716) / 4
      ! (2) (JM notes that 30.6 would work with exact math) => 306 * (M + 1) / 10
      JD_AUX = (1461 * (Y + 4716) / 4) + (306 * (M + 1) / 10) + B

      out = jd(real(JD_AUX) + D - 1524.5)
      err = JME_OK
   end function create_jd_

end module date
