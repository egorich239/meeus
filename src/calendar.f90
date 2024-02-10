module calendar
   use iso_c_binding, only: c_int, f32 => c_float, f64 => c_double

   use errors

   implicit none
   private

   type, bind(c), public :: julian_day
      real(f64) :: rep
   end type julian_day

   integer, parameter, public :: JULIAN_DAY_YEAR_MIN = -4712

   public :: julian_day_set_gregorian_date
   public :: julian_day_set_julian_date
   public :: julian_day_set_day_fraction
   ! public :: julian_day_get_theta0
   ! public :: julian_day_set_jd0

   type, bind(c), public :: gregorian_date
      ! NOTE: Years BC are shifted: Year 1 BC corresponds to year = 0
      integer(c_int) :: year
      integer(c_int) :: month
      real(f32) :: day
   end type gregorian_date

   public :: gregorian_date_set
   public :: gregorian_date_set_julian_day
   ! public :: gregorian_date_get_year
   ! public :: gregorian_date_get_month
   ! public :: gregorian_date_get_day

   type, bind(c), public :: julian_date
      ! NOTE: Years BC are shifted: Year 1 BC corresponds to year = 0
      integer(c_int) :: year
      integer(c_int) :: month
      real(f32) :: day
   end type julian_date

   public :: julian_date_set

   integer, parameter :: CAL_JULIAN = 0
   integer, parameter :: CAL_GREGORIAN = 1
contains
   function julian_day_set_julian_date(self, j) result(err) bind(c)
      type(julian_day), intent(out) :: self
      type(julian_date), intent(in) :: j
      integer(c_int) :: err

      err = julian_day_set_(self, j%year, j%month, j%day, CAL_JULIAN)
   end function julian_day_set_julian_date

   function julian_day_set_gregorian_date(self, g) result(err) bind(c)
      type(julian_day), intent(out) :: self
      type(gregorian_date), intent(in) :: g
      integer(c_int) :: err

      err = julian_day_set_(self, g%year, g%month, g%day, CAL_GREGORIAN)
   end function julian_day_set_gregorian_date

   function julian_day_set_day_fraction(self, f) result(err) bind(c)
      type(julian_day), intent(inout) :: self
      real(f32) :: f
      integer(c_int) :: err

      if (.not. (0.0 < f .and. f < 1.0)) then
         err = JME_DAY_OUT_OF_RANGE
         return
      endif

      self%rep = floor(self%rep) + real(f, kind=f64)
      err = JME_OK
   end function julian_day_set_day_fraction

   function gregorian_date_set(self, Y, M, D) result(err) bind(c)
      type(gregorian_date), intent(out) :: self
      integer(c_int), value :: Y, M
      real(f32), value :: D
      integer(c_int) :: err

      err = jg_date_set_(Y, M, D, CAL_GREGORIAN, self%year, self%month, self%day)
   end function gregorian_date_set

   function gregorian_date_set_julian_day(self, jd) result(err) bind(c)
      type(gregorian_date), intent(out) :: self
      type(julian_day), intent(in) :: jd
      integer(c_int) :: err

      err = julian_day_get_(jd, self%year, self%month, self%day)
   end function gregorian_date_set_julian_day

   function julian_date_set(self, Y, M, D) result(err) bind(c)
      type(gregorian_date), intent(out) :: self
      integer(c_int), value :: Y, M
      real(f32), value :: D
      integer(c_int) :: err

      err = jg_date_set_(Y, M, D, CAL_JULIAN, self%year, self%month, self%day)
   end function julian_date_set

   ! implementation
   function julian_day_set_(out, Y, M, D, cal) result(err)
      integer(c_int), value :: Y, M
      real(f32), value :: D
      integer, intent(in) :: cal
      type(julian_day), intent(out) :: out
      integer :: err

      integer(c_int) :: A, B, JD_AUX

      select case (M)
       case (1:2)
         Y = Y - 1
         M = M + 12
       case (3:12)
         ! do nothing
      end select

      ! JM formulae:
      ! A = floor(y / 100)
      ! B = 2 - A + floor(A / 4)
      !
      ! We use integer division here instead, but have to shift values into the non-negative part:
      ! A' = A + 48 = floor((y + 4800) / 100)  > 0
      ! B = 2 - (A' - 48) + floor((A' - 48) / 4) = 38 - A' + floor(A' / 4)
      A = (Y + 4800) / 100
      select case (cal)
       case (CAL_JULIAN)
         B = 0
       case (CAL_GREGORIAN)
         B = 38 - A + (A / 4)
      end select

      ! JD = (1) floor(365.25 * (Y + 4716)) + (2) floor(30.6001 * (M + 1)) + D + B - 1524.5
      ! (1) (Y + 4716 > 0) => 1461 * (Y + 4716) / 4
      ! (2) (JM notes that 30.6 would work with exact math) => 306 * (M + 1) / 10
      JD_AUX = (1461 * (Y + 4716) / 4) + (306 * (M + 1) / 10) + B

      out = julian_day(real(JD_AUX) + D - 1524.5)
      err = JME_OK
   end function julian_day_set_

   function julian_day_get_(jd, Y_out, M_out, D_out) result(err)
      type(julian_day), intent(in) :: jd
      integer(c_int), intent(out) :: Y_out, M_out
      real(f32), intent(out) :: D_out
      integer :: err

      integer :: Z, alpha, A, B, C, D, E
      real :: F

      if (jd%rep .le. 0.0) then
         err = JME_JULIAN_DAY_OUT_OF_RANGE
         return
      end if

      Z = int(floor(jd%rep))
      F = real(jd%rep - real(Z, kind=f64))

      A = Z
      if (Z .ge. 2299161) then
         ! JM formula:
         ! alpha = floor((Z - 1867216.25)/36524.25)
         ! alpha > 0: we multiply both sides of the fraction by 4
         alpha = (4 * Z - 7468865) / 1607067
         A = Z + 1 + alpha - (alpha / 4)
      end if

      B = A + 1524  ! B >= 1524
      C = (40 * B - 48840) / 14610  ! JM: C = floor((B - 122.1)/365.25)  x40
      D = (1461 * C / 4) ! JM: D = floor(365.25 * C)
      E = int(floor((B - D) / 30.6001))

      D_out  = real(B - D - int(30.6001 * E)) + F
      select case (E)
       case (4:13)
         M_out = E-1
         Y_out = C - 4716
       case (14,15)
         M_out=E-13
         Y_out = C - 4715
       case default
         err = JME_INTERNAL
      end select
      err = JME_OK
   end function julian_day_get_

   function jg_date_set_(Y, M, D, cal, Y_out, M_out, D_out) result(err)
      integer, value :: Y, M
      real, value :: D
      integer, value :: cal
      integer(c_int), intent(out) :: Y_out, M_out
      real(f32), intent(out) :: D_out
      integer :: err

      integer :: D_i
      integer :: FEB_delta

      ! Julian Day has Year zero.
      ! Julian/Gregorian Dates have Year -1 immediately preceding Year 1.
      integer, parameter :: CAL_YEAR_MIN = JULIAN_DAY_YEAR_MIN - 1
      integer, parameter :: MONTH_DAYS(1:12) = [&
         31, 28, 31, 30, 31, 30, &
         31, 31, 30, 31, 30, 31]

      select case (Y)
       case (:CAL_YEAR_MIN-1, 0)
         err = JME_YEAR_OUT_OF_RANGE
         return
       case (CAL_YEAR_MIN:-1)
         Y = Y + 1
       case default
         ! do nothing
      end select

      select case (cal)
       case (CAL_GREGORIAN)
         FEB_delta = gregorian_date_is_leap_year_(Y)
       case (CAL_JULIAN)
         FEB_delta = julian_date_is_leap_year_(Y)
      end select

      select case (M)
       case (:0, 13:)
         err = JME_MONTH_OUT_OF_RANGE
         return
       case (1,3:12)
         FEB_delta = 0
      end select

      D_i = INT(FLOOR(D))
      if (.not. (1 .le. D_i .and. D_i .le. MONTH_DAYS(M) + FEB_delta)) then
         err = JME_DAY_OUT_OF_RANGE
         return
      end if

      Y_out = Y
      M_out = M
      D_out = D
      err = JME_OK
   end function jg_date_set_

   pure function julian_date_is_leap_year_(Y) result(L)
      integer, value :: Y
      integer :: L
      L = 0
      if (mod(Y, 4) .eq. 0) L = 1
   end function julian_date_is_leap_year_

   pure function gregorian_date_is_leap_year_(Y) result(L)
      integer, value :: Y
      integer :: L
      L = 0
      if ((mod(Y, 4) .eq. 0) .and. ((mod(Y, 400) .eq. 0) .or. mod(Y, 100) .ne. 0)) L = 1
   end function gregorian_date_is_leap_year_

end module calendar
