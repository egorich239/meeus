module coords
   use iso_c_binding, only: c_int, f32 => c_float, f64 => c_double

   use errors

   implicit none
   private

   type, bind(c), public :: angle
      real(f64) :: rep  ! degrees, usually normalized
   end type angle

   public :: angle_set_signed
   public :: angle_set_unsigned
   public :: angle_set_dms

   public :: angle_sin
   public :: angle_cos

   real(f64), parameter :: RATIO_RADIAN_OVER_DEGREE = 4.D0*DATAN(1.D0) / 180.D0
   real(f64), parameter :: RATIO_DEGREE_OVER_ARCMIN = 1.0_f64 / 60.0_f64
   real(f64), parameter :: RATIO_DEGREE_OVER_ARCSEC = 1.0_f64 / 3600.0_f64

contains
   subroutine angle_set_signed(self)
      type(angle), intent(inout) :: self
      self%rep = modulo(self%rep, 360.0_f64) - 180.0_f64
   end subroutine angle_set_signed

   subroutine angle_set_unsigned(self)
      type(angle), intent(inout) :: self
      self%rep = modulo(self%rep, 360.0_f64)
   end subroutine angle_set_unsigned

   function angle_set_dms(self, sign, deg, min, sec) result(err)
      type(angle), intent(out) :: self
      integer, value :: sign, deg, min
      real(f32), value :: sec
      integer :: err


      ! TODO: boundaries check

      self%rep = real(sign, kind=f64) * ( &
         real(deg, kind=f64) &
         + real(min, kind=f64) * RATIO_DEGREE_OVER_ARCMIN &
         + real(sec, kind=f64) * RATIO_DEGREE_OVER_ARCSEC)
      err = JME_OK
   end function angle_set_dms

   function angle_sin(self, alpha) result(res)
      type(angle), intent(in) :: self
      real(f64), value :: alpha
      real(f64) :: res
      res = sin(alpha * in_rad_(self))
   end function angle_sin

   function angle_cos(self, alpha) result(res)
      type(angle), intent(in) :: self
      real(f64), value :: alpha
      real(f64) :: res
      res = cos(alpha * in_rad_(self))
   end function angle_cos

! implementation
   function in_rad_(self) result(res)
      type(angle), intent(in) :: self
      real(f64) :: res

      type(angle) :: N
      N = self
      call angle_set_unsigned(N)

      res = N%rep * RATIO_RADIAN_OVER_DEGREE
   end function in_rad_

end module coords
