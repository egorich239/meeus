module solar
   use iso_c_binding, only: c_int, f32 => c_float, f64 => c_double

   use calendar
   use coords
   use errors

   implicit none
   private

   type, bind(c), public :: sun_coord
      type(angle) :: alpha_app  ! apparent right ascention
      type(angle) :: delta_app  ! apparent declination
   end type sun_coord

   public :: sun_coord_set_loprec

contains
   pure function poly_(x, ps) result(r)
      real(f64), value :: x
      real(f64), intent(in) :: ps(:)
      integer :: n, i
      real(f64) :: r

      n = size(ps)
      r = 0.0
      do i = n, 1, -1
         r = ps(i) + r * x
      end do
   end function poly_

   function angle_(sign, deg, min, sec) result(res)
      integer, value :: sign, deg, min
      real(f32), value :: sec
      integer :: err
      type(angle) :: res

      err = angle_set_dms(res, sign, deg, min, sec)
      if (err .ne. JME_OK) error stop 1
   end function angle_

   function sun_coord_set_loprec(self, jd) result(err) bind(c)
      type(sun_coord), intent(out) :: self
      type(julian_day), intent(in) :: jd
      integer(c_int) :: err

      ! Chapter 25. Low accuracy
      real(f64) :: T, L0, M, e, C, Sun, nu, Omega, delta, eps0

      ! NOTE: T measures centuries, its magnitude is <1 in circa 1900-2100.
      ! So the linear member magnitude does not grow beyond 36000.
      ! *If* this method is applicable far beyond 1900-2100 (is it???),
      ! then it might make sense to take into account that L0 is only interesting
      ! up to 360 degrees, and split the linear member into (36000 + a) * (floor(T) + b).
      real(f64), parameter :: L0_poly(3) = [280.46636_f64, 36000.76983_f64, 0.0003032_f64]
      real(f64), parameter :: M_poly(3) = [357.52911_f64, 35999.05029_f64, -0.0001537_f64]
      real(f64), parameter :: e_poly(3) = [0.016708634_f64, -0.000042037_f64, -0.0000001267_f64]
      real(f64), parameter :: C_sinM_poly(3) = [1.914602_f64, -0.004817_f64, -0.0000014_f64]
      real(f64), parameter :: C_sin2M_poly(2) = [0.019993_f64, -0.000101_f64]
      real(f64), parameter :: C_sin3M_alpha = 0.000289_f64
      real(f64), parameter :: Omega_poly(2) = [125.04_f64, -1934.136_f64]

      ! TODO: This whole eps biz should be its own function
      real(f64) :: eps0_poly(4)
      type(angle) :: eps0_tmp
      eps0_tmp = angle_(1, 23, 26, 21.448_f32)
      eps0_poly(1) = eps0_tmp%rep
      eps0_tmp = angle_(-1, 0, 0, 46.8150_f32)
      eps0_poly(2) = eps0_tmp%rep
      eps0_tmp = angle_(-1, 0, 0, 0.00059_f32)
      eps0_poly(3) = eps0_tmp%rep
      eps0_tmp = angle_(1, 0, 0, 0.001813_f32)
      eps0_poly(4) = eps0_tmp%rep

      T = (jd%rep - 2451545.0_f64) / 36525.0_f64
      L0 = poly_(T, L0_poly)
      M = poly_(T, M_poly)
      e = poly_(T, e_poly)
      C = poly_(T, C_sinM_poly) * angle_sin(angle(M), 1.0_f64) &
         + poly_(T, C_sin2M_poly) * angle_sin(angle(M), 2.0_f64) &
         + C_sin3M_alpha * angle_sin(angle(M), 3.0_f64)
      Omega = poly_(T, Omega_poly)
      Sun = L0 + C
      delta = Sun - 0.00569_f64 - 0.00478 * angle_sin(angle(Omega), 1.0_f64)
      nu = M + C

      ! TODO: eps0 does not account for nutation, it should be eps
      eps0 = poly_(T, eps0_poly)

      self%alpha_app = angle(atan2( &
         cos(eps0) * angle_sin(angle(delta), 1.0_f64), &
         angle_cos(angle(delta), 1.0_f64)))
      self%delta_app = angle(asin(sin(eps0) * angle_sin(angle(delta), 1.0_f64)))
      err = JME_OK
   end function sun_coord_set_loprec
end module solar
