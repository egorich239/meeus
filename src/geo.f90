module geo
   use iso_c_binding, only: c_int, f32 => c_float, f64 => c_double
   use errors

   implicit none
   private

   type, bind(c), public :: geo_position
      ! NOTE: single-precision floats have enough bits to encode ~1m distance on Earth surface
      real(f32) :: lat  ! degrees, positive West of Greenwich
      real(f32) :: lon  ! degrees, positive North of equator
   end type geo_position

   public :: geo_position_set_civil

contains

   ! Sets from civil lat/lon coordinates: positive N, positive E
   function geo_position_set_civil(self, lat, lon) result(err) bind(c)
      type(geo_position), intent(out) :: self
      real(f32), value :: lat
      real(f32), value :: lon
      integer(c_int) :: err

      if ((.not. (-90.0 .le. lat .and. lat .le. 90.0)) .or. &
         (.not. (-180.0 .le. lon .and. lon .le. 180.0))) then
         err = JME_GEO_POSITION_INVALID
         return
      end if

      self%lat = lat
      self%lon = -lon
      err = JME_OK
   end function geo_position_set_civil


end module geo
