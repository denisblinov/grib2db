!-------------------------------------------------------------------------------
!
! Authors:
!
!   Hydrometeorological Research Center of Russia, 2017-2018
!   Vladimir Kopeykin, Denis Blinov
!   phone:  +7(499)795-23-59
!   email: v.v.kopeykin@mail.ru, denisblinov@ya.ru
!
!-------------------------------------------------------------------------------

SUBROUTINE upendField(field)
  
    IMPLICIT NONE
    
    REAL(4), DIMENSION(:, :), INTENT(INOUT) :: field 
    INTEGER lonCount, latCount
    INTEGER lon, lat
    
    lonCount = SIZE(field, 1)
    latCount = SIZE(field, 2)
    
    DO lat = 1, latCount / 2
        DO lon = 1, lonCount
            CALL swap(field(lon, lat), field(lon, latCount - lat + 1))
        ENDDO
    ENDDO

    RETURN
ENDSUBROUTINE upendField