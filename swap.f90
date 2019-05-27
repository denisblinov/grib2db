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

SUBROUTINE swap(a, b)

    IMPLICIT NONE

    REAL(4), INTENT(INOUT) :: a
    REAL(4), INTENT(INOUT) :: b
    REAL(4) :: temp
    
    temp = a
    a = b
    b = temp
    
    RETURN
ENDSUBROUTINE swap