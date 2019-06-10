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

MODULE mod_errors

    IMPLICIT NONE 

    INTEGER :: errorCode
    CHARACTER(LEN = 1024) errorMsg
    INTEGER, PARAMETER :: NO_ERROR = 0
    INTEGER, PARAMETER :: ALLOCATE_ERROR = 1
    INTEGER, PARAMETER :: FILE_OPEN_ERROR = 2
    INTEGER, PARAMETER :: FILE_READ_ERROR = 3
    INTEGER, PARAMETER :: FILE_CLOSE_ERROR = 4
    INTEGER, PARAMETER :: FIELD_MISSING_ERROR = 5
    INTEGER, PARAMETER :: HOST_OPEN_ERROR = 6
    INTEGER, PARAMETER :: HOST_CLOSE_ERROR = 7
    INTEGER, PARAMETER :: DB_OPEN_ERROR = 8
    INTEGER, PARAMETER :: DB_CLOSE_ERROR = 9
    INTEGER, PARAMETER :: DB_READ_ERROR = 10
    INTEGER, PARAMETER :: DB_WRITE_ERROR = 11
    INTEGER, PARAMETER :: OTHER_ERROR = 12

CONTAINS

SUBROUTINE printErrorInfo(error)
    INTEGER, INTENT(IN) :: error

    SELECTCASE(error)
        CASE(NO_ERROR)
            PRINT '("No errors!")'
        CASE(ALLOCATE_ERROR)
            PRINT '("Memory allocation error. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(FILE_OPEN_ERROR)
            PRINT '("File open error. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(FILE_READ_ERROR)
            PRINT '("File read error. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(FILE_CLOSE_ERROR)
            PRINT '("File close error. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(FIELD_MISSING_ERROR)
            PRINT '("Field missing warning. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(HOST_OPEN_ERROR)
            PRINT '("Host open error. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(HOST_CLOSE_ERROR)
            PRINT '("Host close error. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(DB_OPEN_ERROR)
            PRINT '("DB open error. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(DB_CLOSE_ERROR)
            PRINT '("DB close error. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(DB_READ_ERROR)
            PRINT '("DB read error. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(DB_WRITE_ERROR)
            PRINT '("DB write error. Msg: ", a, ". Code = ", i0)', trim(errorMsg), errorCode
        CASE(OTHER_ERROR)
            PRINT '("Error. Msg: ", a)', trim(errorMsg)
        CASE DEFAULT
            PRINT '("Unknown error!")'
    ENDSELECT

    RETURN
ENDSUBROUTINE

ENDMODULE mod_errors