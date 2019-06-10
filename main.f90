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

PROGRAM grib2db

    USE grib_api
    USE mod_errors
    USE mod_settings

    IMPLICIT NONE

    INTEGER :: error
    INTEGER :: gribProcessing

    PRINT '("")'
    PRINT '(">>> grib2db start!")'
    PRINT '("")'

    ! Read settings
    error = loadSettings()
    IF (error /= NO_ERROR) THEN
        CALL printErrorInfo(error)
        CALL exit(error)
    ENDIF

    ! Open host and DB
    CALL openRemHost(dBHost, errorCode)
    PRINT '("OPEN Remote Host <", a, ">, code = ", i0)', trim(dBHost), errorCode
    IF (errorCode /= 0) THEN
        error = HOST_OPEN_ERROR
        errorMsg = trim(dBHost)
        CALL printErrorInfo(error)
        CALL exit(error)
    ENDIF

    CALL openRemDB(int4(dBUser), dBName, int4(1), errorCode)
    PRINT '("OPEN Remote DB <", a, ">, code = ", i0)', trim(dBName), errorCode
    IF (errorCode /= 0) THEN
        error = DB_OPEN_ERROR
        errorMsg = trim(dBName)
        CALL printErrorInfo(error)
        CALL exit(error)
    ENDIF

    ! Copying fields from grib-file to DB
    error = gribProcessing()
    PRINT '("gribProcessing, code = ", i0)', errorCode
    IF (error /= NO_ERROR) THEN
        CALL printErrorInfo(error)
        CALL exit(error)
    ENDIF

    ! Close host
    CALL closeRemHost(dBHost, errorCode)  
    IF (errorCode /= 0) THEN
        error = HOST_CLOSE_ERROR
        errorMsg = trim(dBHost)
        CALL printErrorInfo(error)
        CALL exit(error)
    ENDIF

    PRINT '("")'
    PRINT '(">>> grib2db completed successfully!")'
    PRINT '("")'

    CALL exit(NO_ERROR)
ENDPROGRAM
