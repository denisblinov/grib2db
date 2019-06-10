!-------------------------------------------------------------------------------
!
! Authors:
!
!   Hydrometeorological Research Center of Russia, 2017-2019
!   Vladimir Kopeykin, Denis Blinov
!   phone:  +7(499)795-23-59
!   email: v.v.kopeykin@mail.ru, denisblinov@ya.ru
!
!-------------------------------------------------------------------------------

INTEGER FUNCTION gribProcessing()

    USE grib_api
    USE mod_errors
    USE mod_settings

    IMPLICIT NONE

    INTEGER :: gribFile
    INTEGER :: write2db
    INTEGER :: gribMsg
    CHARACTER(LEN = 1024) :: shortName
    INTEGER :: ni, nj
    INTEGER :: error
    INTEGER :: i, index, count
    REAL, DIMENSION(lonCount * latCount) :: values
    INTEGER :: date, srok, zabl
    INTEGER :: editionNumber, indicatorOfUnitOfTimeRange

    count = 0

    ! Open grib-file
    OPEN(gribFile, file = trim(filename_grib), status = 'old', iostat = errorCode)
    IF (errorCode /= 0) THEN
        error = FILE_OPEN_ERROR
        errorMsg = trim(filename_grib)
        CALL printErrorInfo(error)
        ! CALL exit(error)
        gribProcessing = error
        RETURN
    ENDIF
    CLOSE(gribFile)

    CALL grib_open_file(gribFile, filename_grib, 'r', errorCode)
    PRINT '("OPEN grib-file <", a, ">, code = ", i0)', trim(filename_grib), errorCode
    IF (errorCode /= GRIB_SUCCESS) THEN
        error = FILE_OPEN_ERROR
        errorMsg = trim(filename_grib)
        CALL printErrorInfo(error)
        gribProcessing = error
        RETURN
    ENDIF

    DO WHILE (.true.)

        CALL grib_new_from_file(gribFile, gribMsg, errorCode)
        IF ((errorCode /= GRIB_SUCCESS).AND.(errorCode /= GRIB_END_OF_FILE)) THEN
            CALL grib_close_file(gribFile)
            errorMsg = filename_grib
            gribProcessing = FILE_READ_ERROR
            RETURN
        ENDIF

        IF (errorCode == GRIB_END_OF_FILE) EXIT

        CALL grib_get(gribMsg, 'editionNumber', editionNumber)
        CALL grib_get(gribMsg, 'dataDate', date)
        CALL grib_get(gribMsg, 'dataTime', srok)
        srok = srok / 100
        IF( editionNumber == 2 )THEN
            CALL grib_get(gribMsg, 'indicatorOfUnitOfTimeRange', indicatorOfUnitOfTimeRange)
            CALL grib_get(gribMsg, 'forecastTime', zabl)
            IF( indicatorOfUnitOfTimeRange == 0 )THEN ! minute
                zabl = zabl/60
            END IF
        ELSE ! edition 1
            CALL grib_get(gribMsg, 'endStep', zabl)
        END IF

        CALL grib_get(gribMsg, 'shortName', shortName)
        CALL grib_get(gribMsg, 'Ni', ni)
        CALL grib_get(gribMsg, 'Nj', nj)

        index = -1
        DO i = 1, fieldCount
            IF (trim(shortName) == trim(fieldList(i)%shortName)) THEN
                index = i
                EXIT
            ENDIF
        ENDDO

        IF (index /= -1) THEN

            CALL grib_get(gribMsg, 'values', values, errorCode)
            IF (errorCode /= GRIB_SUCCESS) THEN
                errorMsg = filename_grib
                gribProcessing = FILE_READ_ERROR
                RETURN
            ENDIF
            error = write2db(values, fieldList(index), date, srok, zabl, fieldList(index)%factor, fieldList(index)%inc)
            IF (error /= NO_ERROR) THEN
                gribProcessing = error
                RETURN
            ENDIF    

            count = count + 1
        ELSE
            PRINT '("WARNING: unnecessary field in grib-file. Short name = ", a)', trim(shortName)
        ENDIF

        CALL grib_release(gribMsg)

    ENDDO

    ! IF (count /= fieldCount) THEN
        ! errorCode = count
        ! errorMsg = "Number of fields in grib-file and namelist does not match!"
        ! gribProcessing = FIELD_MISSING_ERROR
        ! RETURN
    ! ENDIF

    ! Close grib-file
    CALL grib_close_file(gribFile)

    gribProcessing = NO_ERROR
    RETURN

ENDFUNCTION 
