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

INTEGER FUNCTION gribProcessing(gribFile)

    USE grib_api
    USE mod_errors
    USE mod_settings

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: gribFile
    INTEGER :: write2db
    INTEGER :: gribMsg
    CHARACTER(LEN = 1024) :: shortName
    INTEGER :: ni, nj
    INTEGER :: error
    INTEGER :: i, index, count
    REAL, DIMENSION(lonCount * latCount) :: values
    INTEGER :: date, srok, zabl

    count = 0

    DO WHILE (.true.)

        CALL grib_new_from_file(gribFile, gribMsg, errorCode)
        IF ((errorCode /= GRIB_SUCCESS).AND.(errorCode /= GRIB_END_OF_FILE)) THEN
            CALL grib_close_file(gribFile)
            errorMsg = filename_grib
            gribProcessing = FILE_READ_ERROR
            RETURN
        ENDIF

        IF (errorCode == GRIB_END_OF_FILE) EXIT

        CALL grib_get(gribMsg, 'dataDate', date)
        CALL grib_get(gribMsg, 'dataTime', srok)
        srok = srok / 100
        CALL grib_get(gribMsg, 'forecastTime', zabl)
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

    IF (count /= fieldCount) THEN
        errorCode = count
        errorMsg = "Number of fields in grib-file and namelist does not match!"
        gribProcessing = FIELD_MISSING_ERROR
        RETURN
    ENDIF

    gribProcessing = NO_ERROR
    RETURN

ENDFUNCTION 
