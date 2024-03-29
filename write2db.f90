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

INTEGER FUNCTION write2db(values, shortName, date, srok, zabl, factor, inc)

    USE mod_errors
    USE mod_settings

    IMPLICIT NONE

    ! INCLUDE 'upendField.h'

    REAL, DIMENSION(lonCount, latCount), INTENT(IN) :: values
    CHARACTER(LEN = 3), INTENT(IN) :: shortName
    INTEGER, INTENT(IN) :: date, srok, zabl
    REAL, INTENT(IN) :: factor, inc
    ! REAL(4), DIMENSION(lonCount + 1, latCount) :: valuesR4
    REAL(4), DIMENSION(:, :), ALLOCATABLE :: valuesR4
    CHARACTER(8) :: recName
    INTEGER :: i, j
    INTEGER(4) :: recInfo(60)

    IF( addColumn4cyclicLongitude )THEN
        ALLOCATE( valuesR4(lonCount + 1, latCount) )
        ! valuesR4(1 : lonCount, :) = values(:, :)
        ! valuesR4(lonCount + 1, :) = values(1, :)
        ! CALL upendField(valuesR4)
        valuesR4(1 : lonCount, :) = values(:, latCount:1:-1)
        valuesR4(lonCount + 1, :) = values(1, latCount:1:-1)
    ELSE
        ALLOCATE( valuesR4(lonCount, latCount) )
        valuesR4(1 : lonCount, :) = values(:, :)
    END IF

    WRITE(recName, '(a3,a1,i3.3,a1)') shortName, hemisphere, zabl, grid

    ! PRINT '("get record info: ", a,i7.3)', trim(recName), zabl
    CALL getcRemDB(dBName, int4(dbUser), recName, recInfo, errorCode)
    PRINT '("get record info: ", a, ", code = ", i0)', trim(recName), errorCode
    IF (errorCode /= 0) THEN
        errorMsg = dBName
        write2db = DB_READ_ERROR
        RETURN
    ENDIF

    WHERE(valuesR4 == 9999.0)
        valuesR4 = recInfo(30) + 1 ! Empty const = max + 1
    ELSEWHERE
        valuesR4 = factor * valuesR4 + inc
    ENDWHERE

    PRINT '("Write field to DB: shortname = ", a, ", date = ", i8.8, ", srok = ", i2.2, ", zabl = ", i3.3, &
        ", minVal = ", f10.2, ", maxVal = ", f10.2)',  trim(shortName), date, srok, zabl, minval(valuesR4), maxval(valuesR4)

    IF (dBType == 0) THEN
        CALL wrfcRemDB(dBName, int4(dbUser), int4(date), recName, int4(srok), valuesR4, errorCode)
    ELSE IF(dBType == 1)THEN
        CALL wrfqRemDB(dBName, int4(dbUser), recName, int4(srok), valuesR4, errorCode)
    ELSE IF(dBType == 2)THEN
        CALL WrfcrRemDB(dBName, int4(dbUser), int4(date), recName, int4(srok), P1, P2, P3, P4, P5, valuesR4, errorCode)
        ! CALL Wrfcr(dBName, int4(dbUser), int4(date), recName, int4(srok), P1, P2, P3, P4, P5, valuesR4, errorCode)
    ELSE IF(dBType == 3)THEN
        CALL WrfqrRemDB(dBName, int4(dbUser), recName, int4(srok), P1, P2, P3, P4, P5, valuesR4, errorCode)
    ELSE IF(dBType == 9999)THEN ! debug_outputField
        OPEN(11,FILE='debug_outputField.txt',status='UNKNOWN',POSITION='APPEND')
        WRITE(11, '("date,srok,P1,recName: ",i8.8, 2i4.2, a10)') date, srok, P1, recName
        WRITE(11, '(f12.4)') valuesR4
        CLOSE(11)
    ENDIF

    DEALLOCATE( valuesR4 )
    IF (errorCode /= 0) THEN
        errorMsg = dBName
        write2db = DB_WRITE_ERROR
        RETURN
    ENDIF

    write2db = NO_ERROR

ENDFUNCTION 