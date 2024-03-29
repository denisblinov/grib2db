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

MODULE mod_settings

    USE mod_errors

    IMPLICIT NONE

    INTEGER, PARAMETER :: fNamelist = 30
    INTEGER, PARAMETER :: fieldCountMax = 500
    INTEGER :: fieldCount

    TYPE FieldRec
        CHARACTER(LEN = 3) :: shortName
        REAL :: factor
        REAL :: inc
    ENDTYPE FieldRec

    ! from namelist
    CHARACTER(LEN = 1024) :: filename_grib
    CHARACTER(LEN = 15) :: dBHost    
    INTEGER :: dBUser
    CHARACTER(LEN = 4) :: dBName
    INTEGER :: dBType
    CHARACTER(LEN = 1) :: grid
    CHARACTER(LEN = 1) :: hemisphere 
    INTEGER :: lonCount
    INTEGER :: latCount
    ! Parameters of range for rows
    INTEGER :: P1, P2, P3, P4, P5
    LOGICAL :: addColumn4cyclicLongitude
    TYPE(FieldRec) :: fieldList(fieldCountMax)

CONTAINS

INTEGER FUNCTION loadSettings()

    CHARACTER(LEN = 1024), PARAMETER :: filename_namelist = "grib2db.nl"

    INTEGER :: k

    NAMELIST /grib2db/ &
        filename_grib, &
        dBHost, &
        dBUser, &
        dBName, &
        dBType, &
        grid, &
        hemisphere, &
        lonCount, &
        latCount, &
        fieldList, &
        P1, P2, P3, P4, P5, &
        addColumn4cyclicLongitude

    ! default values
    dBType = 0
    dBHost = '192.168.97.71'
    P1 = 0
    P2 = 0
    P3 = 0
    P4 = 0
    P5 = 0
    addColumn4cyclicLongitude = .FALSE.

    DO k = 1, fieldCountMax
        fieldList(k)%shortName = "---"
    ENDDO 

    PRINT '(">>> Loading settings...")'

    OPEN(fNamelist, file = trim(filename_namelist), iostat = errorCode)
    IF (errorCode /= 0) THEN
        errorMsg = trim(filename_namelist)
        loadSettings = FILE_OPEN_ERROR
        RETURN
    ENDIF
    READ(fNamelist, nml = grib2db, iostat = errorCode)
    IF (errorCode /= 0) THEN
        errorMsg = trim(filename_namelist)
        loadSettings = FILE_READ_ERROR
        RETURN
    ENDIF
    CLOSE(fNamelist)

    PRINT '("filename_grib = ", a)', trim(filename_grib)
    PRINT '("dBHost = ", a)', trim(dBHost)  
    PRINT '("dBUser = ", i0)', dBUser
    PRINT '("dBName = ", a)', trim(dBName)
    PRINT '("dBType = ", i0)', dBType
    PRINT '("grid = ", a)', trim(grid)
    PRINT '("hemisphere = ", a)', trim(hemisphere)
    PRINT '("lonCount = ", i0)', lonCount
    PRINT '("latCount = ", i0)', latCount
    PRINT '("P1 = ", i0)', P1
    PRINT '("P2 = ", i0)', P2
    PRINT '("P3 = ", i0)', P3
    PRINT '("P4 = ", i0)', P4
    PRINT '("P5 = ", i0)', P5
    PRINT '("shortName list:")'
    fieldCount = 0
    DO k = 1, fieldCountMax
        IF (fieldList(k)%shortName == "---") EXIT
        fieldCount = k
        PRINT '("  ", a)', fieldList(k)%shortName
    ENDDO 
    PRINT '("")'

    loadSettings = NO_ERROR
    RETURN
ENDFUNCTION

ENDMODULE mod_settings