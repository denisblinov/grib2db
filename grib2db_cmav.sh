#!/bin/bash -x
#-------------------------------------------------------------------------------
#
# Authors:
#
#   Hydrometeorological Research Center of Russia, 2017-2018
#   Vladimir Kopeykin, Denis Blinov
#   phone:  +7(499)795-23-59
#   email: v.v.kopeykin@mail.ru, denisblinov@ya.ru
#
#-------------------------------------------------------------------------------

source ~/.profile 2>/dev/null
source ~/.bashrc  2>/dev/null
source ${COSMO_DIR:-~opercosmo/COSMO}/template/func4cosmo.sh 2>/dev/null

# export GRIB_DEFINITION_PATH=~vkopeikin/grib2db_project/def
GRIB_DEFINITION_PATH_vendor=~dblinov/software/package/grib_api/1.13.1/x86_64-intel/share/grib_api/definitions
GRIB_DEFINITION_PATH_rums=~dblinov/postpr/delivery/database/EN05/definitions/1.13.1

export GRIB_DEFINITION_PATH=$GRIB_DEFINITION_PATH_vendor:$GRIB_DEFINITION_PATH_rums

  if [ ${#1} -le 2 ];then
    DATE=$(date -u +%Y%m%d)${1:-00}
  elif [ ${#1} -eq 10 ]; then
    DATE=${1}
  fi
  HH=${DATE:8:2}

echo $HH

CATALOG 2>/dev/null

function print_surf_list {
    cat >grib2db.nl <<MARKER
&grib2db
    filename_grib = "$filename_s"
    dBHost = "10.1.11.97"
    dBUser = 970101
    dBName = "CMAV"
    dBType = 2
    grid = "B"
    hemisphere = "R"
    lonCount = 420
    latCount = 470
    P1       = $min
    addColumn4cyclicLongitude = .FALSE.
    fieldList = "P98", 0.01, 0.0,
                "T99", 1.0, -273.15,
                "Q99", 1.0, -273.15,
/
MARKER
}

function print_plev_list {
    cat >grib2db.nl <<MARKER
&grib2db
    filename_grib = "$filename_p"
    dBHost = "10.1.11.97"
    dBUser = 970101
    dBName = "CMAV"
    dBType = 2
    grid = "B"
    hemisphere = "R"
    lonCount = 420
    latCount = 470
    P1       = $min
    addColumn4cyclicLongitude = .FALSE.
    fieldList = "H00", 0.102, 0.0,
                "H92", 0.102, 0.0,
                "H85", 0.102, 0.0,
                "H70", 0.102, 0.0,
                "H50", 0.102, 0.0,
                "H40", 0.102, 0.0,
                "H30", 0.102, 0.0,
                "H25", 0.102, 0.0,
                "H20", 0.102, 0.0,
                "H15", 0.102, 0.0,
                "H10", 0.102, 0.0,
                "H07", 0.102, 0.0,
                "H05", 0.102, 0.0,
                "T00", 1.0, -273.15,
                "T92", 1.0, -273.15,
                "T85", 1.0, -273.15,
                "T70", 1.0, -273.15,
                "T50", 1.0, -273.15,
                "T40", 1.0, -273.15,
                "T30", 1.0, -273.15,
                "T25", 1.0, -273.15,
                "T20", 1.0, -273.15,
                "T15", 1.0, -273.15,
                "T10", 1.0, -273.15,
                "T07", 1.0, -273.15,
                "T05", 1.0, -273.15,
                "U00", 1.0, 0.0,
                "U92", 1.0, 0.0,
                "U85", 1.0, 0.0,
                "U70", 1.0, 0.0,
                "U50", 1.0, 0.0,
                "U40", 1.0, 0.0,
                "U30", 1.0, 0.0,
                "U25", 1.0, 0.0,
                "U20", 1.0, 0.0,
                "U15", 1.0, 0.0,
                "U10", 1.0, 0.0,
                "U07", 1.0, 0.0,
                "U05", 1.0, 0.0,
                "V00", 1.0, 0.0,
                "V92", 1.0, 0.0,
                "V85", 1.0, 0.0,
                "V70", 1.0, 0.0,
                "V50", 1.0, 0.0,
                "V40", 1.0, 0.0,
                "V30", 1.0, 0.0,
                "V25", 1.0, 0.0,
                "V20", 1.0, 0.0,
                "V15", 1.0, 0.0,
                "V10", 1.0, 0.0,
                "V07", 1.0, 0.0,
                "V05", 1.0, 0.0,
                "R00", 1.0, 0.0,
                "R92", 1.0, 0.0,
                "R85", 1.0, 0.0,
                "R70", 1.0, 0.0,
                "R50", 1.0, 0.0,
                "R40", 1.0, 0.0,
                "R30", 1.0, 0.0,
                "R25", 1.0, 0.0,
                "R20", 1.0, 0.0,
                "R15", 1.0, 0.0,
                "R10", 1.0, 0.0,
                "R07", 1.0, 0.0,
                "R05", 1.0, 0.0,
/
MARKER
}


zablList="000 004 005 006 007 008 009 010"

DIR=/RHM-GPFS/users/spectr/vkopeikin/grib2db_project
# DIR_DATA=~grivin/postpr/interp/4baseEN05/results
DIR_DATA=$OUT_CFO02_oc/CM_out
logdir=~/postpr/delivery/database/CMAV/logs/$HH

cd $logdir

for zabl in ${zablList} #=== Process for the current zabl ======================
do
  for ((min=0;min<=50;min=min+10));do
    [ $min -gt 0 ] && [ "$zabl" == "000" -o "$zabl" == "010" ] && continue

    minute=$(printf %02d $min)
    filename_s=${DIR_DATA}/lfff0${zabl}${minute}00_4dbCMAV
    echo 'process grib-file = '${filename_s}

    print_surf_list
    grib2db.1.4.exe
    # Control of the return code
    if [ $? -ne 0 ]; then
        : exit 1
    fi

    filename_p="${DIR_DATA}/lfff0${zabl}${minute}00p_4dbCMAV"
    echo 'process grib-file = '${filename_p}

    print_plev_list
    grib2db.1.4.exe


    # Control of the return code
    if [ $? -ne 0 ]; then
        : exit 1
    fi
  done
done #==========================================================================

# exit 0
