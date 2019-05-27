# Code by Kopeykin V.V., 2014-2015

SOLUTION = grib2db.ex

GRIB_API = /RHM-GPFS/users/cosmo/dblinov/software/package/grib_api/1.13.1/x86_64-intel
JASPER = /RHM-GPFS/software/ice/local

FC = ifort
FCFLAGS = -i4 -r8 -check bounds -Wl,--trace -extend_source
FLFLAGS = -I$(GRIB_API)/include -I$(JASPER)/include

LIB1 = ~asoihmc/LibRemDB/LibServHMC_Tornado.a
LIB2 = -L$(GRIB_API)/lib -lgrib_api_f77 -lgrib_api_f90 -lgrib_api
LIB3 = -L$(JASPER)/lib -ljasper
LIBS = $(LIB1) $(LIB2) $(LIB3)

SOURCES = $(wildcard *.f90)

MODOBJS = $(patsubst %.f90, %.o, $(filter mod_%.f90, $(SOURCES)))
OBJS = $(patsubst %.f90, %.o, $(filter-out mod_%.f90, $(SOURCES)))

ALL: $(SOLUTION)

$(SOLUTION): $(MODOBJS) $(OBJS)
	$(FC) -o $@ $(FCFLAGS) $(FLFLAGS) $^ $(LIBS)

%.o: %.f90
	$(FC) -c $(FCFLAGS) $(FLFLAGS) $<

clean:
	rm -rf *.o *.mod $(SOLUTION)
