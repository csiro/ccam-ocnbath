
ifneq ($(CUSTOM),yes)
FC = ifort
XFLAGS = -qopenmp -xHost -assume byterecl -fp-model precise -traceback
LIBS = -L $(NETCDF_ROOT)/lib -lnetcdf
ifneq ($(NCCLIB),yes)
LIBS += -lnetcdff
endif
INC = -I $(NETCDF_ROOT)/include
PPFLAG90 = -fpp
PPFLAG77 = -fpp
endif

ifeq ($(GFORTRAN),yes)
FC = gfortran
XFLAGS = -O2 -mtune=native -march=native -I $(NETCDF_ROOT)/include
PPFLAG90 = -x f95-cpp-input
PPFLAG77 = -x f77-cpp-input
endif

ifeq ($(SETONIX),yes)
FC = ftn
INC =
LIBS =
XFLAGS = -O2 -mtune=native -march=native -fallow-argument-mismatch -Dncclib
PPFLAG90 = -x f95-cpp-input
PPFLAG77 = -x f77-cpp-input
endif

ifeq ($(CRAY),yes)
FC = ftn
XFLAGS = -h noomp
PPFLAG90 = -eZ
PPFLAG77 = -eZ
DEBUGFLAG =
endif

ifeq ($(NCCLIB),yes)
XFLAGS += -Dncclib
endif


OBJT = ocnbath.o ocnread.o readswitch.o ccinterp.o\
       latltoij_m.o setxyz_m.o xyzinfo_m.o newmpar_m.o \
       indices_m.o parm_m.o precis_m.o ind_m.o jimco_m.o jimcc_m.o \
       jim_utils.o nfft_m.o ncwrite.o ncread.o misc.o netcdf_m.o \
       stacklimit.o

ocnbath:$(OBJT)
	$(FC) $(XFLAGS) $(OBJT) $(LIBS) -o ocnbath

clean:
	rm *.o core *.mod ocnbath
# This section gives the rules for building object modules.

.SUFFIXES:.f90

stacklimit.o: stacklimit.c
	cc -c stacklimit.c
version.h: FORCE
	rm -f brokenver tmpver
	echo "character(len=*), parameter :: version = &" > brokenver
	echo "'OCNBATH '" >> brokenver
	echo "character(len=*), parameter :: version = &" > tmpver
	echo "'OCNBATH `git log | head -3 | tail -1`" "`git log | head -1`'" >> tmpver
	cmp tmpver brokenver || cmp tmpver version.h || mv tmpver version.h
FORCE:

.f90.o:
	$(FC) -c $(XFLAGS) $(INC) $(PPFLAG90) $<
.f.o:
	$(FC) -c $(XFLAGS) $(INC) $(PPFLAG77) $<

# Remove mod rule from Modula 2 so GNU make doesn't get confused
%.o : %.mod

ocnbath.o : ccinterp.o version.h
ocnread.o : ccinterp.o netcdf_m.o
ccinterp.o : ccinterp.f90 setxyz_m.o xyzinfo_m.o latltoij_m.o newmpar_m.o precis_m.o
latltoij_m.o : latltoij_m.f90 xyzinfo_m.o newmpar_m.o precis_m.o
setxyz_m.o : setxyz_m.f90 newmpar_m.o indices_m.o parm_m.o precis_m.o ind_m.o xyzinfo_m.o jimco_m.o jimcc_m.o 
xyzinfo_m.o : xyzinfo_m.f90 precis_m.o
newmpar_m.o : newmpar_m.f90 
precis_m.o : precis_m.f90
indices_m.o : indices_m.f90
parm_m.o : parm_m.f90 precis_m.o 
ind_m.o : ind_m.f90 newmpar_m.o 
jimcc_m.o : jimcc_m.f90 parm_m.o precis_m.o 
jimco_m.o : jimco_m.f90 precis_m.o jim_utils.o nfft_m.o 
jim_utils.o : jim_utils.f90 precis_m.o 
nfft_m.o : nfft_m.f90 precis_m.o 
ncread.o : netcdf_m.o
ncwrite.o : netcdf_m.o
