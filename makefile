FF = ifort
XFLAGS = -O -assume byterecl -fpp
LIBS = -L $(NETCDF_ROOT)/lib -lnetcdf -lnetcdff
INC = -I $(NETCDF_ROOT)/include


OBJT = ocnbath.o ocnread.o readswitch.o ccinterp.o\
       latltoij_m.o setxyz_m.o xyzinfo_m.o newmpar_m.o \
       indices_m.o parm_m.o precis_m.o ind_m.o jimco_m.o jimcc_m.o \
       jim_utils.o nfft_m.o ncwrite.o ncread.o misc.o netcdf_m.o \
       stacklimit.o

ocnbath:$(OBJT)
	$(FF) $(XFLAGS) $(OBJT) $(LIBS) -o ocnbath

clean:
	rm *.o core *.mod
# This section gives the rules for building object modules.

.SUFFIXES:.f90

stacklimit.o: stacklimit.c
	cc -c stacklimit.c

.f90.o:
	$(FF) -c $(XFLAGS) $(INC) $<
.f.o:
	$(FF) -c $(XFLAGS) $(INC) $<

# Remove mod rule from Modula 2 so GNU make doesn't get confused
%.o : %.mod

ocnbath.o : ccinterp.o
ocnread.o : ccinterp.o
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
