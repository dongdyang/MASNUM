F77 = mpiifort
LF77OPTS = 
FFLAGS   = -xhost -O3

EXE = masnum.wam.mpi

NETCDF_PATH = /usr/local

LIBS = -L$(NETCDF_PATH)/lib -lpnetcdf -L/usr/local/lib 

INC_PATH = -I$(NETCDF_PATH)/include

SRC = \
wamvar_mod.f90 netcdf_mod.f90 time_mod.f90  wamcpl_mod.f90 \
wamfio_mod.f90 wamcor_mod.f90  ympi_mod.f90 wammpi_mod.f90 \
masnum_wam_mpi.f90

INC = \
get_wind.inc  mixture.inc     precom.inc    wamfio_mod_init.inc   \
mixture_limit.inc             mixture_wit.inc                     \
implsch.inc   nlweight.inc    propagat.inc                        \
intact.inc    outmix.inc      readwi.inc                          \
inter.inc     output.inc      setspec.inc                         \
io_rest.inc   outwav.inc      settopog.inc                        \
mean1.inc     out_wavmix.inc  set_uv.inc                          \
mean2.inc     outwav_t.inc    setwave.inc  outmix_wit.inc

QSRC=

VPATH= ../wave_cor/:../ympi_mod/

OBJS = ${SRC:.f90=.o}
QOBJS= ${QSRC:.f90=.o}

.SUFFIXES:
.SUFFIXES: .f90 .o
.f90.o:
	$(F77) $(INC_PATH) $(FFLAGS) -c $<


$(EXE)  :$(INC) $(OBJS)  $(QOBJS)
	$(F77) $(LF77OPTS) -o $@  $(OBJS) $ $(QOBJS) $(LIBS) 

$(OBJS) $(QOBJS) : $(INC)

clean:
	rm -f  $(EXE) *.o *.mod 

cleano:
	rm -f  *.o *.mod


