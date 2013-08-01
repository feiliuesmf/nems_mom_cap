SRCDIR=/home/Fei.Liu/noscrub/20130801/NEMS.trunk30111-working/src/
include $(SRCDIR)/conf/configure.nems

MAKEFILE = makefile

UTILINCS = -I/home/Fei.Liu/MOM5/mom-5.0.2/exec/zeus/lib_FMS -I/home/Fei.Liu/MOM5/mom-5.0.2/exec/zeus/lib_ocean/

LIBRARY  = libmom.a

MODULES  = mom_cap.o time_utils.o

MODULES_STUB  = 

DEPEND_FILES = ${MODULES:.o=.F90}


all default: depend
	@gmake -f $(MAKEFILE) $(LIBRARY)

$(LIBRARY): $(MODULES)
	$(AR) $(ARFLAGS) $@ $?
	cp libmom.a mom5.mk mom_cap_mod.mod /home/Fei.Liu/mom5nems
	
$(MODULES): %.o: %.f90
	$(FC) $(FFLAGS) $(UTILINCS) -c $*.f90

$(MODULES_STUB): %.o: %.f90
	$(FC) $(FFLAGS) $(UTILINCS) -c $*.f90

stub: $(MODULES_STUB)
	$(AR) $(ARFLAGS) $(LIBRARY) $(MODULES_STUB)

clean:
	$(RM) -f $(LIBRARY) *.f90 *.o *.mod *.lst depend

MKDEPENDS = $(SRCDIR)/../exe/mkDepends.pl

include $(SRCDIR)/conf/make.rules

include depend
