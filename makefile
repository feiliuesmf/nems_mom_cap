SRCDIR=/home/Fei.Liu/NEMS/src
include $(SRCDIR)/conf/configure.nems

MAKEFILE = makefile

UTILINCS = -I/home/Fei.Liu/github/mom/exec/zeus/lib_FMS -I/home/Fei.Liu/github/mom/exec/zeus/lib_ocean/

LIBRARY  = libmom.a

MODULES  = mom_cap.o time_utils.o

MODULES_STUB  = 

DEPEND_FILES = ${MODULES:.o=.F90}

installdir := $(shell date '+%Y-%m-%d-%H-%M-%S')
githead := $(shell git show-ref origin/master)


all default: depend
	@gmake -f $(MAKEFILE) $(LIBRARY)

$(LIBRARY): $(MODULES)
	$(AR) $(ARFLAGS) $@ $?
	mkdir /home/Fei.Liu/OCN-INSTALLS/$(installdir) && sed -e 's/timestr/$(installdir)/g' mom5.mk.template > mom5.mk.install && sed -e -i 's/mom5_cap_github_revision/$(githead)/g' mom5.mk.install && cp libmom.a mom_cap_mod.mod /home/Fei.Liu/OCN-INSTALLS/$(installdir) && cp mom5.mk.install /home/Fei.Liu/OCN-INSTALLS/$(installdir)/mom5.mk && rm mom5.mk.install
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
