SRCDIR=/home/Fei.Liu/NEMS/src
include $(SRCDIR)/conf/configure.nems

MAKEFILE = makefile

UTILINCS = -I/home/Fei.Liu/github/mom/exec/zeus/lib_FMS -I/home/Fei.Liu/github/mom/exec/zeus/lib_ocean/

LIBRARY  = libmom.a

MODULES  = mom_cap.o time_utils.o

MODULES_STUB  = 

DEPEND_FILES = ${MODULES:.o=.F90}

installdir := $(shell date '+%Y-%m-%d-%H-%M-%S')
capgithead := $(shell git show-ref origin/master| cut -f1 -d' ')
mom5githead := $(shell cd ../mom/ && git show-ref origin/master | cut -f1 -d' ' && cd ../nems_mom_cap/)


all default: depend
	@gmake -f $(MAKEFILE) $(LIBRARY)

$(LIBRARY): $(MODULES)
	$(AR) $(ARFLAGS) $@ $?
	sed -e 's/timestr/$(installdir)/g' mom5.mk.template > mom5.mk.install && sed -i -e 's/mom5_github_revision/$(mom5githead)/g' mom5.mk.install && sed -i -e 's/mom5_cap_github_revision/$(capgithead)/g' mom5.mk.install && mkdir /home/Fei.Liu/OCN-INSTALLS/$(installdir) && cp libmom.a mom_cap_mod.mod /home/Fei.Liu/OCN-INSTALLS/$(installdir) && cp mom5.mk.install /home/Fei.Liu/OCN-INSTALLS/$(installdir)/mom5.mk && rm mom5.mk.install
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
