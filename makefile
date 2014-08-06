
SRCDIR=/home/$(USER)/NEMS/src
#installdate=latest
installdate := $(shell date '+%Y-%m-%d-%H-%M-%S')
INSTALLDIR=/home/$(USER)/OCN-INSTALLS/MOM5_$(installdate)
NEMSMOMGITDIR=/home/Fei.Liu/github/mom
NEMSMOMDIR=/home/Fei.Liu/MOM5/mom-5.0.2/exec/zeus

include $(SRCDIR)/conf/configure.nems

PWDDIR := $(shell pwd)
UTILINCS = -I$(NEMSMOMDIR)/lib_FMS -I$(NEMSMOMDIR)/lib_ocean

MAKEFILE = makefile

LIBRARY  = libmom.a

MODULES  = mom_cap.o time_utils.o

MODULES_STUB  = 

DEPEND_FILES = ${MODULES:.o=.F90}

capgitname  := $(shell git remote -v | grep origin | head -1 | cut -f2 | cut -f1 -d " " )
capgithead  := $(shell git show-ref origin/master| cut -f1 -d " ")
momgitname  := $(shell cd $(NEMSMOMGITDIR) && git remote -v | grep origin | head -1 | cut -f2 | cut -f1 -d " "  && cd $(PWDDIR) )
momgithead  := $(shell cd $(NEMSMOMGITDIR) && git show-ref origin/master | cut -f1 -d " " && cd $(PWDDIR) )


all default: depend
	@gmake -f $(MAKEFILE) $(LIBRARY)

$(LIBRARY): $(MODULES)
	$(AR) $(ARFLAGS) $@ $?

	rm -f mom5.mk.install
	@echo "# ESMF self-describing build dependency makefile fragment" > mom5.mk.install
	@echo "# src location Zeus: $pwd" >> mom5.mk.install
	@echo "# MOM github location:  $(momgitname) $(momgithead)" >> mom5.mk.install
	@echo "# MOM CAP github location: $(capgitname) $(capgithead)" >> mom5.mk.install
	@echo  >> mom5.mk.install
	@echo "ESMF_DEP_FRONT     = mom_cap_mod" >> mom5.mk.install
	@echo "ESMF_DEP_INCPATH   = $(INSTALLDIR)" >> mom5.mk.install
	@echo "ESMF_DEP_CMPL_OBJS = " >> mom5.mk.install
	@echo "ESMF_DEP_LINK_OBJS = $(INSTALLDIR)/libmom.a $(NEMSMOMDIR)/lib_ocean/lib_ocean.a $(NEMSMOMDIR)/lib_FMS/lib_FMS.a" >> mom5.mk.install
	mkdir -p $(INSTALLDIR)
	cp -f libmom.a mom_cap_mod.mod $(INSTALLDIR)
	cp -f mom5.mk.install $(INSTALLDIR)/mom5.mk

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
