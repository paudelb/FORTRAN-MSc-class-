# $@ target, $< first dependency, $^ all dependencies, 
# $* "stem" of target filename e.g. %.f: %.F will five foo for foo.f
SHELL  = /bin/tcsh -f
FC     = gfortran
FFLAGS = -O2

all: autoc jack boot boot_bin hist

autoc:  getopt.o autoc.o
	$(FC)  $(FFLAGS) $^   -o $@
jack:   getopt.o jack.o
	$(FC)  $(FFLAGS) $^   -o $@
boot:   getopt.o boot.o
	$(FC)  $(FFLAGS) $^   -o $@
boot_bin: getopt.o boot_bin.o
	$(FC)  $(FFLAGS) $^   -o $@
hist:   getopt.o histogram.o
	$(FC)  $(FFLAGS) $^   -o $@

rho_function.mod: autoc.f90
	@true
jack_function.mod: jack.f90
	@true
boot_function.mod: boot.f90
	@true
bootbin_function.mod: boot_bin.f90
	@true
getopt_m.mod: getopt.f90 getopt.o
	@true  #true is a command that does nothing
hist_function.mod: histogram.f90
	@true

clean:
	-/bin/rm *.o *.mod

#########################################
#        RULES                         
#########################################
%.o : %.f90
	$(FC) $(FFLAGS) -c $< -o $@
#########################################

# Standard commands:
# $(FC)  $(FFLAGS) $^   -o $@
# $(CC)  $(CFLAGS) $^   -o $@ -lm
# $(CC)  $(CFLAGS) $@.c -o $@ -lm #if we only have .c file


