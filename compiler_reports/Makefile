
FC := ftn
CC := cc

#FFLAGS := -g
#FFLAGS := -O3

matrix.cray: matrix
matrix.cray: EXEEXT=cray
matrix.cray: REPFLAGS=-rm
#matrix.cray: REPFLAGS=
matrix.cray: REPEXT=lst

matrix.intel: matrix
matrix.intel: EXEEXT=intel
matrix.intel: REPFLAGS=-qopt-report=3 -mkl=sequential
#matrix.intel: REPFLAGS=
matrix.intel: REPEXT=optrpt

MATRICES = matrix.O0 matrix.O1 matrix.O2 matrix.O3

matrix: $(MATRICES)

matrix.O0 : matrix.f90
	$(FC) -O0 $(REPFLAGS) -o $@ $<
	mv $@ $@.$(EXEEXT)
	mv $(basename $<).$(REPEXT) $(basename $<).$(REPEXT)$(suffix $@)

matrix.O1 : matrix.f90
	$(FC) -O1 $(REPFLAGS) -o $@ $<
	mv $@ $@.$(EXEEXT)
	mv $(basename $<).$(REPEXT) $(basename $<).$(REPEXT)$(suffix $@)

matrix.O2 : matrix.f90
	$(FC) -O2 $(REPFLAGS) -o $@ $<
	mv $@ $@.$(EXEEXT)
	mv $(basename $<).$(REPEXT) $(basename $<).$(REPEXT)$(suffix $@)

matrix.O3 : matrix.f90
	$(FC) -O3 $(REPFLAGS) -o $@ $<
	mv $@ $@.$(EXEEXT)
	mv $(basename $<).$(REPEXT) $(basename $<).$(REPEXT)$(suffix $@)

clean:
	rm -f *.mod *.o *.O0* *.O1* *.O2* *.O3* *lst* *optrpt*

clean.intel:
	rm -f *.mod *.o *.O0.intel *.O1.intel *.O2.intel *.O3.intel *optrpt*

clean.cray:
	rm -f *.mod *.o *.O0.cray *.O1.cray *.O2.cray *.O3.cray *lst*
