
FC := ftn
CC := cc

#FFLAGS := -g
FFLAGS := -O3

MATRICES = matrix.O0 matrix.O1 matrix.O2 matrix.O3

matrix: $(MATRICES)

matrix.O0 : matrix.f90
	$(FC) -O0 -o $@ $<

matrix.O1 : matrix.f90
	$(FC) -O1 -o $@ $<

matrix.O2 : matrix.f90
	$(FC) -O2 -o $@ $<

matrix.O3 : matrix.f90
	$(FC) -O3 -o $@ $<

clean:
	rm -f *.mod *.o $(MATRICES) 

