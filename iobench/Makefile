
FC := ftn
CC := cc

#FFLAGS := -g
FFLAGS := -O3


iobench: iobench.f90
	$(FC) $(FFLAGS) -o $@ $<

iobench_hdf5: iobench_hdf5.f90
	$(FC) $(FFLAGS) -o $@ $<

clean:
	rm -f *.mod *.o iobench_hdf5 iobench

