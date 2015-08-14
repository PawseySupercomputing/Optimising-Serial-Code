
FC := ftn
CC := cc

#FFLAGS := -g
FFLAGS := -O3

game_of_life: game_of_life.f90
	$(FC) $(FFLAGS) -o $@ $<

clean:
	rm -f game_of_life 

