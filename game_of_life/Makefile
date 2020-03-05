FC := ftn
CC := cc

OPTFLAG := O3
#FFLAGS := -g
FFLAGS := -$(OPTFLAG)

game_of_life.cray: game_of_life
game_of_life.cray: EXEEXT=cray
game_of_life.cray: REPFLAGS=-h keepfiles -rm
#game_of_life.cray: REPFLAGS=
game_of_life.cray: REPEXT=lst

game_of_life.intel: game_of_life
game_of_life.intel: EXEEXT=intel
game_of_life.intel: REPFLAGS=-save-temps -qopt-report=3 -mkl=sequential
#game_of_life.intel: REPFLAGS=
game_of_life.intel: REPEXT=optrpt

game_of_life: game_of_life.f90
	$(FC) $(FFLAGS) $(REPFLAGS) -o $@ $<
	mv $@ $@.$(OPTFLAG).$(EXEEXT)
	mv $(basename $<).$(REPEXT) $(basename $<).$(OPTFLAG).$(REPEXT)

clean:
	rm -f game_of_life *.intel *.cray *.o *.s *lst* *optrpt* *.out

clean.intel:
	rm -f game_of_life *.intel *.o *optrpt*

clean.cray:
	rm -f game_of_life *.cray *.o *.s *lst*
