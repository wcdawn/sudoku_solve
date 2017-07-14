# Makefile

FC = gfortran
FFLAGS = -O2 -fbacktrace -fbounds-check

.SUFFIXES : .f90 .o

default : sudoku_solve.exe

.f90.o :
	$(FC) $(FFLAGS) -c $*.f90

sudoku_solve.exe : sudoku_tools.o sudoku_solve.o
	$(FC) $(FFLAGS) sudoku_tools.o sudoku_solve.o -o $@

clean :
	rm -f core *.o *.mod *.exe

query :
	cvs -n update -P

update : 
	cvs update -P