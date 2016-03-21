#
# Makefile for lpSolveS4 Package
#
#
PRINT = a2ps -1 --media=Letterdj  --lines-per-page=80 --sides=2 --file-align=sheet -i -G -o-

# Code review - add line numbers: -C
PRINT = a2ps -1 --lines-per-page=80 --tabsize=2 --sides=2 --file-align=sheet -i -C -G -o-

CODE = lpSolve_Class.R  lpSolve_Solve_lpSolveAPI.R

.PHONY: print clean

all:

list:
	$(PRINT) $(CODE) | ps2pdf - > list-code.pdf

clean:
	rm -f list*.pdf



