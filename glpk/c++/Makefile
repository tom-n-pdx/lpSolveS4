#
# Makefile for GLPK examples
#

# implict C rules ‘$(CC) $(CPPFLAGS) $(CFLAGS) -c’
# CPPFLAGS = -I/usr/include

# implict link rules ‘$(CC) $(LDFLAGS) n.o $(LOADLIBES) $(LDLIBS)’.
LDLIBS   = -lglpk -lm -lz -lltdl
# LDFLAGS  = -L/usr/lib/x86_64-linux-gnu

#
# Pretty Print Setup
#
#PRINT = a2ps -Pfab5517bw1 -1 --lines-per-page=80 --sides=2 --file-align=sheet -i -G
# a2ps -PEPSON_XP_820_Series --media=Letterdj -1 --lines-per-page=80 --sides=2 --file-align=sheet -i -G

# Code review - add line numbers: -C
# PRINT = a2ps -1 --media=Letterdj --lines-per-page=80 --tabsize=2 --sides=2 --file-align=sheet -i -C -G -o-
PRINT = a2ps -1 --lines-per-page=90 --sides=2 --file-align=sheet -i -G -o-
# EPSON_XP_820_Series

CODE = sample.c sample2.cpp

all: sample sample2

list-code:
	$(PRINT) $(CODE) | ps2pdf - > list-code.pdf


sample: sample.c

sample2: sample2.cpp

clean:
	rm -f sample sample2 list-code.pdf


