#PRINT = a2ps -Pfab5517bw1 -1 --lines-per-page=80 --sides=2 --file-align=sheet -i -G
# a2ps -PEPSON_XP_820_Series --media=Letterdj -1 --lines-per-page=80 --sides=2 --file-align=sheet -i -G

# Code review - add line numbers: -C
PRINT = a2ps -1 --media=Letterdj --lines-per-page=80 --tabsize=2 --sides=2 --file-align=sheet -i -C -G -o-
# EPSON_XP_820_Series

CODE = lp.R lp2.R mip.R sos.R 

# Normal
PRINT = a2ps -1 --lines-per-page=80 --tabsize=2 --sides=2 --file-align=sheet -i -G -o-

list-code:
	$(PRINT) $(CODE) | ps2pdf - > list-code.pdf


clean:
	rm -r list-code.pdf
