#
# Makefile for MC4LJsystem
#

TARGET = MC4LJsystem

FC   = gfortran

#FLAG = -fbacktrace -ffpe-trap=invalid,zero,overflow -O -Wuninitialized -g -pg -Wall -pedantic -std=f95
FLAG = -O2

SRC = MonteCarlo.f90 pdb.f90 energy.f90 generate_candidate.f90 ini_conf.f90 mtmod.f90 \
	write_outputs.f90 read_inputs.f90 pbs.f90

OBJS =  MonteCarlo.o pdb.o energy.o generate_candidate.o ini_conf.o mtmod.o \
	 write_outputs.o read_inputs.o pbs.o

MOD_FILES = pdb.mod energy.mod generate_candidate.mod ini_conf.mod mtmod.mod \
	 write_outputs.mod read_inputs.mod  pbs.mod

.SUFFIXES: .f90

all:$(TARGET)

${TARGET}:${OBJS}
	$(FC) $(FLAG) -o $@ $(OBJS) ;

MonteCarlo.o: energy.o read_inputs.o write_outputs.o  mtmod.o generate_candidate.o ini_conf.o pbs.o
pdb.o:	
energy.o: pbs.o
pbs.o: 
generate_candidate.o: mtmod.o
ini_conf.o:
mtmod.o: 
read_inputs.o: pdb.o
write_outputs.o: pdb.o

.f90.o:
	$(FC) -c $<;

.PHONY:clean
clean:
	rm $(OBJS) $(MOD_FILES) $(TARGET)
