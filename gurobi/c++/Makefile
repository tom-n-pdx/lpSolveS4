#
# Makefile for C++
# Chunks copied from /Library/gurobi652/mac64/examples/build/Makefile
#

INC      = /Library/gurobi652/mac64/include/
CARGS    = -m64 -g
CC       = gcc
CPP      = g++
CPPLIB   = -L/Library/gurobi652/mac64/lib/ -lgurobi_c++ -lgurobi65

# Generic c++ rule: ‘$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c’
# Generic linker rule: ‘$(CC) $(LDFLAGS) n.o $(LOADLIBES) $(LDLIBS)’
CXX      = g++        # is default
CPPFLAGS = -I/Library/gurobi652/mac64/include/
CXXFLAGS = -m64 -g -L/Library/gurobi652/mac64/lib/ -lgurobi_c++ -lgurobi65

LDLIBS   = -lgurobi_c++ -lgurobi65
LDFLAGS  = -L/Library/gurobi652/mac64/lib/







all: lp_c++ mip2_c++ mip1_c++ diet_c++ facility_c++ \
     workforce1_c++ workforce2_c++ workforce3_c++ workforce4_c++ \
     lpmod_c++ lpmethod_c++ sensitivity_c++ tune_c++ callback_c++ \
     callback_c++

run: run_lp_c++ run_mip2_c++ run_mip1_c++ run_diet_c++ run_facility_c++ \
     run_workforce1_c++ run_workforce2_c++ run_workforce3_c++ run_workforce4_c++ \
     run_lpmod_c++ run_lpmethod_c++ run_sensitivity_c++ run_tune_c++ run_callback_c++ \
     run_callback_c++

# Example 1 - Read a lp model from a file and solve, complain if infesable
lp_c++: lp_c++.cpp
#	$(CPP) $(CARGS) -o lp_c++ lp_c++.cpp -I$(INC) $(CPPLIB) -lm

# Example 2 - Read a mip model from a file and solve
mip2_c++: mip2_c++.cpp
#	$(CPP) $(CARGS) -o mip2_c++ mip2_c++.cpp -I$(INC) $(CPPLIB) -lm

# Example 3 - Internally build a MIP (mixed integer programming) model and solve
mip1_c++: mip1_c++.cpp
#	$(CPP) $(CARGS) -o mip1_c++ mip1_c++.cpp -I$(INC) $(CPPLIB) -lm

# Example application - solves classic diet problem
diet_c++: diet_c++.cpp

# Examnple application - solves facality location problem
facility_c++: facility_c++.cpp

# Example application, Formulates and solves a workforce scheduling model
workforce1_c++: workforce1_c++.cpp

# This example solves the same workforce scheduling model, but if the model is infeasible, it computes an IIS,
# removes one of the associated constraints from the model, and re-solves. 
workforce2_c++: workforce2_c++.cpp

# This example solves the same workforce scheduling model, but if the model is infeasible,
# it adds artificial variables to each constraint and minimizes the sum of the artificial variables.
workforce3_c++: workforce3_c++.cpp

# This example solves the same workforce scheduling model, but it starts with artificial variables in
# each constraint. It first minimizes the sum of the artificial variables. Then, it introduces a
# new quadratic objective to balance the workload among the workers. Demonstrates optimization
# with multiple objective functions.
workforce4_c++: workforce4_c++.cpp

# Demonstrates the use of advanced starts in LP. Reads a continuous model from a file, solves it,
# and then modifies one variable bound. The resulting model is then solved in two different ways:
# starting from the solution of the original model, or restarting from scratch.
lpmod_c++: lpmod_c++.cpp

# Demonstrates the use of different LP algorithms. Reads a continuous model from a file and solves it
# using multiple algorithms, reporting which is the quickest for that model.
lpmethod_c++: lpmethod_c++.cpp


# MIP sensitivity analysis. Reads a MIP model, solves it, and then computes the objective impact
# of fixing each binary variable in the model to 0 or 1. Demonstrates simple MIP model
# modification by changing variable bounds.
sensitivity_c++: sensitivity_c++.cpp


# Uses the parameter tuning tool to search for improved parameter settings for a model. 
tune_c++: tune_c++.cpp


# Demonstrates the use of Gurobi callbacks
callback_c++: callback_c++.cpp




#
# Run commands w/ data files
#

run_lp_c++: lp_c++
#	lp_c++ klein1.mps
	./lp_c++ /Library/gurobi652/mac64/examples/data/coins.lp

run_mip2_c++: mip2_c++
	./mip2_c++ /Library/gurobi652/mac64/examples/data//stein9.mps

run_mip1_c++: mip1_c++
	./mip1_c++

run_diet_c++: diet_c++
	./diet_c++

run_facility_c++: facility_c++
	./facility_c++

run_workforce1_c++: workforce1_c++
	./workforce1_c++

run_workforce2_c++: workforce2_c++
	./workforce2_c++

run_workforce3_c++: workforce3_c++
	./workforce3_c++

run_workforce4_c++: workforce4_c++
	./workforce4_c++

run_lpmod_c++: lpmod_c++
	./lpmod_c++ /Library/gurobi652/mac64/examples/data/afiro.mps

run_lpmethod_c++: lpmethod_c++
	./lpmethod_c++ /Library/gurobi652/mac64/examples/data/afiro.mps

run_sensitivity_c++: sensitivity_c++
	./sensitivity_c++ /Library/gurobi652/mac64/examples/data/p0033.lp

run_tune_c++: tune_c++
	./tune_c++ /Library/gurobi652/mac64/examples/data/p0033.mps

run_callback_c++: callback_c++
	./callback_c++ /Library/gurobi652/mac64/examples/data/p0033.mps

clean:
	rm -rf *.o *_c *_c++ *.class *.log *.rlp *.lp *.bas *.ilp *.dSYM tune.prm
