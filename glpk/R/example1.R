#
# Simple GLPK example from glpkAPI doccumentation
#

#  maximize
#    z  =  10 * x1  + 6 * x2    + 4 * x3
#  subject to
#               x1  +     x2  +    x3  ≤ 100
#            10 x1  +   4 x2  +  5 x3  ≤ 600
#             2 x1  +   2 x2  +  6 x3  ≤ 300

library(glpkAPI)

# Empty Problem Object
prob <- initProbGLPK()

# Name Object
setProbNameGLPK(prob, "sample")

# Set Maxamize
setObjDirGLPK(prob, GLP_MAX)

# Add 3 ros and 3 cols
addRowsGLPK(prob, 3)
addColsGLPK(prob, 3)

# Set row / col names
setRowNameGLPK(prob, 1, "p")
setRowNameGLPK(prob, 2, "q")
setRowNameGLPK(prob, 3, "r")

setColNameGLPK(prob, 1, "x1")
setColNameGLPK(prob, 2, "x2")
setColNameGLPK(prob, 3, "x3")

# Set tye type and bounds of rows - by row
# setRowBndGLPK(prob, 1, GLP_UP, 0, 100)
# setRowBndGLPK(prob, 2, GLP_UP, 0, 600)
# setRowBndGLPK(prob, 3, GLP_UP, 0, 300)

# Set the type and bound with function that takes vector
lb    <- c(0, 0, 0)
ub    <- c(100, 600, 300)
type  <- rep(GLP_UP, 3)
setRowsBndsGLPK(prob, 1:3, lb, ub, type)

# Are functions to set colume bounds - setColBnd
# Can set obj function coef at a time - setObjCoefGLPK

# set type & bounds cols, obj function as vectors
lb    <- c(0, 0, 0)
ub    <- lb
type  <- rep(GLP_LO, 3)
obj   <- c(10, 6, 4)
setColsBndsObjCoefsGLPK(prob, 1:3, lb, ub, obj, type)

# Set constraints
ia <- c(1, 1, 1,   2,  3,  2,   3, 2, 3)    # Data is in a set of indexs for i, j & value - non zero
ja <- c(1, 2, 3,   1,  1,  2,   2, 3, 3)
ar <- c(1, 1, 1,  10,  2,  4,   2, 5, 6)
loadMatrixGLPK(prob, 9, ia, ja, ar)

solveSimplexGLPK(prob)

objval <- getObjValGLPK(prob)
print(objval)

# Get Col Values
print(getColsPrimGLPK(prob))

# Get Dual Values
print(getColsDualGLPK(prob))

# Save the solution
printSolGLPK(prob, "sol.txt")

# Write lp
writeLPGLPK(prob, "prob.lp")


# read in problem
lp <- initProbGLPK()
readLPGLPK(lp, "prob.lp")


# Deallocate Memory
delProbGLPK(prob)
delProbGLPK(lp)

# print GLPK Paramaters
# help(glpkConstants)

#
# Setting up GLPK sense and rhs - different
#
# GLP_FR  Inf  < x <  Inf         Free Vars
# GLP_LO   lb <= X <  Inf         Variable with lower bound
# GLP_UP  -Inf < x <= ub          Variable with upper bound
# GLP_DB   lb <= x <= ub          Double-bounded variable
# GLP_FX   lb  = x =  ub          Fixed variable

# free -> GLP_FR, lb  NA     ub NA
# >=   -> GLP_LO, lb <- rhs, ub
# <=   -> GLP_UP, lb  NA,    ub <- rhs
# =    -> GLP_FX, lb rhs,    ub <- RHS
