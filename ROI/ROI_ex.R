#
# Try out ROI
# http://roi.r-forge.r-project.org/examples/lp.html
#

library("ROI")
library("ROI.plugin.glpk")

mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)

# Create Linear Problem
lp <- OP(objective   = c(2, 4, 3),
         constraints = L_constraint(L = mat,
                                    dir = c("<=", "<=", "<="),
                                    rhs = c(60, 40, 80)),
         maximum = TRUE)

# List Solvers
ROI_applicable_solvers(lp)

sol <- ROI_solve(lp, solver = "glpk")
sol

solution(sol)
sol$message

#
# MIP solving
#
mat <- matrix(c(-1,  2,  1,
                 0,  4, -3,
                 1, -3,  2), nrow = 3, byrow=TRUE)


lp <- OP(objective = c(3, 1, 3),
         constraints = L_constraint(L = mat,
                                    dir = c("<=", "<=", "<="),
                                    rhs = c(4, 2, 3)),
         types = c("I", "C", "I"),
         maximum = TRUE)

sol <- ROI_solve(lp)
sol

solution(sol)


#
# Second example
#
mat <- matrix(c(-1,  2,  1,
                0,  4, -3,
                1, -3,  2), nrow = 3, byrow=TRUE)

bounds <- V_bound(li = c(1L, 3L), ui = c(1L, 2L),
                  lb = c(-Inf, 2), ub = c(4, 100))


lp <- OP(objective = c(3, 1, 3),
         constraints = L_constraint(L = mat,
                                    dir = c("<=", "<=", "<="),
                                    rhs = c(4, 2, 3)),
         types = c("I", "C", "I"),
         bounds = bounds,
         maximum = TRUE)

sol <- ROI_solve(lp)
sol

solution(sol)


