#
# tests
#

a <- lpSolveR6$new()
class(a)
a
str(a)
a$show()


a <- lpSolveR6$new(3, 5)

# Check for legal values
a <- lpSolveR6$new(-3, 5)
a <- lpSolveR6$new("bob", 5)
a <- lpSolveR6$new( c(1, 3), 5)

a <- lpSolveR6$new( NA, 5)

# Check for legal nrow values
a <- lpSolveR6$new(3, -5)
a <- lpSolveR6$new("bob", 5)
a <- lpSolveR6$new( c(1, 3), 5)

a <- lpSolveR6$new( NA, 5)

a <-lpSolveR6$new(2, 3)
a



# New - make new linear eqn N Vars x N Constraints

# Assign C
a   <-lpSolveR6$new(3, 4)
a$A <-matrix(seq(1, 12), 3, 4)
a


a$show()


a$A <- matrix(c(1,1,0,0,1,1), nrow=2, byrow=T)
a$A[1,2]
a$A[1,1]






# Assign RHS

# Assign Obj


# Assign name

a <- lpSolveR6$new(modelname = "DEA Env Input", modelsense = "max",
                   A=matrix(seq(1, 12), 3, 4),
                   obj=seq(4,1),
                   sense=">=", rhs=seq(-3,-1))

a
str(a)
a$show(digits = 8)

# Print

a   <-lpSolveR6$new(3, 4)
a$modelname  <- "DEA Env Output"
# a$modelsense <- "min"

a$A   <-matrix(seq(1, 12), 3, 4)
# a$obj <- seq(4,1)

# a$rhs <- seq(-3, -1)



a$show()

