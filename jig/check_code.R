#
# Simple Tests to check if working
#
require("lpSolveS4")

lpq_good <- new("lpSolve",
                modelname = "DEA CCR",
                constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
                rhs     = 7,
                sense   = c(">=", ">="),
                obj     = c(0, 2)
)
rownames(lpq_good@constraints) <- c("R1", "R2")
colnames(lpq_good@constraints) <- c("C1", "C2")

print(summary(lpq_good))
print(lpq_good)
validObject(lpq_good)
result <- solve(lpq_good)
print(result)

# Fails on new
lpq_bad1 <- new("lpSolve",
                modelname = "bad1",
                constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
                rhs = c(5, 6),
                obj = c(0, 2),
                sense = c("XX")
)

# Bad value tests - fail on test since not checked when assign slots
lpq_bad <- lpq_good
print(validObject(lpq_bad, test=TRUE))


lpq_bad <- lpq_good
lpq_bad@modelsense <- "bob"
print(validObject(lpq_bad, test=TRUE))



# lpq_bad <- new("lpSolve")
# lpq_bad@modelname     <- "bad"
# lpq_bad@constraints   <- matrix(c(1,1,0,0,1,1), nrow=2, byrow=T)
# # lpq_bad@obj         <- c(1,1,2)  # Bad - required
# lpq_bad@modelsense    <- "max"
# lpq_bad@rhs           <- c(1)
# lpq_bad@sense         <- c("<=", "xx")  # Bad - should be 1 or 3, xx illegal
# validObject(lpq_bad, test = TRUE)


# lpq_bad1 <- new("lpSolve",
#                 modelname = "bad1",
#                 constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
#                 rhs = c(5, 6),
#                 obj = c(0, 2),
#                 sense = c("XX")
# )

# lpq_bad2 <- new("lpSolve",
#                 modelname = "bad2",
#                 constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
#                 rhs = c(5, 6),
#                 obj = c(0, 2),
#                 modelsense = "XX"
# )


