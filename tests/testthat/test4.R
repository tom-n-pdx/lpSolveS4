#
# Test legal values
#
#
context("lpSolve solve using lpSolveAPI")


test_that("quick check works", {

  lpq_good <- new("lpSolve",
                  # modelname = "DEA CCR",
                  constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
                  rhs     = 7,
                  sense   = c(">=", ">="),
                  obj     = c(0, 2)
  )
  # print(summary(lpq_good))
  # print(lpq_good)
  validObject(lpq_good)
  result <- solve(lpq_good)
  #print(result)
  expect_equivalent(result$variables, c(7, 0))

})


