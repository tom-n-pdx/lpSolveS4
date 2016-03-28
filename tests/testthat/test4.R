#
# Test legal values
#
#
context("test4: lpSolve solve using lpSolveAPI")


test_that("quick check works", {

  # Short minimal test
  lpq_good <- new("lpSolve",
                  # modelname = "DEA CCR",
                  constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
                  rhs     = 7,
                  sense   = c(">=", ">="),
                  obj     = c(0, 2),
                  type    = c("real")
  )
  # print(summary(lpq_good))
  # print(lpq_good)
  validObject(lpq_good)
  result <- solve(lpq_good)
  #print(result)
  expect_equivalent(result$status, 0)
  expect_equivalent(result$variables, c(7, 0))

  # Long tests set all values
  lpq_good <- new("lpSolve",
                  modelname = "DEA CCR",
                  modelsense = "min",
                  constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
                  rhs     = 7,
                  sense   = c(">=", ">="),
                  obj     = c(0, 2),
                  lb      = c(-Inf),
                  ub      = c(20)
  )
  # print(summary(lpq_good))
  # print(lpq_good)
  validObject(lpq_good)
  result <- solve(lpq_good)
  #print(result)
  expect_equivalent(result$variables, c(20, -6.5))


  # Test using binary vars that is infessable
  lpq_good <- new("lpSolve",
                  # modelname = "DEA CCR",
                  constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
                  rhs     = 7,
                  sense   = c(">=", ">="),
                  obj     = c(0, 2),
                  type    = c("binary")
  )
  # print(summary(lpq_good))
  # print(lpq_good)
  validObject(lpq_good)
  result <- solve(lpq_good)
  #print(result)
  expect_equivalent(result$status, 2)
  expect_equivalent(result$variables, c(NA, NA))



})


