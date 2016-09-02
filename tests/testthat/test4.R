#
# Test legal values
#
#
context("test4: lpSolve solve using lpSolveAPI")


test_that("quick check works", {

  # Short minimal test
  lpq_good <- new("lpSolve",
                  # modelname = "DEA CCR",
                  A = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
                  rhs     = 7,
                  sense   = c(">=", ">="),
                  obj     = c(0, 2),
                  type    = c("real")
#                  env     = new.env(parent=emptyenv())
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
                  A = matrix( c(4, 5, 6, 7, 8, 9), nrow=2, byrow=TRUE),
                  rhs     = 7,
                  sense   = c(">=", ">="),
                  obj     = c(0, 2, 3),
                  lb      = c(-Inf),
                  ub      = c(20)
#                  env     = new.env(parent=emptyenv())
  )
  # print(summary(lpq_good))
  # print(lpq_good)
  validObject(lpq_good)
  result <- solve(lpq_good)
  #print(result)
  expect_equal(result$variables, c(20.00000, 20.00000, -28.83333), tolerance = .002)


  #
  # Example from lpSolve Doc Page: http://web.mit.edu/lpsolve/doc/formulate.htm
  #
  lpq_good <- new("lpSolve",
                  modelname = "lpSolve EX-1",
                  modelsense = "max",
                  obj   = c(143, 60),
                  A = matrix(
                    c(120, 210,
                      110,  30,
                        1,   1), nrow=3, byrow=TRUE),
                  rhs   = c(15000, 4000, 75),
                  sense = c("<=")
  )


  validObject(lpq_good)
  result <- solve(lpq_good)
  expect_equal(result$variables, c(21.875, 53.125), tolerance = .002)



  # Test using binary vars that is infessable
  lpq_good <- new("lpSolve",
                  # modelname = "DEA CCR",
                  A = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
                  rhs     = 7,
                  sense   = c(">=", ">="),
                  obj     = c(0, 2),
                  type    = c("binary")
                  # env     = new.env(parent=emptyenv())
  )
  # print(summary(lpq_good))
  # print(lpq_good)
  validObject(lpq_good)
  result <- solve(lpq_good)
  #print(result)
  expect_equivalent(result$status, 2)
  expect_equivalent(result$variables, c(NA, NA))

})


