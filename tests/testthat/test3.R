#
# Test legal values
#
#
context("test3: lpSolve show")


test_that("check show", {

  Y <- new("lpSolve")
  Y@constraints <- array(0, c(2,3))
  Y@obj         <- c(1, 2, 3)
  Y@rhs         <- c(1, 2)
  # print(Y)
  expect_output(show(Y), "lpSolve show:")

  # Show for undefined constraints
  Y <- new("lpSolve")
  Y@obj         <- c(1, 2, 3)
  Y@rhs         <- c(1, 2)
  expect_output(show(Y), "lpSolve debug:")


})


context("test3: lpSolve summary")

test_that("check summary", {

  Y <- new("lpSolve")
  Y@constraints <- array(0, c(2,3))
  Y@obj         <- c(1, 2, 3)
  Y@rhs         <- c(1, 2)
  # summary(Y)
  expect_output(summary(Y), "Linear Solver:")
})



