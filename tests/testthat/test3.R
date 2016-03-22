#
# Test legal values
#
#
context("lpSolve print")


test_that("check print", {

  Y <- new("lpSolve")
  Y@constraints <- array(0, c(2,3))
  Y@obj         <- c(1, 2, 3)
  Y@rhs         <- c(1, 2)
  print(Y)

})


context("lpSolve summary")

test_that("check summary", {

  Y <- new("lpSolve")
  Y@constraints <- array(0, c(2,3))
  Y@obj         <- c(1, 2, 3)
  Y@rhs         <- c(1, 2)
  summary(Y)

})



