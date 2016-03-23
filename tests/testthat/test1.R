#
# First TestThat Test
# has lass been installed?

context("test1: Has S4 lpSolve class been installed with methods")

test_that("lpSolve Class installed", {
  X <- new("lpSolve")
  expect_true(is.object(X))
  expect_equal(class(X)[1], "lpSolve")
})


test_that("lpSolve methods installed", {
  # Extract Methods into vector
  mtext   <- showMethods(class="lpSolve", printTo =FALSE )
  fvec    <- gsub( "Function(\\:\\s|\\s\\\")(.+)(\\s\\(|\\\")(.+$)",
                "\\2", mtext[grep("^Function", mtext)] )

  expect_true("summary" %in% fvec)
  expect_true("show" %in% fvec)
  expect_true("solve" %in% fvec)
})
