#
# First TestThat Test
# has lass been installed?

context("Has S4 lpSolve class been installed with methods")

test_that("lpSolve Class installed", {
  x <- new("lpSolve")
  expect_true(is.object(x))
  expect_equal(class(x)[1], "lpSolve")
})



test_that("lpSolve methods installed", {
  # Extract Methods into vector
  mtext   <- showMethods(class="lpSolve", printTo =FALSE )
  fvec    <- gsub( "Function(\\:\\s|\\s\\\")(.+)(\\s\\(|\\\")(.+$)",
                "\\2", mtext[grep("^Function", mtext)] )

  expect_true("summary" %in% fvec)
  expect_true("print" %in% fvec)
  expect_true("solve" %in% fvec)
})


test_that("required slots have been defined", {
  x <- new("lpSolve")

  y <- x
  y@constraints <- array(NA, c(2,3))
  y@obj         <- c(1, 2, 3)
  y@rhs         <- c(1, 2)
  expect_equal(validObject(y, test = TRUE), TRUE)


  y <- x
  # y@constraints <- array(NA, c(2,3))
  y@obj     <- c(1, 2, 3)
  y@rhs     <- c(1, 2)

  check     <- validObject(y, test = TRUE)
  expect_true(check != TRUE)
  expect_match(check, "*constraints*")

  y <- x
  y@constraints <- array(NA, c(2,3))
  # y@obj         <- c(1, 2, 3)
  y@rhs         <- c(1, 2)

  check     <- validObject(y, test = TRUE)
  expect_true(check != TRUE)
  expect_match(check, "obj")


  y <- x
  y@constraints <- array(NA, c(2,3))
  y@obj         <- c(1, 2, 3)
  # y@rhs         <- c(1, 2)

  check     <- validObject(y, test = TRUE)
  expect_true(check != TRUE)
  expect_match(check, "rhs")

})
