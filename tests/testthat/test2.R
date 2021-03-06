#
# Test legal values
# To check coverage on validlpSolveObject must call directly
#
context("test2: lpSolve class legal values")


test_that("required slots have been defined", {
  X <- new("lpSolve")

  Y <- X
  Y@A <- array(0, c(2,3))
  Y@obj         <- c(1, 2, 3)
  Y@rhs         <- c(1, 2)
  # expect_equal(validObject(Y, test = TRUE), TRUE)
  expect_equal(validlpSolveObject(Y), TRUE)


  Y <- X
  # Y@A <- array(0, c(2,3))
  Y@obj     <- c(1, 2, 3)
  Y@rhs     <- c(1, 2)

  # check     <- validObject(Y, test = TRUE)
  check     <- validlpSolveObject(Y)
  expect_true(check != TRUE)
  expect_match(check, "*A*")

  Y <- X
  Y@A <- array(0, c(2,3))
  # Y@obj         <- c(1, 2, 3)
  Y@rhs         <- c(1, 2)

  # check     <- validObject(Y, test = TRUE)
  check     <- validlpSolveObject(Y)
  expect_true(check != TRUE)
  expect_match(check, "obj")


  Y <- X
  Y@A <- array(0, c(2,3))
  Y@obj         <- c(1, 2, 3)
  # Y@rhs         <- c(1, 2)

  # check     <- validObject(Y, test = TRUE)
  check     <- validlpSolveObject(Y)
  expect_true(check != TRUE)
  expect_match(check, "rhs")

})


test_that("slots vales are correct", {

  # modelname -
  Y <- new("lpSolve", A = array(0, c(2,3)), obj=1, rhs=1)
  expect_equal(Y@modelname <- "Good", "Good")
  expect_error(Y@modelname <- 2, "character")           # Check bad type caught at assignment

  # modelsense
  Y <- new("lpSolve", A = array(0, c(2,3)), obj=1, rhs=1)
  expect_equal(Y@modelsense <- "min", "min")
  expect_equal(Y@modelsense <- "max", "max")
  expect_error(Y@modelsense <- 2, "character")           # Check bad type caught at assignment
  expect_equal(Y@modelsense <- "bob", "bob")

  # check <- validObject(Y, test = TRUE)
  check <- validlpSolveObject(Y)
  expect_true(check != TRUE)
  expect_match(check, "modelsense")



  # constraints
  Y <- new("lpSolve", A = array(0, c(2,3)), obj=1, rhs=1)
  expect_equal(Y@A <- array(0, c(2,2)),      array(0, c(2,2)))
  expect_error(Y@A <- "b",                    "matrix")

  Y@A <- array("b", c(2, 2))                        # caught at test, not assign
  # check <- validObject(Y, test = TRUE)
  check <- validlpSolveObject(Y)
  expect_true(check != TRUE)
  expect_match(check, "numeric")

  expect_error(Y@A <- 2,  "matrix")                 # single values, not legal
  expect_error(Y@A <- c(2, 2, 2),  "matrix")        # vector not legal
  expect_error(Y@A <- array(2, c(2, 2, 2)),  "matrix")  # 3d not legal



  # check obj, lb, ub (numeric row sized values)

  for(slot in c("obj", "lb", "ub")){
    Y <- new("lpSolve", A = array(0, c(2,3)), obj=1, rhs=1)
    expect_equal(slot(Y, slot) <- 2, 2)
    # expect_equal(validObject(Y, test=TRUE), TRUE)
    expect_equal(validlpSolveObject(Y), TRUE)

    expect_error(slot(Y, slot) <- "b", "numeric")             # Check bad type caught at assignment

    slot(Y, slot) <- c(1, 2, 3)                               # length = ncol in constraint OK
    # expect_equal(validObject(Y, test=TRUE), TRUE)
    expect_equal(validlpSolveObject(Y), TRUE)

    slot(Y, slot) <- c(1, 2)                                  # length != 1 or ncol fail
    # expect_match(validObject(Y, test=TRUE), "ncols")
    expect_match(validlpSolveObject(Y), "ncols")
  }

  # # Check type
  # Y <- new("lpSolve", A = array(0, c(2,3)), obj=1, rhs=1)
  # expect_equal(Y@type <- "real", "real")
  # # expect_equal(validObject(Y), TRUE)
  # expect_equal(validlpSolveObject(Y), TRUE)
  # expect_error(Y@type <- 2, "numeric")                      # Check bad type caught at assignment
  #
  # Y@type  <- c("real", "integer", "binary")                 # length = ncol in constraint OK
  # # expect_equal(validObject(Y), TRUE)                        # Check all three legalo values
  # expect_equal(validlpSolveObject(Y), TRUE)               # Check all three legalo values
  #
  # Y@type  <- c("real", "real")                              # length != 1 or ncol fail
  # # expect_match(validObject(Y, test=TRUE), "ncols")
  # expect_match(validlpSolveObject(Y), "ncols")
  #
  # Y@type  <- "bob"                                          # Character, but illegal value
  # # expect_match(validObject(Y, test=TRUE), "illegal value:bob")  # Not caught until valid check
  # expect_match(validlpSolveObject(Y), "illegal value:bob")  # Not caught until valid check



  # Check rhs
  Y <- new("lpSolve", A = array(0, c(2,3)), obj=1, rhs=1)
  expect_equal(Y@rhs <- 2, 2)
  # expect_equal(validObject(Y, test=TRUE), TRUE)
  expect_equal(validlpSolveObject(Y), TRUE)

  expect_error(Y@rhs <- "b", "numeric")                       # Check bad type caught at assignment

  Y@rhs <- c(1, 2)                                            # length = nrow in constraint OK
  # expect_equal(validObject(Y, test=TRUE), TRUE)
  expect_equal(validlpSolveObject(Y), TRUE)

  Y@rhs <- c(1, 2, 3)                                         # length != 1 or nrow fail
  # expect_match(validObject(Y, test=TRUE), "nrows")
  expect_match(validlpSolveObject(Y), "nrows")


  # Check sense
  Y <- new("lpSolve", A = array(0, c(2,3)), obj=1, rhs=1)
  expect_equal(Y@sense <- "=", "=")
  # expect_equal(validObject(Y, test=TRUE), TRUE)
  expect_equal(validlpSolveObject(Y), TRUE)

  expect_error(Y@sense <- 2, "character")                     # Check bad type caught at assignment

  Y@sense <- c("=", "=")                                      # length = nrow in constraint OK
  # expect_equal(validObject(Y, test=TRUE), TRUE)
  expect_equal(validlpSolveObject(Y), TRUE)

  Y@sense <- c("=", "=", "=")                                 # length != 1 or nrow fail
  # expect_match(validObject(Y, test=TRUE), "nrows")
  expect_match(validlpSolveObject(Y), "nrows")

  # Check legal & illegal sense values
  Y@sense <- c("=")                                           # legal sense value
  # expect_equal(validObject(Y, test=TRUE), TRUE)
  expect_equal(validlpSolveObject(Y), TRUE)

  Y@sense <- c("free")                                        # legal sense value
  # expect_equal(validObject(Y, test=TRUE), TRUE)
  expect_equal(validlpSolveObject(Y), TRUE)

  Y@sense <- c("<=")                                          # legal sense value
  # expect_equal(validObject(Y, test=TRUE), TRUE)
  expect_equal(validlpSolveObject(Y), TRUE)

  Y@sense <- c(">=")                                          # legal sense value
  # expect_equal(validObject(Y, test=TRUE), TRUE)
  expect_equal(validlpSolveObject(Y), TRUE)

  Y@sense <- c("==")                                          # bad sense value
  # expect_match(validObject(Y, test=TRUE), "illegal value")
  expect_match(validlpSolveObject(Y), "illegal value")

  Y@sense <- c("b")                                        # bad sense value
  # expect_match(validObject(Y, test=TRUE), "illegal value")
  expect_match(validlpSolveObject(Y), "illegal value")

})



