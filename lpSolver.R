#
# Test Solver Class
#

require(methods)

#
#
# lpSolver Class
#
#
sense_legal.l <- c("", "<=", ">=", "=")

validlpSolverObject <- function(object){

  error_msg <- ""
  # Check required vars set

  for (value in c(c("constraints", "obj", "rhs"))){
    if (length(slot(object, value)) < 1){
      error_msg <- paste0(error_msg, "Required slot ", value, " uninitialized; ")
    }
  }

  # If constraints defined make sure is 2 dimensions
  if (length(object@constraints) > 0){
    if (length(dim(object@constraints)) != 2){
      error_msg <- paste0(error_msg, "Slot constraints dimensions != 2; ")
    } else {

      # Check vars that must match ncols in constraints
      n_col <- ncol(object@constraints)

      for (value in c("obj", "lb", "ub")){                 # Check that vars are length 0, 1 or ncol
        n <-length(slot(object, value))
        if (n > 0 && n!= 1 && n != n_col){
          error_msg <- paste0(error_msg, "Slot ", value, " length is not = 1 or ncols in constraints; ")
        }
      }

      # Check vars that must match nrows in constraints
      n_row <- nrow(object@constraints)

      for (value in c("sense", "rhs")){                   # Check that vars are length 0, 1 or ncol
        n <-length(slot(object, value))
        if (n > 0 && n!= 1 && n != n_row){
          error_msg <- paste0(error_msg, "Slot ", value, " length is not = 1 or nrows in constraints; ")
        }
      }
    }
  }

  # Check Character values with limited values
  if (length(object@modelsense) > 0 && !object@modelsense %in% c("min", "max")){
    error_msg <- paste0(error_msg, "modelsense Not min or max; ")
  }

  if (length(object@sense) > 0){
    for (i in 1:length(object@sense)){
      if (! object@sense[i] %in% sense_legal.l){
        error_msg <- paste0(error_msg, "Slot sense contains illegal value:", object@sense[i], "; ")
      }
    }
  }

  status <- TRUE
  if (nchar(error_msg) > 0){
     status <- error_msg
  }
  return(status)
}

setClass("lpSolver",
         slots = c(
           modelname = "character",           # optional
           modelsense = "character",          # optional

           constraints = "matrix",            # required

           obj = "numeric",                   # required - length must match constraints cols
           lb = "numeric",                    # optional
           ub = "numeric",                    # optional

           sense = "character",               # optional = length must match constraints rows
           rhs = "numeric"                    # rqeuired
         )
)

setValidity("lpSolver", validlpSolverObject)

#
# Method Summary
#
lpSolverSummary <- function(object){

  cat("lpSolver: ", object@modelname, "\n")
  if (length(dim(object@constraints)) == 2){
    cat("Linear Solver: ", nrow(object@constraints), " X ", ncol(object@constraints), "\n")
  }
}

#setGeneric("summary")

setMethod("summary", signature(object = "lpSolver"),
  lpSolverSummary
)


#
# Method Print
# use getGeneric("print") to get args list to match
#
lpSolverPrint <- function(x, ...){
  object <- x

  cat("lpSolver: ", object@modelname, "\n")
  print(object@constraints)
  for(slot_name in slotNames(object)){
    temp <- slot(object, slot_name)
    if (length(temp) > 0)
      cat(slot_name, "\t", temp, "\n")
  }

}

setMethod("print", signature(x = "lpSolver"),
  lpSolverPrint
)


#
# Method solve
# use getGeneric("print") to get args list to match
#
lpSolverSolve <- function(a){
  require(lpSolveAPI)
  object <- a
  validObject(object)

  cat("lpSolver: solve\n")
  nrow <- nrow(object@constraints)
  ncol <- ncol(object@constraints)
  lprec <- make.lp(nrow=nrow, ncol=ncol)

  if(length(object@constraints) > 0){
    for (i in 1:ncol(object@constraints)){
      set.column(lprec, i, object@constraints[,i])
    }
  }

  for(slot in c("modelname", "modelsense","obj", "lb", "ub", "sense", "rhs")){
    value <- slot(object, slot)
    if (length(value) > 0){
      switch(slot,
             modelname    = name.lp(lprec, name=value),
             modelsense   = lp.control(lprec, sense=value),

             obj          = set.objfn(lprec, rep_len(value, ncol)),
             lb           = set.bounds(lprec, lower = rep_len(value, ncol)),
             ub           = set.bounds(lprec, upper = rep_len(value, ncol)),

             sense        = set.constr.type(lprec,
                                            rep_len(match(value, sense_legal.l) - 1,
                                                    nrow)),
             rhs          = set.constr.value(lprec, rep_len(value, nrow))
      )
    }
  }

  #
  # Solve
  #
  #print(lprec)
  result <- list()
  result$status     <- solve(lprec)
  result$variables  <- get.variables(lprec)

  return(result)
}

setGeneric("solve")

setMethod("solve", signature(a = "lpSolver"),
          lpSolverSolve
)

# lpq_good <- new("lpSolver",
#                 modelname = "DEA CCR",
#                 constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
#                 rhs     = 7,
#                 sense   = c(">=", ">="),
#                 obj     = c(0, 2)
# )


# lpq_bad <- new("lpSolver")
# lpq_bad@modelname     <- "bad"
# lpq_bad@constraints   <- matrix(c(1,1,0,0,1,1), nrow=2, byrow=T)
# # lpq_bad@obj         <- c(1,1,2)  # Bad - required
# lpq_bad@modelsense    <- "max"
# lpq_bad@rhs           <- c(1)
# lpq_bad@sense         <- c("<=", "xx")  # Bad - should be 1 or 3, xx illegal
# validObject(lpq_bad, test = TRUE)


# lpq_bad1 <- new("lpSolver",
#                 modelname = "bad1",
#                 constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
#                 rhs = c(5, 6),
#                 obj = c(0, 2),
#                 sense = c("XX")
# )

# lpq_bad2 <- new("lpSolver",
#                 modelname = "bad2",
#                 constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
#                 rhs = c(5, 6),
#                 obj = c(0, 2),
#                 modelsense = "XX"
# )
