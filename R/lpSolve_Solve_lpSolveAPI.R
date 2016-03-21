#
# Method solve for lpSolver Class using lpSolveAPI to lp_solve Program
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

setMethod("solve", signature(a = "lpSolve"),
          lpSolverSolve
)
