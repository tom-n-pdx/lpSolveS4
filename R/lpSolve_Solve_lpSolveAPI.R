#
# Method solve for lpSolve Class using lpSolveAPI to lp_solve Program
# use getGeneric("print") to get args list to match
#

sense_legal.l <- c("free", "<=", ">=", "=")
type_legal.l <- c("real", "integer", "binary")

#' Solve method for lpSolve Object
#'
#' Using the lpSolveAPI package solves a lpSolve Object
#' @param a lpSolpackage 'methods' is used but not declaredve object to be solved
#'
#' @export
#' @import lpSolveAPI
#' @import methods
#' @aliases solve
#'

lpSolveSolve <- function(a){
  object <- a
  validObject(object)

  # cat("lpSolve: solve\n")
  nrow <- nrow(object@constraints)
  ncol <- ncol(object@constraints)
  lprec <- make.lp(nrow=nrow, ncol=ncol)

  # Set constraints
  if(length(object@constraints) > 0){
    for (i in 1:ncol(object@constraints)){
      set.column(lprec, i, object@constraints[,i])
    }
  }

  # Set all other used slots
  for(slot in c("modelname", "modelsense","obj", "lb", "ub", "sense", "rhs", "type")){
    value <- slot(object, slot)
    if (length(value) > 0){
      switch(slot,
             modelname    = {
               name.lp(lprec, name=value) },
             modelsense   = {
               lp.control(lprec, sense=value) },

             obj = {
               set.objfn(lprec, rep_len(value, ncol)) },
             lb = {
               set.bounds(lprec, lower = rep_len(value, ncol)) },
             ub = {
               set.bounds(lprec, upper = rep_len(value, ncol)) },
             type = {
               # Must set one value at a time
               for(i in 1:ncol){
                 set.type(lprec, i, rep_len(value, ncol)[i])
               }
             },

             sense = {
               set.constr.type(lprec, rep_len(match(value, sense_legal.l) - 1, nrow)) },

             rhs = {
               set.constr.value(lprec, rep_len(value, nrow)) },

             warning("solve dropped thru to dfeault for slot:", slot)
      )
    }
  }

  #
  # Solve
  #
  #print(lprec)
  result            <- list()
  result$status     <- solve(lprec)
  result$variables  <- get.variables(lprec)

  if (result$status != 0){
    # if (debug >= 1) warn("Solver returned non-zero status:", result$status)
    result$variables <- rep_len(NA, ncol)
  }

  return(result)
}

#' @export
setGeneric("solve")
methods::setMethod("solve", signature(a = "lpSolve"),
          lpSolveSolve
)

