#
# Method solve for lpSolve Class using lpSolveAPI to lp_solve Program
# use getGeneric("print") to get args list to match
#
# Add check - if model size different - change size - resize.lp
require(lpSolveAPI)

# ToDo
# * Add solve error codes
# * Add a debug option to print lprec directly

#
# This is ineffecient - always reloads the whole LP eqn. If it was smarter - would only update
# the values that have changed.
#


sense_legal.l <- c("free", "<=", ">=", "=")
# type_legal.l  <- c("real", "integer", "binary")

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

  nrow <- nrow(object@A)
  ncol <- ncol(object@A)

  # Check if the solver object has already been defined
  # If not defined, create a new one and store value in env
  if(is.null(object@env$lprec)){
    lprec <- make.lp(nrow=nrow, ncol=ncol)
    object@env$lprec <- lprec
  } else {
    lprec <- object@env$lprec
  }

  update_slots <- c("modelname", "modelsense", "A", "obj", "lb", "ub", "rhs", "sense")

  # WARNING: workaround for lpSolveAPI bug - must set sense after rhs to have free vars set
  # correctly. Also must set obj after A.
  #
  for(slot in update_slots){
    value <- slot(object, slot)

    if (length(value) < 1)
      next

    switch(slot,
           modelname    = {
             name.lp(lprec, name=value) },
           modelsense   = {
             lp.control(lprec, sense=value) },
           A = {
             for (i in 1:ncol){
               set.column(lprec, i, object@A[,i])
             } },
           obj = {
             set.objfn(lprec, rep_len(value, ncol)) },
           lb = {
             set.bounds(lprec, lower = rep_len(value, ncol)) },
           ub = {
             set.bounds(lprec, upper = rep_len(value, ncol)) },
           # type = {
           #   # Must set one value at a time
           #   for(i in 1:ncol){
           #     set.type(lprec, i, rep_len(value, ncol)[i])
           #   }
           # },
           sense = {
             set.constr.type(lprec, rep_len(match(value, sense_legal.l) - 1, nrow)) },
           rhs = {
             set.constr.value(lprec, rep_len(value, nrow)) },

           warning("solve dropped thru to dfeault for slot:", slot)
    )
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

#
# Return Dual Values from Solved Equation
#
lpSolveDual <- function(object){

  # Check if the solver object has already been defined - should be if getting duals
  if(is.null(object@env$lprec)){
    stop("Solve LP first")
  }

  lprec <- object@env$lprec

  result            <- list()
  result$dual       <- get.dual.solution(lprec)

  return(result)
}

#' @export
setGeneric("getDual",
           function(object)
             standardGeneric("getDual")
)

methods::setMethod("getDual", signature(object = "lpSolve"),
                   definition = lpSolveDual
)

#
# Return Dual Values from Solved Equation
#
lpSolveVariables <- function(object){

  # Check if the solver object has already been defined - should be if getting variables
  if(is.null(object@env$lprec)){
    stop("Solve LP first")
  }

  lprec <- object@env$lprec

  result            <- list()
  result$variables  <- get.variables(lprec)

  return(result)
}

#' @export
setGeneric("getVariables",
           function(object)
             standardGeneric("getVariables")
)

methods::setMethod("getVariables", signature(object = "lpSolve"),
                   definition = lpSolveVariables
)



# #
# # Return Basis
# #
# lpSolveBasis <- function(object){
#
#   # Check if the solver object has already been defined - should be if getting duals
#   if(is.null(object@env$lprec)){
#     stop("Solve LP first")
#   }
#
#   lprec <- object@env$lprec
#
#   result            <- list()
#   result$basis      <- get.basis(lprec)
#
#   return(result)
# }
#
#
# #' @export
# setGeneric("getBasis",
#            function(object)
#              standardGeneric("getBasis")
# )
#
# methods::setMethod("getBasis", signature(object = "lpSolve"),
#                    definition = lpSolveBasis
# )
#

