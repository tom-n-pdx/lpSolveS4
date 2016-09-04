#
# Method solve for lpSolve Class using glpkAPI to lp_solve Program
# use getGeneric("print") to get args list to match
#
#
require(glpkAPI)

# ToDo
# * add names for rows & cols


sense_legal.l <- c("free",  "<=",   ">=",   "=")
sense_glpk.l  <- c(GLP_FR,  GLP_UP, GLP_LO, GLP_FX)

type_legal.l  <- c("real", "integer", "binary")
type_glpk.l   <- c(GLP_CV,  GLP_IV,   GLP_IV)

#' Solve method for lpSolve Object
#'
#' Using the lpSolveAPI package solves a lpSolve Object
#' @param a lpSolpackage 'methods' is used but not declaredve object to be solved
#'
#' @export
#' @import glpkAPI
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
  # Create prob & setup cols & rows
  if(is.null(object@env$prob)){
    prob <- initProbGLPK()
    object@env$prob <- prob
    addRowsGLPK(prob, nrow)
    addColsGLPK(prob, ncol)
  } else {
    prob <- object@env$prob
  }

  # update_slots <- c("modelname", "modelsense", "A", "obj", "lb", "ub", "rhs", "type", "sense")
  update_slots <- c("modelname", "modelsense", "obj", "rhs", "ub", "A", "type")

  for(slot in update_slots){
    value <- slot(object, slot)

    if (length(value) < 1)
      next

    switch(slot,
           modelname    = {
             setProbNameGLPK(prob, value)  },
           modelsense   = {
             value_glpk <- ifelse(value == "max", GLP_MAX, GLP_MIN)
             setObjDirGLPK(prob, value_glpk) },

          A = {
             for (i in 1:ncol){
               setMatColGLPK(prob, i, nrow, c(1:nrow), object@A[,i])
             } },

          obj = {
             setObjCoefsGLPK(prob, c(1:ncol), object@obj)  },


          rhs = {
            ub    <- rep_len(object@rhs,  nrow)
            lb    <- rep_len(object@rhs,  nrow)

            sense.i <- match(rep_len(object@sense, nrow), sense_legal.l)
            sense_glpk <- sense_glpk.l[sense.i]
            setRowsBndsGLPK(prob, c(1:nrow), lb, ub, sense_glpk) },

          ub = {
            ub      <- rep_len(object@ub, ncol)
            lb      <- rep_len(object@lb, ncol)
            # type.i  <- match(rep_len(object@type, ncol), type_legal.l)
            # type_glpk <- type_glpk.l[type.i]
            cat("lb:", lb, "ub:", ub, "1:ncol", c(1:ncol), "\n")

            setColsBndsGLPK(prob, c(1:ncol), lb, ub, type=NULL)

            for(i in c(1:ncol)){
              cat("i: ", i, "ub:", getColsUppBndsGLPK(prob, i), "\n")
            }  },

          type = {
            type.i  <- match(rep_len(value, ncol), type_legal.l)
            type_glpk <- type_glpk.l[type.i]

            setColsKindGLPK(prob, c(1:ncol), type_glpk) },


            warning("solve dropped thru to dfeault for slot:", slot)
    )
  }
  #
  # #
  # # Solve
  # # Must solve for simplex and then solve for integer...
  # #print(lprec)
  result            <- list()
  result$status     <- solveSimplexGLPK(prob)
  result$status     <- solveMIPGLPK(prob)

  result$variables  <- getColsPrimGLPK(prob)
  result$variables  <- mipColsValGLPK(lpq_good@env$prob)
  #
  # if (result$status != 0){
  #   # if (debug >= 1) warn("Solver returned non-zero status:", result$status)
  #   result$variables <- rep_len(NA, ncol)
  # }

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



#
# Return Basis
#
lpSolveBasis <- function(object){

  # Check if the solver object has already been defined - should be if getting duals
  if(is.null(object@env$lprec)){
    stop("Solve LP first")
  }

  lprec <- object@env$lprec

  result            <- list()
  result$basis      <- get.basis(lprec)

  return(result)
}


#' @export
setGeneric("getBasis",
           function(object)
             standardGeneric("getBasis")
)

methods::setMethod("getBasis", signature(object = "lpSolve"),
                   definition = lpSolveBasis
)


