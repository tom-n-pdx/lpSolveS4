#
#  lpSolve Class as frontend for lpSolveAPI
#
require(methods)

#
#
# lpSolve Class
#
#
sense_legal.l <- c("", "<=", ">=", "=")

#
# lpSolve Class - Generic S4 OOP interface to solver
#
#' \code{lpSolve} Class Definition
#'
#' @slot modelname name for model, type character (optional)
#' @slot modelsense model sense. legal values are \code{min, max} (optional: default max)
#' @slot constraints model constraints. numeric matrix
#' @slot obj objective function. type numeric. length is 1 or ncols of constraints
#' @slot ub upper bound. type numeric. length is 1 or ncols of constraints (optional:
#'        default value is Inf)
#' @slot lb lower bound. type numeric. length is 1 or ncols of constraints (optional:
#'        default valueis 0)
#' @slot rhs RHS constraints. type numeric. length is 1 or nrowss of constraints
#' @slot sense sense of constraints. legal values are \code{"<=", "=", ">=", ""} Blank
#'        sets constraint to be free. length is 1 or nrowss of constraints
#'
#' @exportClass lpSolve
#' @import methods

setClass("lpSolve",
         slots = c(
           modelname = "character",           # optional
           modelsense = "character",          # optional, legal values "min" or "Max"

           constraints = "matrix",            # required, 2 dimensions

           obj = "numeric",                   # required - length must match constraints ncols
           lb = "numeric",                    # optional
           ub = "numeric",                    # optional

           rhs = "numeric",                   # rqeuired - length must match constraints rows
           sense = "character"                # optional

         )
)



validlpSolveObject <- function(object){

  error_msg <- ""
  # Check required vars set

  for (value in c(c("constraints", "obj", "rhs"))){
    if (length(slot(object, value)) < 1){
      error_msg <- paste0(error_msg, "Required slot ", value, " uninitialized; ")
    }
  }

  # If constraints defined make sure is 2 dimensions
  if (length(object@constraints) > 0){
    if (!is.numeric(object@constraints) || length(dim(object@constraints)) != 2){
      error_msg <- paste0(error_msg, "Slot constraints must be numeric with dimensions = 2; ")
    } else {

      # Check vars that must match ncols in constraints
      n_col <- ncol(object@constraints)

      for (value in c("obj", "lb", "ub")){              # Check that vars are length 0, 1 or ncol
        n <-length(slot(object, value))
        if (n > 0 && n!= 1 && n != n_col){
          error_msg <- paste0(error_msg, "Slot ", value,
                              " length is not = 1 or ncols in constraints; ")
        }
      }

      # Check vars that must match nrows in constraints
      n_row <- nrow(object@constraints)

      for (value in c("sense", "rhs")){                 # Check that vars are length 0, 1 or ncol
        n <-length(slot(object, value))
        if (n > 0 && n!= 1 && n != n_row){
          error_msg <- paste0(error_msg, "Slot ", value,
                              " length is not = 1 or nrows in constraints; ")
        }
      }
    }
  }

  # Check character slots with only some legal values
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

setValidity("lpSolve", validlpSolveObject)

#
# Method Summary
#
#' Summary
#'
#' Summary method lpSolve Object
#'
#' @export
#'
lpSolveSummary <- function(object){

  cat("lpSolve: ", object@modelname, "\n")
  if (length(dim(object@constraints)) == 2){
    cat("Linear Solver: ", nrow(object@constraints), " X ", ncol(object@constraints), "\n")
  }
}

#' @export
setGeneric("summary")

setMethod("summary", signature("lpSolve"),
  lpSolveSummary
)


#
# Method Print
# use getGeneric("print") to get args list to match
#
#' Print
#'
#' Print method lpSolve Object
#'
#' @export
#'
lpSolvePrint <- function(x, ...){
  object <- x

  cat("lpSolve print: ", object@modelname, "\n")
  print(object@constraints)
  for(slot_name in slotNames(object)){
    temp <- slot(object, slot_name)
    if (length(temp) > 0)
      cat(slot_name, "\t", temp, "\n")
  }

}

#' @export
setGeneric("print")

setMethod("print", signature(x = "lpSolve"),
  lpSolvePrint
)

# source("lpSolve_Solve_lpSolveAPI.R")
