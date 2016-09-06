#
#  lpSolve Class as frontend for lpSolveAPI
#
#require(methods)

#
#
# lpSolve Class
#
#
sense_legal.l <- c("free", "<=", ">=", "=")
# type_legal.l  <- c("real", "integer", "binary")


#
# lpSolve Class - Generic S4 OOP interface to solver
#
#' \code{lpSolve} Class Definition
#'
#' @slot modelname name for model, type character (optional)
#' @slot modelsense model sense. legal values are \code{min, max} (optional: default max)
#' @slot A model constraints. numeric matrix
#' @slot obj objective function. type numeric. length is 1 or ncols of constraints
#' @slot ub upper bound. type numeric. length is 1 or ncols of constraints (optional:
#'        default value is Inf)
#' @slot lb lower bound. type numeric. length is 1 or ncols of constraints (optional:
#'        default valueis 0)
#' @slot rhs RHS constraints. type numeric. length is 1 or nrowss of constraints
#' @slot sense sense of constraints. legal values are \code{"<=", "=", ">=", "free"}
#'        length is 1 or nrowss of constraints
#'
#' @exportClass lpSolve
#'

#
# TRICK - Warning
# Need to keep a ptr to solver data structure and set durring assignment.
# Becuase of pass by value - can't change when on RHS of assignment.
# The use of the enviorment as part of class is a hack to this problem.
# It can be modified, even when using the obj on right side of an assignment.
#
setClass("lpSolve",
         slots = c(
           modelname = "character",           # optional
           modelsense = "character",          # optional, values: "min" or "Max"

           A    = "matrix",                   # constraint required, 2 dimensions

           obj  = "numeric",                  # required - length must match constraints ncols
           lb   = "numeric",                  # optional
           ub   = "numeric",                  # optional
#            type = "character",                # optional - values: "real", "integer" or "binary"

           rhs  = "numeric",                  # rqeuired - length must match constraints rows
           sense = "character",               # optional, must be legal sense value

           solved = "logical",
           env  = "environment"
         ),
          # Setup default values for optional paramaters
         prototype = list(
           modelsense = "max",
           lb = 0,
           ub = Inf,
#           type = "real",
           sense = "free",
           solved = FALSE
         )
)

#
# Must initialize to new environment. Otherwise every lpSolve object created would
# point to only one lpSolve structure.
#
setMethod("initialize",
          "lpSolve", function(.Object, ...) {
            .Object@env <- new.env(parent=emptyenv())
            .Object <- callNextMethod(.Object, ...)

            .Object
          })


#' @export
validlpSolveObject <- function(object){

  error_msg <- ""
  # Check required vars set

  for (value in c(c("A", "obj", "rhs"))){
    if (length(slot(object, value)) < 1){
      error_msg <- paste0(error_msg, "Required slot ", value, " uninitialized; ")
    }
  }

  # If constraints defined make sure is 2 dimensions
  if (length(object@A) > 0){
    if (!is.numeric(object@A) || length(dim(object@A)) != 2){
      error_msg <- paste0(error_msg, "Slot constraints must be numeric with dimensions = 2; ")
    } else {
      #
      # column setup
      #
      if (is.null(colnames(object@A))){
        object@A
      }

      # Check vars that must match ncols in constraints
      n_col <- ncol(object@A)

      # for (value in c("obj", "lb", "ub", "type")){      # Check that vars are length 0, 1 or ncol
      for (value in c("obj", "lb", "ub")){      # Check that vars are length 0, 1 or ncol
          n <-length(slot(object, value))
        if (n > 0 && n!= 1 && n != n_col){
          error_msg <- paste0(error_msg, "Slot ", value,
                              " length is not = 1 or ncols in constraints; ")
        }
      }

      # Check vars that must match nrows in constraints
      n_row <- nrow(object@A)

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

  # Make not loop
  if (length(object@sense) > 0){
    for (i in 1:length(object@sense)){
      if (! object@sense[i] %in% sense_legal.l){
        error_msg <- paste0(error_msg, "Slot sense contains illegal value:", object@sense[i], "; ")
      }
    }
  }

  # if (length(object@type) > 0){
  #   for (i in 1:length(object@type)){
  #     if (! object@type[i] %in% type_legal.l){
  #       error_msg <- paste0(error_msg, "Slot type contains illegal value:", object@type[i], "; ")
  #     }
  #   }
  # }

  status <- TRUE
  if (nchar(error_msg) > 0){
     status <- error_msg
  }
  return(status)
}

setValidity("lpSolve", validlpSolveObject)

# #' @export
# setGeneric("validObject")


#
# Method Summary
#
#' Summary
#'
#' Summary method lpSolve Object
#'
#' @param object S4 Object to display
#' @aliases summary
#' @export
#'
lpSolveSummary <- function(object){

  cat("lpSolve: ", object@modelname, "\n")
  if (length(dim(object@A)) == 2){
    cat("Linear Solver: ", nrow(object@A), " X ", ncol(object@A), "\n")
  }
}

#' @export
setGeneric("summary")

methods::setMethod("summary", signature("lpSolve"),
  lpSolveSummary
)


#
# Method Show
# use getGeneric("print") to get args list to match
#
#' Show
#'
#' Show method lpSolve Object
#' @param object S4 Object to display
#' @export
#'

lpSolveShow <- function(object){

  cat("lpSolve show: ", object@modelname, "\n")

  digits <- getOption("digits")
  # digits=4

  width  <- digits + 3
  format_b <- "%5.5s"
  format_s <- paste0(" %", width, ".", width,  "s", collapse = "")
  format_g <- paste0(" %", width, ".", digits, "g", collapse = "")

  # Check that A is defined & has two dimensions
  if (is.null(object@A) || length(object@A) < 1 || length(dim(matrix(NA, 2 ,2))) < 2){
    cat("Can't show lpSolve object with undefined constraints - falling back to debug print\n")
    .lpSolveDebug(object)
    return()
  }

  col.n     <- ncol(object@A)
  col.names <- colnames(object@A, do.NULL=FALSE, prefix = "C")

  row.n     <- nrow(object@A)
  row.names <- rownames(object@A, do.NULL=FALSE, prefix = "R")

  # Col Names
  cat(sprintf(format_b, ""), sprintf(format_s, col.names), "\n", sep="")

  # Min/Max & Objective
  obj_str   <- sprintf(format_g, rep_len(object@obj, col.n))
  cat(sprintf(format_b, object@modelsense), obj_str, "\n", sep="")

  # row name, constraint row, sense, rhs
  for (i in 1:row.n){
    cons_str  <- sprintf(format_g, object@A[i,])
    sense_str <- sprintf(" %4s",   rep_len(object@sense, row.n)[i])
    rhs_str   <- sprintf(format_g, rep_len(object@rhs,   row.n)[i])
    cat(sprintf(format_b, row.names[i]), cons_str, sense_str, rhs_str, "\n", sep="")
  }

  # upper & lower bounds & variable type
  ub_str <- sprintf(format_g, rep_len(object@ub, col.n))
  cat(sprintf(format_b, "Upper"), ub_str, "\n", sep="")

  lb_str <- sprintf(format_g, rep_len(object@lb, col.n))
  cat(sprintf(format_b, "Lower"), lb_str, "\n", sep="")

  # type_str <- sprintf(format_s, rep_len(object@type, col.n))
  # cat(sprintf(format_b,"Type"), type_str, "\n", sep="")
}


# #' @export
# setGeneric("show")

methods::setMethod("show", signature("lpSolve"),
  lpSolveShow
)


.lpSolveDebug <- function(object){
  cat("lpSolve debug: ", object@modelname, "\n")
  print(object@A)
  for(slot_name in slotNames(object)){
    temp <- slot(object, slot_name)
    if (length(temp) > 0)
      cat(slot_name, "\t", temp, "\n")
  }
}

