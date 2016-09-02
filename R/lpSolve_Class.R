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
type_legal.l  <- c("real", "integer", "binary")


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
           type = "character",                # optional - values: "real", "integer" or "binary"

           rhs  = "numeric",                  # rqeuired - length must match constraints rows
           sense = "character",               # optional, must be legal sense value

           env  = "environment"
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

      for (value in c("obj", "lb", "ub", "type")){      # Check that vars are length 0, 1 or ncol
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

  if (length(object@sense) > 0){
    for (i in 1:length(object@sense)){
      if (! object@sense[i] %in% sense_legal.l){
        error_msg <- paste0(error_msg, "Slot sense contains illegal value:", object@sense[i], "; ")
      }
    }
  }

  if (length(object@type) > 0){
    for (i in 1:length(object@type)){
      if (! object@type[i] %in% type_legal.l){
        error_msg <- paste0(error_msg, "Slot type contains illegal value:", object@type[i], "; ")
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

  width  <- digits + 3
  format_s <- paste0(" %", width, ".", width,  "s", collapse = "")
  format_g <- paste0(" %", width, ".", digits, "g", collapse = "")

  # Get Size
  if (length(object@A) > 0){
    col.n     <- ncol(object@A)
    col.names <- colnames(object@A, do.NULL=FALSE, prefix = "C")
    row.n     <- nrow(object@A)
    row.names <- rownames(object@A, do.NULL=FALSE, prefix = "R")
    temp_cons <- object@A
  } else {
    cat("Can't show lpSolve object with undefined constraints - falling back to debug print\n")
    .lpSolveDebug(object)
    return()
  }

  # Col Names
  # cat("     ", paste0(sprintf("%5.5s ", col.names), collapse=""), "\n")
  cat("    ", paste0(sprintf(format_s, col.names), collapse=""), "\n")

  # Min/Max & Objective
  sense_str <- sprintf("%5.5s", ifelse(length(object@modelsense) > 0,   object@modelsense, "max"))
  # obj_str   <- paste0(sprintf(" %5.3g", rep_len(object@obj, col.n)), collapse="")
  obj_str   <- paste0(sprintf(format_g, rep_len(object@obj, col.n)), collapse="")
  cat(paste0(sense_str, obj_str, collapse=""), "\n")

  if (length(object@sense) == 0)
    object@sense <- c("free")

  # row name, constraint row, sense, rhs
  for (i in 1:row.n){
    name_str  <- sprintf("%5.5s", row.names[i])
    # cons_str  <- paste0(sprintf(" %5.3g", rep_len(temp_cons[i,], col.n)), collapse="")
    cons_str  <- paste0(sprintf(format_g, rep_len(temp_cons[i,], col.n)), collapse="")
    sense_str <- sprintf(" %4s", rep_len(object@sense, row.n)[i])
    # rhs_str   <- sprintf(" %5.3g", rep_len(object@rhs,   row.n)[i])
    rhs_str   <- sprintf(format_g, rep_len(object@rhs,   row.n)[i])
    cat(paste0(name_str, cons_str, sense_str, rhs_str, collapse=""), "\n")
  }

  # upper & lower bounds
  if (length(object@ub) == 0)
    object@ub <- Inf
  # ub_str <- paste0( sprintf(" %5.3g", rep_len(object@ub, col.n)), collapse="")
  ub_str <- paste0( sprintf(format_g, rep_len(object@ub, col.n)), collapse="")
  cat(paste0("Upper", ub_str, collapse=""), "\n")

  if (length(object@lb) == 0)
    object@lb <- 0
  # lb_str <- paste0( sprintf(" %5.3g", rep_len(object@lb, col.n)), collapse="")
  lb_str <- paste0( sprintf(format_g, rep_len(object@lb, col.n)), collapse="")
  cat(paste0("Lower", lb_str, collapse=""), "\n")


  if (length(object@type) == 0)
    object@type <- "real"
  # type_str <- paste0( sprintf(" %5.5s", rep_len(object@type, col.n)), collapse="")
  type_str <- paste0( sprintf(format_s, rep_len(object@type, col.n)), collapse="")
  cat(paste0("Type ", type_str, collapse=""), "\n")
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

