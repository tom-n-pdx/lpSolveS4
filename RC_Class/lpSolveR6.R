#
# Test lpSolveR6
#

#
# Check paramaters
#


library(R6)

sense_legal.l <- c("free", "<=", ">=", "=")
type_legal.l  <- c("real", "integer", "binary")


lpSolveR6 <- R6Class("lpSolveR6",
  public = list(

    # ncol        = NULL,
    # nrow        = NULL,

    modelsense  = "max",                          # optional, min or max
    modelname   = "",                             # optional, text

    A           = NULL,                           # constraints, numeric, required ncols x nrows

    obj         = NA,                             # required, numeric, length = ncols
    lb          = 0,                              # optional, numeric, length = ncols
    ub          = Inf,                            # optional, numeric, length = ncols
    type        = "real",                         # optional, valid type, length = ncols

    rhs         = 0,                              # required, numeric, length = nrows
    sense       = "free",                         # optional, valid type, length = nrows


    initialize = function(A=matrix(NA, 0, 0), obj=0, rhs=0, sense="free",
                          lb = 0, ub=Inf, type="real",
                          modelsense="max", modelname="") {

      self$modelsense   <- modelsense
      self$modelname    <- modelname

      self$A            <- A

      self$obj          <- obj
      self$lb           <- lb
      self$ub           <- ub
      self$type         <- type

      self$rhs          <- rhs
      self$sense        <- sense

      },


    print = function() {
      if (!is.na(self$A)){
        col.n     <- ncol(self$A)
        row.n     <- nrow(self$A)
      } else {
        col.n     <- NA
        row.n     <- NA
      }

      cat("lpSolveR6 Object ", self$modelname, " -- ", col.n, "X", row.n, "\n")
      print(self$A)
      invisible(self)
    },

    show = function(digits=NA) {

      cat("lpSolve show: ", self$modelname, "\n")

      # Get digits option from environemnt
      digits <- ifelse(!is.na(digits), digits, getOption("digits"))

      width  <- digits + 3
      format_b <- "%5.5s"
      format_s <- paste0(" %", width, ".", width,  "s", collapse = "")
      format_g <- paste0(" %", width, ".", digits, "g", collapse = "")

      # Get Size
      if (length(self$A) < 2){
        cat("Can't show lpSolve object with undefined constraints - falling back to debug print\n")
        self$print()
        return()
      }

      col.n     <- ncol(self$A)
      col.names <- colnames(self$A, do.NULL=FALSE, prefix = "C")

      row.n     <- nrow(self$A)
      row.names <- rownames(self$A, do.NULL=FALSE, prefix = "R")

      # temp_cons <- self$A
      # Build the format strings we'll need - one for lines w/ strings, one for lines numbers
      # row_fmt_s  <- paste0(c(format_b, rep_len(format_s, col.n)),
      #                      collapse="")
      # row_fmt_g  <- paste0(c(format_b, rep_len(format_g, col.n), " %4s", format_g),
      #                      collapse="")


      # Col Names
      cat("    ", paste0(sprintf(format_s, col.names), collapse=""), "\n")
      # cat("format: ", row_fmt_s, "\n")
      # cat(sprintf(row_fmt_s, col.names, col.names), "\n")

      # Min/Max & Objective
      sense_str <- sprintf("%5.5s", self$modelsense)
      obj_str   <- paste0(sprintf(format_g, rep_len(self$obj, col.n)), collapse="")
      cat(paste0(sense_str, obj_str, collapse=""), "\n")

      # row name, constraint row, sense, rhs
      for (i in 1:row.n){
        name_str  <- sprintf("%5.5s", row.names[i])
        cons_str  <- paste0(sprintf(format_g, rep_len(self$A[i,], col.n)), collapse="")
        sense_str <- sprintf(" %4s", rep_len(self$sense, row.n)[i])
        rhs_str   <- sprintf(format_g, rep_len(self$rhs,   row.n)[i])
        cat(paste0(name_str, cons_str, sense_str, rhs_str, collapse=""), "\n")
      }

      # upper & lower bounds
      ub_str <- paste0( sprintf(format_g, rep_len(self$ub, col.n)), collapse="")
      cat(paste0("Upper", ub_str, collapse=""), "\n")

      lb_str <- paste0( sprintf(format_g, rep_len(self$lb, col.n)), collapse="")
      cat(paste0("Lower", lb_str, collapse=""), "\n")

      # type
      type_str <- paste0( sprintf(format_s, rep_len(self$type, col.n)), collapse="")
      cat(paste0("Type ", type_str, collapse=""), "\n")

      invisible(self)
    }

  )
)



