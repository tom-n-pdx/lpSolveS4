#
# Test lpSolveR6
#

# CHange to lpSolve nrow, ncol

library(R6)

sense_legal.l <- c("free", "<=", ">=", "=")
type_legal.l  <- c("real", "integer", "binary")



lpSolveR6 <- R6Class("lpSolveR6",
  public = list(
    ncol        = NULL,
    nrow        = NULL,
    modelsense  = "max",
    modelname   = "",

    A           = NULL,

    obj         = 0,
    lb          = 0,
    ub          = Inf,
    type        = "real",

    rhs         = 0,
    sense       = "free",


    initialize = function(nrow= NA, ncol = NA, modelsense="max", modelname="",
                          A=NA, obj=0, lb = 0, ub=Inf, type="real",
                          rhs=0, sense="free") {
      if (!is.na(nrow) && (
        nrow < 1 || !is.numeric(nrow) || length(nrow) > 1))
        stop("nrow must be sclalr interger >= 1")

      if (!is.na(ncol) && (
        ncol < 1 || !is.numeric(ncol) || length(ncol) > 1))
        stop("ncol must be sclalr interger >= 1")

      self$ncol   <- ncol
      self$nrow   <- nrow

      # if(!is.na(ncol) && !is.na(nrow)){
      #   self$A      <- matrix(NA, nrow=nrow, ncol=ncol)
      # }

      self$modelsense   <- modelsense
      self$modelname    <- modelname

      self$A            <- A

      self$obj          <- obj
      self$lb           <- lb
      self$ub           <- ub

      self$rhs          <- rhs
      self$sense        <- sense

      },


    print = function() {
      cat("lpSolveR6 Object ", self$modelname, " -- ", self$ncol, "X", self$nrow, "\n")
      print(self$A)
      invisible(self)
    },

    show = function() {

      cat("lpSolve show: ", self$modelname, "\n")

      # Get digits option from environemnt
      digits <- getOption("digits")

      digits <- 4
      width  <- digits + 3
      format_s <- paste0(" %", width, ".", width,  "s", collapse = "")
      format_g <- paste0(" %", width, ".", digits, "g", collapse = "")

      # Get Size
      if (length(self$A) > 0){
        col.n     <- ncol(self$A)
        col.names <- colnames(self$A, do.NULL=FALSE, prefix = "C")
        row.n     <- nrow(self$A)
        row.names <- rownames(self$A, do.NULL=FALSE, prefix = "R")
        temp_cons <- self$A
      } else {
        cat("Can't show lpSolve object with undefined constraints - falling back to debug print\n")
        # .lpSolveDebug(object)
        self$print()
        return()
      }

      # Col Names
      cat("    ", paste0(sprintf(format_s, col.names), collapse=""), "\n")

      # Min/Max & Objective
      sense_str <- sprintf("%5.5s", ifelse(length(self$modelsense) > 0,   self$modelsense, "max"))
      # obj_str   <- paste0(sprintf(" %5.3g", rep_len(object@obj, col.n)), collapse="")
      obj_str   <- paste0(sprintf(format_g, rep_len(self$obj, col.n)), collapse="")
      cat(paste0(sense_str, obj_str, collapse=""), "\n")

      if (length(self$sense) == 0)
        self$sense <- c("free")

      # row name, constraint row, sense, rhs
      for (i in 1:row.n){
        name_str  <- sprintf("%5.5s", row.names[i])
        # cons_str  <- paste0(sprintf(" %5.3g", rep_len(temp_cons[i,], col.n)), collapse="")
        cons_str  <- paste0(sprintf(format_g, rep_len(temp_cons[i,], col.n)), collapse="")
        sense_str <- sprintf(" %4s", rep_len(self$sense, row.n)[i])
        # rhs_str   <- sprintf(" %5.3g", rep_len(object@rhs,   row.n)[i])
        rhs_str   <- sprintf(format_g, rep_len(self$rhs,   row.n)[i])
        cat(paste0(name_str, cons_str, sense_str, rhs_str, collapse=""), "\n")
      }

      # upper & lower bounds
      if (length(self$ub) == 0)
        self$ub <- Inf
      # ub_str <- paste0( sprintf(" %5.3g", rep_len(object@ub, col.n)), collapse="")
      ub_str <- paste0( sprintf(format_g, rep_len(self$ub, col.n)), collapse="")
      cat(paste0("Upper", ub_str, collapse=""), "\n")

      if (length(self$lb) == 0)
        self$lb <- 0
      # lb_str <- paste0( sprintf(" %5.3g", rep_len(object@lb, col.n)), collapse="")
      lb_str <- paste0( sprintf(format_g, rep_len(self$lb, col.n)), collapse="")
      cat(paste0("Lower", lb_str, collapse=""), "\n")

      if (length(self$type) == 0)
        self$type <- "real"
      # type_str <- paste0( sprintf(" %5.5s", rep_len(object@type, col.n)), collapse="")
      type_str <- paste0( sprintf(format_s, rep_len(self$type, col.n)), collapse="")
      cat(paste0("Type ", type_str, collapse=""), "\n")

    }

  )
)



a <- lpSolveR6$new()
class(a)
a
str(a)


a <- lpSolveR6$new(3, 5)

# Check for legal values
a <- lpSolveR6$new(-3, 5)
a <- lpSolveR6$new("bob", 5)
a <- lpSolveR6$new( c(1, 3), 5)

a <- lpSolveR6$new( NA, 5)

# Check for legal nrow values
a <- lpSolveR6$new(3, -5)
a <- lpSolveR6$new("bob", 5)
a <- lpSolveR6$new( c(1, 3), 5)

a <- lpSolveR6$new( NA, 5)

a <-lpSolveR6$new(2, 3)
a



# New - make new linear eqn N Vars x N Constraints

# Assign C
a   <-lpSolveR6$new(3, 4)
a$A <-matrix(seq(1, 12), 3, 4)
a


a$show()


a$A <- matrix(c(1,1,0,0,1,1), nrow=2, byrow=T)
a$A[1,2]
a$A[1,1]






# Assign RHS

# Assign Obj


# Assign name

a <- lpSolveR6$new(3, 4, modelname = "DEA Env Input", modelsense = "max",
                  A=matrix(seq(1, 12), 3, 4),
                  obj=seq(4,1),
                  sense=">=", rhs=seq(-3,-1))

a
str(a)

# Print

a   <-lpSolveR6$new(3, 4)
a$modelname  <- "DEA Env Output"
# a$modelsense <- "min"

a$A   <-matrix(seq(1, 12), 3, 4)
# a$obj <- seq(4,1)

# a$rhs <- seq(-3, -1)



a$show()



