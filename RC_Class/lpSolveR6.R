#
# Test lpSolveR6
#

# CHange to lpSolve nrow, ncol

library(R6)

lpSolveR6 <- R6Class("lpSolveR6",
  public = list(

    initialize = function(nvar = NA,  nconst= NA) {
      if (!is.na(nvar) && (
        nvar < 1 || !is.numeric(nvar) || length(nvar) > 1))
        stop("nvar must be sclalr interger >= 1")

      if (!is.na(nconst) && (
        nconst < 1 || !is.numeric(nconst) || length(nconst) > 1))
        stop("nconst must be sclalr interger >= 1")

      private$nvar     <- nvar
      private$nconst   <- nconst

      if(!is.na(nvar) && !is.na(nconst)){
        private$a_     <- matrix(NA, nrow=nvar, ncol=nconst)
      }


    },

    print = function() {
      cat("lpSolveR6 Object ", private$nvar, "X", private$nconst, "\n")
      print(private$a_)
      invisible(self)
    }
  ),

  active = list(
    A = function(value) {
      if (missing(value)) return(private$a)
      else private$a_ <- value
    }
  ),

  private = list(
    nvar    = NULL,
    nconst  = NULL,

    a_      = NULL,
    obj     = NULL,
    modelsense = "max",
    rhs     = NULL,
    sense   = NULL
  )

)

a <- lpSolveR6$new()

class(a)

a <- lpSolveR6$new(3, 5)

# Check for legal values
a <- lpSolveR6$new(-3, 5)
a <- lpSolveR6$new("bob", 5)
a <- lpSolveR6$new( c(1, 3), 5)

a <- lpSolveR6$new( NA, 5)

# Check for legal nconst values
a <- lpSolveR6$new(3, -5)
a <- lpSolveR6$new("bob", 5)
a <- lpSolveR6$new( c(1, 3), 5)

a <- lpSolveR6$new( NA, 5)

a <-lpSolveR6$new(2, 3)
a



# New - make new linear eqn N Vars x N Constraints

# Assign C
a   <-lpSolveR6$new(2, 3)


a$A <- matrix(c(1,1,0,0,1,1), nrow=2, byrow=T)
a$A[1,2]
a$A[1,1]






# Assign RHS

# Assign Obj

# Print

