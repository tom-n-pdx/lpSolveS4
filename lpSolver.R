#
# Test Solver Class
#

require(methods)

#
#
# lpSolver Class
#
#

setClass("lpSolver",
         slots = c(
           modelname = "character",
           constraints = "matrix",
           rhs = "numeric",
           obj = "numeric",
           sense = "character",                       # Linear constraint type - >=, ==,
           lb = "numeric",
           ub = "numeric",
           modelsense = "character"
         )
)


#
# Carefull - how chane size without creating error on the fly?
#
# ToDo - Make check all vales?
validlpSoverObject <- function(object){

  # Check Character values with limited values
  if (length(object@modelsense) > 0 &&
      ! object@modelsense %in% c("min", "max")){
    return("modelsense Not min or max")
  }

  sense_legal.l <- c("<=", "==", ">=", "")
  if (length(object@sense) > 0){
    for (i in 1:length(object@sense)){
      if (! object@sense[i] %in% sense_legal.l){
        return("sense contains illegal value")
      }
    }
  }


    return(TRUE)
}

#setValidity("lpSolver", validlpSoverObject)

lpq_good <- new("lpSolver",
                modelname="DEA CCR",
                constraints = matrix( c(1, 2, 3, 4), nrow=2, byrow=TRUE),
                rhs = c(5, 6, 7, 8)
)

# lpq_bad1 <- new("lpSolver",
#                 name=3,
#                 constraints = c(1, 2, 3, 4),
#                 rhs = c(5, 6, 7, 8)
# )


lpq_bad <- new("lpSolver")
lpq_bad@constraints   <- matrix(c(1,1,0,0,1,1), nrow=2, byrow=T)
lpq_bad@obj           <- c(1,1,2)
lpq_bad@modelsense    <- "max"
lpq_bad@rhs           <- c(1,1)
lpq_bad@sense         <- c("<=", "<=")

