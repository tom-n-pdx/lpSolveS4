#
# doMC examples
# From doMC vingette

#
# Warning - connection objects will likely fail under doMC
# Alaso - don't run inside a GUI
#

# library(foreach)
library(doMC)
registerDoMC(2)
# Undo by using registerDoSEQ

foreach(i=1:3) %dopar% sqrt(i)

#
# Run two cores
#
x       <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials  <- 10000
ptime   <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %dopar% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})[3]
ptime

#
# Serial
#
stime <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %do% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})[3]
stime

getDoParWorkers()

getDoParName()

getDoParVersion()

#
# Can set MC options
# see ?mclapply
#
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
foreach(i=1:3, .options.multicore=mcoptions) %dopar% sqrt(i)

