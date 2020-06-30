##########################################################################
# This script:
# 1. Defines utility functions for use in the analysis.
#
# Exports:
# 1. q5(): factor continuous variable in quintiles
# 2. qBr(): find quintile breaks
# 3. 
#
# To-do:
# 1. 
##########################################################################

## 1. ----
q5 <- function(variable) {as.factor(ntile(variable, 5))}

## 2. ----
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}