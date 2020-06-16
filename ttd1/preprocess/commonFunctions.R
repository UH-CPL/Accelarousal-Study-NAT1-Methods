###################### COMMON LIBRARIES ########################
library(plotly)
library(lme4)
library(lmerTest)
library(nlme)
library(formattable)
library(xgboost)

library(EBImage)
library(ggplot2)
library(stringr)
library(gridExtra)
library(latex2exp)

####################### COMMON FUNCTIONS #########################
calculateMean <- function(l) {
  return(mean(l, na.rm = T))
}

calculateStd <- function(l) {
  return(sd(l, na.rm = T))
}