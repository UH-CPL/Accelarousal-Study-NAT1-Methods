############ COMMON LIBRARIES ################
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
library(dendextend)
library(caret)

############ COMMON FUNCTIONS ################
getSampleSegmentedData <- function(p, all, window=5) {
  if (is.na(p)) {
    pData <- all[complete.cases(all), ]
    return(pData[seq(1, nrow(pData), window), ])
  } else {
    pData <- all[all$Subject == p & complete.cases(all), ]
    return(pData[seq(1, nrow(pData), window), ])
  }
}