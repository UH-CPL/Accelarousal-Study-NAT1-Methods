print("Start setting up ...")

print("Setuping visualization packages ...")
if (!require("plotly")) {
  install.packages("plotly")
}
if (!require("formattable")) {
  install.packages("formattable")
}
if (!require("latex2exp")) {
  install.packages("latex2exp")
}
if (!require("BiocManager")) {
  install.packages("BiocManager")
  BiocManager::install("EBImage")
}

print("Setuping analytical packages ...")
if (!require("corrplot")) {
  install.packages("corrplot")
}

print("Setuping linear model packages ...")
if (!require("lme4")) {
  install.packages("lme4")
}
if (!require("lmerTest")) {
  install.packages("lmerTest")
}
if (!require("nlme")) {
  install.packages("nlme")
}

print("Setuping ML packages ...")
if (!require("xgboost")) {
  install.packages("xgboost")
}
if (!require("randomForest")) {
  install.packages("randomForest")
}
if (!require("MLmetrics")) {
  install.packages("MLmetrics")
}
if (!require("caret")) {
  install.packages("caret")
}

print("Test packages ...")
# library(plotly)
# library(lme4)
# library(lmerTest)
# library(nlme)
# library(formattable)
# library(xgboost)

# library(EBImage)
# library(ggplot2)
# library(stringr)
# library(gridExtra)
# library(latex2exp)
packageVersion("plotly")

# Finish
print("Done!")
