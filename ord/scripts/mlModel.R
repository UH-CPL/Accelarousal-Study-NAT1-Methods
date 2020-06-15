###################### IMPORT COMMON LIBRARIES/ FUNCTIONS ##############
source("./settings/settings.R")
source("./scripts/commonFunctions.R")
DEBUG_MODE <- F

###################### WORKING DIRECTORY ###############################
# setwd('./')
print(paste0("Working directory: ", getwd()))

###################### LIBRARY LOADING #################################
# install.packages("randomForest")
# install.packages("MLmetrics")
# install.packages("caret")
library(randomForest)
library(ROCR)
library(caret)

###################### VARIABLE DECLAREATION ###########################
persons <- SELECTED_SUBJECTS

###################### DATA LOADING ####################################
data <- loadProcessedData(persons)
all <- data$dfAll
allBaseline <- data$dfAllBaseline

if (DEBUG_MODE) {
  print(length(all$Time))
  print(length(allBaseline$Time))
}

###################### PROCESS ##########################################
all <- processTemporalData(all, persons, timePrevSeconds = TIME_PREV_SECONDS, timeNextSeconds = TIME_NEXT_SECONDS)

if (DEBUG_MODE) {
  print(length(all$Time))
}

###################### CORRELLATION #####################################
behavioralColumns <- BEHAVIORAL_COLUMNS
behavioralMatrix <- matrix(nrow = length(persons), ncol = length(behavioralColumns))

###################### 1. Correlation of all Subjects ###################
computeAndPlotCorrelationOfAllSubjects(all, skipPlot = T)

###################### 2. Correlation of each Subject ####################
for (p in persons) {
  computeAndPlotCorrelation(p, all, behavioralMatrix, rowNo = match(p, persons), skipPlot = T)
}

###################### ML MODEL ##########################################
ML_USE_CLUSTER <- F

n <- length(persons)
nPerfRow <- ifelse(ML_USE_CLUSTER, n + 2, n + 1)
perfDf <- data.frame(
  Subject = character(nPerfRow),
  Cls_High = integer(nPerfRow),
  Cls_Low = integer(nPerfRow),
  Accuracy = double(nPerfRow),
  Precision = double(nPerfRow),
  Recall = double(nPerfRow),
  Spec = double(nPerfRow),
  F1 = double(nPerfRow),
  NPV = double(nPerfRow),
  AUC = double(nPerfRow),
  stringsAsFactors = FALSE
)

# Add cluster information to all data
if (ML_USE_CLUSTER) {
  all <- all %>% mutate(Cluster = getClusterName(Subject, clusters))
  all$Cluster <- as.factor(all$Cluster)
}

# Train
trainAndTestModel <- function(p, idx = 1, useData = "All", useCluster = F) {
  if (!is.na(p) && idx > 0) {
    pData <- all[all$Subject == p, ]
  } else {
    pData <- all
    pData <- pData[complete.cases(pData), ]
  }

  pData <- pData[!is.na(pData$ppNext), ]
  data_PP_Log_P <- pData$ppNext
  data_PP_Log_P <- data_PP_Log_P[!is.na(data_PP_Log_P)]
  mat_PP_P <- matrix(data_PP_Log_P, nrow = 1, ncol = length(data_PP_Log_P))
  
  # Select a threshold
  threshold_PP_Log_P <- NA
  if (!is.na(p) && p == "01") { 
    # As #01 has imbalanced distribution of PP, we manually select a threshold equal to the baseline PP
    threshold_PP_Log_P <- mean(allBaseline[allBaseline$Subject==p,]$pp_nr2)
  } else {
    # Use Otsu algorithm to select a threshold to discriminate the 2 classes
    threshold_PP_Log_P <- otsu(mat_PP_P, range = c(min(data_PP_Log_P), max(data_PP_Log_P)))
  }

  pp_min <- min(data_PP_Log_P)
  pp_max <- max(data_PP_Log_P)

  pData$clsPP <- ifelse(pData$ppNext >= threshold_PP_Log_P, "High", "Low")
  pData$clsPP <- factor(pData$clsPP)

  # Selected data
  selectedColumns <- c()
  if (useData == "All") {
    selectedColumns <- c(
      "clsPP",
      "Speed_u", "Acc_u", "Brake_u", "Steering_u",
      "Speed_std", "Acc_std", "Brake_std", "Steering_std"
    )
  } else if (useData == "SpeedOnly") {
    selectedColumns <- c("clsPP", "Speed_u", "Speed_std")
  } else if (useData == "AccOnly") {
    selectedColumns <- c("clsPP", "Acc_u", "Acc_std")
  } else if (useData == "SpeedAndAcc") {
    selectedColumns <- c("clsPP", "Speed_u", "Speed_std", "Acc_u", "Acc_std")
  } else {
    print("No data selection applied. Please select a value for param `useData`.")
    return(F)
  }

  if (useCluster) {
    selectedColumns <- c(selectedColumns, "Cluster")
  }

  pSelected <- pData %>% select(selectedColumns)

  nHigh <- nrow(pSelected[pSelected$clsPP == "High", ])
  nLow <- nrow(pSelected[pSelected$clsPP == "Low", ])

  if(DEBUG_MODE) {
    print(paste("High =", nHigh))
    print(paste("Low =", nLow))
  }

  # Split dataset
  set.seed(43)
  nFolds <- 10

  # XGB
  param <- list(
    objective = "binary:logistic",
    booster = "gbtree",
    eval_metric = "auc",
    eta = 0.1,
    max_depth = 10,
    gamma = 0.8,
    min_child_weight = 3,
    subsample = 1,
    colsample_bytree = 0.5,
    stratified = T
  )
  pSelected <- pSelected %>% mutate(clsPP = ifelse(clsPP == "High", 1, 0))

  aucs <- c()
  if (!useCluster) {
    xgb_m <- xgb.cv(
      params = param,
      data = as.matrix(pSelected %>% select(-clsPP)),
      label = pSelected$clsPP,
      nrounds = 500,
      verbose = F,
      prediction = T,
      maximize = T,
      nfold = nFolds,
      metrics = "auc",
      early_stopping_rounds = 100,
      scale_pos_weight = 1,
      stratified = T
    )
    aucs <- c(aucs, as.numeric(xgb_m$evaluation_log[xgb_m$best_iteration, "test_auc_mean"]))
  }
  
  # Prediction
  pSelected$clsPPPred <- round(xgb_m$pred)

  # Get average performance
  performance <- computePerformanceResults(pSelected %>% select(clsPP, clsPPPred))
  acc <- performance[1]
  prec <- performance[4]
  recall <- performance[3]
  spec <- performance[2]
  npv <- performance[5]
  f1 <- (2 * recall * prec) / (recall + prec)
  
  auc <- mean(aucs)

  # Return
  rtn <- list(
    accuracy = acc,
    recall = recall,
    precision = prec,
    specificity = spec,
    f1 = f1,
    npv = npv,
    auc = auc
  )

  # Store to common variable
  if (!is.na(p) && idx > 0) {
    perfDf$Subject[idx] <<- paste0("Subject #", p)
    perfDf$Cls_High[idx] <<- nHigh
    perfDf$Cls_Low[idx] <<- nLow
    perfDf$Accuracy[idx] <<- acc
    perfDf$Precision[idx] <<- prec
    perfDf$Recall[idx] <<- recall
    perfDf$Spec[idx] <<- spec
    perfDf$F1[idx] <<- f1
    perfDf$NPV[idx] <<- npv
    perfDf$AUC[idx] <<- auc
  } else {
    idx <- n + ifelse(useCluster, 2, 1)
    perfDf$Subject[idx] <<- ifelse(useCluster, "All (w. Cluster)", "All (No Cluster)")
    perfDf$Cls_High[idx] <<- nHigh
    perfDf$Cls_Low[idx] <<- nLow
    perfDf$Accuracy[idx] <<- acc
    perfDf$Precision[idx] <<- prec
    perfDf$Recall[idx] <<- recall
    perfDf$Spec[idx] <<- spec
    perfDf$F1[idx] <<- f1
    perfDf$NPV[idx] <<- npv
    perfDf$AUC[idx] <<- auc
  }
}

################### NOTES ################
# - Random Forest model to predict class of PP (High, Low)
# - I used Otsu algoritm to mark the class of PP, and then use driving data (Speed, Acc, Brake, Steering) to predict the class in 5 seconds.
# - For each subject, I exclusively trained and tested a ML model to predict the class of PP.
# - Applied Cross validation with stratified KFold=10.
# - Used temporal information of data within last 5/10/15/30 seconds.
# - Data of Subject#1 and #11 is so significant imbalance that need more evaluation (Here, I used AUC metric to evaluate the result).

### 1. ML Model 1: Use all driving data (includes Speed, Accelertor, Brake, Steering)
for (p in persons) {
  trainAndTestModel(p, idx = match(p, persons), useData = "All")
}

# Train model for all subjects
trainAndTestModel(NA, idx = 0, useData = "All")
# if (ML_USE_CLUSTER) {
#   trainAndTestModel(NA, idx = 0, useData = "All", useCluster = T)
# }
head(perfDf, n = length(persons))

# Export
ftable_perf <- formattable(
  perfDf,
  align = c("c", "r", "r", "r", "r", "r", "r", "r", "r"),
  list(
    `Subject` = formatter("span", style = ~ formattable::style(color = "black", font.weight = "bold"))
  )
)
fname_perf <- str_interp("./plots/mlperf/perf_Prev_${tPre}s_Next_${tNext}s.jpg", list(tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS))
exportFormatTable(ftable_perf, fname_perf)
ftable_perf

# Write perf to csv file
write.csv(perfDf, str_interp("./outputs/mlperf/perf_Prev_${tPre}s_Next_${tNext}s.csv", list(tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS)))
