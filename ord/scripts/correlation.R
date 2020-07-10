###################### IMPORT COMMON LIBRARIES/ FUNCTIONS ##############
source("./settings/settings.R")
source("./scripts/commonFunctions.R")
DEBUG_MODE <- F

###################### WORKING DIRECTORY ###############################
# setwd('./')
print(paste0("Working directory: ", getwd()))

###################### LIBRARY LOADING #################################

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

###################### CORRELATION #####################################
behavioralColumns <- BEHAVIORAL_COLUMNS
behavioralMatrix <- matrix(nrow = length(persons), ncol = length(behavioralColumns))

###################### 1. Correlation of all Subjects ###################
computeAndPlotCorrelationOfAllSubjects(all, window=TIME_PREV_SECONDS, skipPlot = F, savePlot=T)

###################### 2. Correlation of each Subject ####################
for (p in persons) {
  computeAndPlotCorrelation(p, all, behavioralMatrix, window=TIME_PREV_SECONDS, rowNo = match(p, persons), skipPlot = F, savePlot=T)
}
