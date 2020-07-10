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
computeAndPlotCorrelationOfAllSubjects(all, skipPlot = T)

###################### 2. Correlation of each Subject ####################
for (p in persons) {
  computeAndPlotCorrelation(p, all, behavioralMatrix, rowNo = match(p, persons), skipPlot = T)
}

###################### LINEAR MODEL ######################################
## Linear Model
layout(matrix(c(1, 2, 3, 4), 2, 2))

### 1. Linear model for all subjects
linearModel_AllSubjects <- plotLinearModelForAllSubjects(all, usePhysiological = F, removeIncompletedSubject = F)
plot(linearModel_AllSubjects,
  main = str_interp("Linear Model (Previous ${tPre}s, Next ${tNext}s)", list(tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS)),
  ylab = "Residual",
  xlab = "Fitted"
)

# Store plot to a file
fname <- str_interp("./plots/linearmodel/lm_Prev_${tPre}s_Next_${tNext}s.jpg", list(tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS))
jpeg(fname, width = 600)
plot(linearModel_AllSubjects,
  main = str_interp("Linear Model (Previous ${tPre}s, Next ${tNext}s)", list(tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS)),
  ylab = "Residual",
  xlab = "Fitted"
)
dev.off()

print(summary(linearModel_AllSubjects))
print("ANOVA:")
print(anova(linearModel_AllSubjects))

# Export the anova table
lmCoeffs <- summary(linearModel_AllSubjects)$coefficients
lmAnova <- anova(linearModel_AllSubjects)

print(xtable(lmCoeffs, digits=c(0,5,5,5,5,5)))
print(xtable(lmAnova), digits=c(0,5,5,5,5,5))

# Log
sink(str_interp("./outputs/linearmodel/lm_Prev_${tPre}s_Next_${tNext}s.txt", list(tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS)))
print(summary(linearModel_AllSubjects))
sink()

### 2. Linear model for each subject
for (p in persons) {
  plotLinearModel(p, all, usePhysiological = F)
}
