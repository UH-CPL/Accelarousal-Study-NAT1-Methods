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

################ 3. Correllation #########################
# Compute and draw correlation matrix of a subject
# @param p: subject ID
# @param all: Data frame of all subject
# @param behavioralMatrix: Matrix storing correlation
# @param rowNo: subject index
# @param skipPlot: disable plotting function
# @returns: void
computeAndPlotCorrelation <- function(p, all, behavioralMatrix, drive=1, window=3, rowNo = 1, skipPlot=F, savePlot=T) {
  # Sample data
  pData <- getSampleSegmentedData(p, all, window)
  
  # Correlation
  pCorrData <- pData %>% select(
    ppNext,
    Speed_u, Speed_std,
    Acc_u, Acc_std, 
    Brake_u, Brake_std, 
    Steering_u, Steering_std
  )
  pCorrData$PP <- pCorrData$ppNext
  pCorrData$ppNext <- NULL
  
  pCorrData <- pCorrData[!is.na(pCorrData$PP), ]
  
  col <- rev(brewer.pal(n = 10, name = "RdBu"))
  corMatrix <- cor(pCorrData)
  # Store to behavioral matrix
  rowCorPP <- corMatrix[nrow(corMatrix), ]
  behavioralMatrix[rowNo, ] <<- c(
    paste0("Subject #", p),
    round(rowCorPP[["Speed_u"]], digits = 3),
    round(rowCorPP[["Speed_std"]], digits = 3),
    round(rowCorPP[["Acc_u"]], digits = 3),
    round(rowCorPP[["Acc_std"]], digits = 3),
    round(rowCorPP[["Brake_u"]], digits = 3),
    round(rowCorPP[["Brake_std"]], digits = 3),
    round(rowCorPP[["Steering_u"]], digits = 3),
    round(rowCorPP[["Steering_std"]], digits = 3)
  )
  
  # Draw
  if (!skipPlot) {
    if (savePlot) {
      jpeg(str_interp("../plots/correlation/Drive${drive}/corrMatrix_P${p}_${tPre}s_Next_${tNext}s.jpg", list(p=p, drive=drive, tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS)),
           width = 960,height = 1040, res=140)
    }
    title <- paste0("Correlation Matrix of Subject #", p)
    corNames <- CORRELATION_NAMES
    corrplot(corMatrix, method = "circle", type = "lower", title = "", mar = c(1, 6, 7, 0), col = col, tl.col = "white", tl.pos='n')
    text(-0.2, 9:1, corNames, cex=1.3)
    text(1:9, c(10:2) + 0.2, corNames, srt=90, cex=1.3)
    if (savePlot) {
      dev.off()
    }
  }
}

