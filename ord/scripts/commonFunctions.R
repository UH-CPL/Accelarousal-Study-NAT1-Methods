############# COMMON LIBARIES #######################
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
library(corrplot)
library(dplyr)
library(RColorBrewer)
library(htmltools)
library(webshot)  
library(dendextend)

############# COMMON FUNCTIONS ######################
############# 1. General Functions ##################
# Calculate mean of a list
# @param l: vector
calculateMean <- function(l) {
  l <- l[!is.na(l)]
  l <- l[!is.null(l)]
  u <- mean(l)
  return(u)
}

# Calculate SD of a list
# @param l: vector
calculateStd <- function(l) {
  l <- l[!is.na(l)]
  l <- l[!is.null(l)]
  return(sd(l))
}

############## 2. Data Loading, Processing ########################
# Load processed data
# @param persons: list of subject IDs
# @returns: List of data frames
loadProcessedData <- function(persons) {
  datas <- vector(mode = "list", length = length(persons))
  datas_baseline <- vector(mode = "list", length = length(persons))
  pp_means <- vector(mode = "list", length = length(persons))
  
  names(datas_baseline) <- persons
  names(pp_means) <- persons
  names(datas) <- persons
  
  extra <- 0.0000000001
  all <- data.frame()
  all_baseline <- data.frame()
  
  for (p in persons) {
    datas[[p]] <- read.csv(str_interp("./data/processed/combined/P${person}.csv", list(person = p)))
    datas_baseline[[p]] <- read.csv(str_interp("./data/processed/baseline/pp/P${person}.csv", list(person = p)))
    
    # Compute the mean
    p_pp_nr <- datas_baseline[[p]]$pp_nr2
    p_pp_nr <- p_pp_nr[!is.na(p_pp_nr)]
    pp_means[[p]] <- mean(p_pp_nr)
  }
  
  for (p in persons) {
    # On-road
    df_p <- datas[[p]]
    df_p$ppNormalized <- df_p$pp_nr5 - pp_means[[p]]
    df_p$ppLogNormalized <- log(df_p$pp_nr5 + extra) - log(pp_means[[p]] + extra)
    df_p$Subject <- p
    
    all <- rbind(all, df_p)
    
    # Baseline
    df_p_baseline <- datas_baseline[[p]]
    df_p_baseline$Subject <- p
    all_baseline <- rbind(all_baseline, df_p_baseline)
  }
  
  return(list(dfAll=all, dfAllBaseline=all_baseline))
}

# Process temporal data
# @param all: data frame of all subjects
# @param persons: sorted vector of all subject IDs
# @returns: data frame
processTemporalData <- function(all, persons, timePrevSeconds=30, timeNextSeconds=5) {
  temp_all <- all
  all <- data.frame()
  for (p in persons) {
    # On-road
    p_data <- temp_all[temp_all$Subject == p, ]
    n <- nrow(p_data)
    # print(paste(n, " rows"))
    
    if (n > 0) {
      # Add Speed_u, Speed_std, Acc_u, Acc_std, Brake_u, Brake_std, Steering_u, Steering_std
      p_data$Speed_u <- rep(NA, n)
      p_data$Speed_std <- rep(NA, n)
      p_data$Acc_u <- rep(NA, n)
      p_data$Acc_std <- rep(NA, n)
      p_data$Brake_u <- rep(NA, n)
      p_data$Brake_std <- rep(NA, n)
      p_data$Steering_u <- rep(NA, n)
      p_data$Steering_std <- rep(NA, n)
      
      p_data$ppNext <- rep(NA, n)
      
      # Driving stat info of prev seconds
      for (i in timePrevSeconds:n) {
        sfrom <- i - timePrevSeconds
        sto <- i
        p_data$Speed_u[i] <- calculateMean(p_data$Speed[sfrom:sto])
        p_data$Acc_u[i] <- calculateMean(p_data$Accelerator[sfrom:sto])
        p_data$Brake_u[i] <- calculateMean(p_data$Brake[sfrom:sto])
        p_data$Steering_u[i] <- calculateMean(p_data$Steering[sfrom:sto])
        
        p_data$Speed_std[i] <- calculateStd(p_data$Speed[sfrom:sto])
        p_data$Acc_std[i] <- calculateStd(p_data$Accelerator[sfrom:sto])
        p_data$Brake_std[i] <- calculateStd(p_data$Brake[sfrom:sto])
        p_data$Steering_std[i] <- calculateStd(p_data$Steering[sfrom:sto])
      }
      
      # PP of next seconds
      for (i in 1:(n - timeNextSeconds)) {
        sfrom <- i + 1
        sto <- i + timeNextSeconds
        p_data$ppNext[i] <- calculateMean(p_data$ppLogNormalized[sfrom:sto])
      }
      all <- rbind(all, p_data)
    }
  }
  
  # Remove NA value
  all <- all[!is.na(all$Speed_u) & !is.na(all$Speed_std)
             & !is.na(all$Acc_u) & !is.na(all$Acc_std)
             & !is.na(all$Brake_u) & !is.na(all$Brake_std)
             & !is.na(all$Steering_u) & !is.na(all$Steering_std)
             & !is.na(all$ppNext), ]
  return(all)
}

################ 3. Correllation #########################
# Compute and draw correlation matrix of a subject
# @param p: subject ID
# @param all: Data frame of all subject
# @param behavioralMatrix: Matrix storing correlation
# @param rowNo: subject index
# @param skipPlot: disable plotting function
# @returns: void
computeAndPlotCorrelation <- function(p, all, behavioralMatrix, rowNo = 1, skipPlot = F) {
  pData <- all[all$Subject == p, ]
  # Correlation
  pCorrData <- pData %>% select(
    ppNext, Speed, Accelerator, Brake, HR, BR, Steering,
    Speed_u, Acc_u, Brake_u, Steering_u,
    Speed_std, Acc_std, Brake_std, Steering_std
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
    round(rowCorPP[["Speed_u"]], digits = 5),
    round(rowCorPP[["Speed_std"]], digits = 5),
    round(rowCorPP[["Acc_u"]], digits = 5),
    round(rowCorPP[["Acc_std"]], digits = 5),
    round(rowCorPP[["Brake_u"]], digits = 5),
    round(rowCorPP[["Brake_std"]], digits = 5),
    round(rowCorPP[["Steering_u"]], digits = 5),
    round(rowCorPP[["Steering_std"]], digits = 5)
  )

  # Draw
  if (!skipPlot) {
    cPlot <- corrplot(corMatrix, method = "circle", type = "lower", title = paste0("Correlation Matrix of Subject #", p), mar = c(0, 0, 4, 0), col = col, tl.col = "black")
  }
}

# Compute and draw correlation matrix of all subject
# @param all: Data frame of all subjects
# @param skipPlot: disable plotting function
# @returns: void
computeAndPlotCorrelationOfAllSubjects <- function(all, skipPlot = F) {
  pCorrData <- all %>% select(ppNext, Speed, Accelerator, Brake, HR, BR, Steering, Speed_u, Speed_std, Acc_u, Acc_std, Brake_u, Brake_std, Steering_u, Steering_std)
  pCorrData$PP <- pCorrData$ppNext
  pCorrData$ppNext <- NULL

  pCorrData <- pCorrData[!is.na(pCorrData$PP), ]

  col <- rev(brewer.pal(n = 10, name = "RdBu"))
  corMatrix <- cor(pCorrData)
  
  if (!skipPlot) {
    corrplot(corMatrix, method = "circle", type = "lower", title = paste0("Correlation Matrix of All Subjects"), mar = c(0, 0, 4, 0), col = col, tl.col = "black")
  }
}

################ 4. Clustering #########################
getClusterName <- function(s, clusters) {
  sID <- str_replace(s, "Subject ", "")
  if (str_sub(sID, 1,1) != "#") {
    sID <- paste0("#", sID)
  }
  return(paste0("C", clusters[sID]))
}


################ 5. Linear Model #######################
# Plot Linear Model for each subject
# @param p: subject ID
# @param all: data frame of all subject
# @param usePhysiological: whether using other physiological information such as heart rate, breath rate
# @returns: model
plotLinearModel <- function(p, all, usePhysiological=F) {
  pData <- all[all$Subject==p,]
  pData <- pData[!is.na(pData$pp),]
  if (usePhysiological) {
    linearModel <- lm(ppLogNormalized ~ Speed_u + Speed_std + Acc_u + Acc_std + Brake_u + Brake_std + Steering_u + Steering_std + HR + BR, data=pData)
    # Diagnostic
    plot(linearModel,caption = paste0("Linear Model (with Physiological) of Subject #", p),which = 1)
    return(linearModel)
  } else {
    linearModel <- lm(ppLogNormalized ~ Speed_u + Speed_std + Acc_u + Acc_std + Brake_u + Brake_std + Steering_u + Steering_std, data=pData)
    # Diagnostic
    plot(linearModel,caption = paste0("Linear Model (without Physiological) of Subject #", p),which = 1)
    return(linearModel)
  }
}

# Plot Linear Model for all subjects
# @param all: data frame of all subject
# @param usePhysiological: whether using other physiological information such as heart rate, breath rate
# @param removeIncompletedSubject: remove specific subjects who having insufficient data
# @returns: model
plotLinearModelForAllSubjects <- function(all, usePhysiological=F, removeIncompletedSubject=T) {
  linearModelAll <- NULL
  pData <- all[!is.na(all$pp),]
  if (removeIncompletedSubject) {
    pData <- pData[!(pData$Subject %in% c("11")),]
  }
  if (usePhysiological) {
    linearModelAll <- lmer(ppLogNormalized ~ (1|Subject) + Speed_u + Speed_std + Acc_u + Acc_std + Brake_u + Brake_std + Steering_u + Steering_std + HR + BR, data=pData, REML = T)
  } else {
    linearModelAll <- lmer(ppLogNormalized ~  (1|Subject) + Speed_u + Speed_std + Acc_u + Acc_std + Brake_u + Brake_std + Steering_u + Steering_std, data=pData, REML = T)
  }
  return(linearModelAll)
}


################## 6. ML Model #########################



################## 7. Visualization ####################
# Store a table to an image
# @param f: formatable object
# @param file: filename
# @param delay: time to wait before shotting
# @returns: void
exportFormatTable <- function(f, file, width = "100%", height = NULL, 
                              background = "white", delay = 0.2) {
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}