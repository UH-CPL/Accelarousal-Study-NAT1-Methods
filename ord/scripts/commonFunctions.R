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
library(xtable)
library(cluster)

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

getResponseAbbr <- function(s) {
  if (s == "ppNext") { return("PP") }
  if (s == "HRNext") { return("HR") }
  if (s == "BRNext") { return("BR") }
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

  return(list(dfAll = all, dfAllBaseline = all_baseline))
}

# Process temporal data
# @param all: data frame of all subjects
# @param persons: sorted vector of all subject IDs
# @returns: data frame
processTemporalData <- function(all, persons, timePrevSeconds = 30, timeNextSeconds = 5) {
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
      p_data$HRNext <- rep(NA, n)
      p_data$BRNext <- rep(NA, n)

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
        p_data$HRNext[i] <- calculateMean(p_data$HR[sfrom:sto])
        p_data$BRNext[i] <- calculateMean(p_data$BR[sfrom:sto])
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

# Get sample data
# @param p: Subject ID
# @param all: Data frame of all subjects
# @returns: Data frame of a subject
getSampleSegmentedData <- function(p, all, window=5) {
  if (is.na(p)) {
    pData <- all[complete.cases(all), ]
    return(pData[seq(1, nrow(pData), window), ])
  } else {
    pData <- all[all$Subject == p, ]
    return(pData[seq(1, nrow(pData), window), ])
  }
}

getSignificanceDenote <- function(v, accept0.1=F) {
  if (v <= 0.001) {
    return("***");
  } else if (v <= 0.01) {
    return("**");
  } else if (v <= 0.05) {
    return("*")
  } else if (v <= 0.1 && accept0.1) {
    return('.')
  } else {
    return('')
  }
}

isSignificant <- function(s, type="string") {
  if (type == "string") {
    r <- (str_count(as.character(s), "\\*") >=2)
    return(r)
  } else {
    return(s <= 0.01)
  }
}

getValue <- function(s) {
  s <- as.character(s)
  return(as.numeric(gsub("[\\* ]", "", s)))
}

################ 3. Correllation #########################
# Compute and draw correlation matrix of a subject
# @param p: subject ID
# @param all: Data frame of all subject
# @param behavioralMatrix: Matrix storing correlation
# @param rowNo: subject index
# @param skipPlot: disable plotting function
# @returns: void
computeAndPlotCorrelation <- function(p, all, behavioralMatrix, behavioralMatrixWithPValue, response="ppNext", window=5, rowNo = 1, skipPlot=F, savePlot=F) {
  # Sample data
  pData <- getSampleSegmentedData(p, all, window)
  
  # Correlation
  if (response=="ppNext") {
    pCorrData <- pData %>% select(ppNext, 
                                  Speed_u, Speed_std,
                                  Acc_u, Acc_std, 
                                  Brake_u, Brake_std, 
                                  Steering_u, Steering_std)
    pCorrData$PP <- pCorrData$ppNext
    pCorrData$ppNext <- NULL
    pCorrData <- pCorrData[!is.na(pCorrData$PP), ]
    corNames <- CORRELATION_NAMES
  }
  if (response=="HRNext") {
    pCorrData <- pData %>% select(Speed_u, Speed_std,
                                  Acc_u, Acc_std, 
                                  Brake_u, Brake_std,
                                  Steering_u, Steering_std, 
                                  HRNext)
    pCorrData <- pCorrData[!is.na(pCorrData$HRNext), ]
    corNames <- CORRELATION_NAMES_HR
  }
  if (response=="BRNext") {
    pCorrData <- pData %>% select(Speed_u, Speed_std,
                                  Acc_u, Acc_std, 
                                  Brake_u, Brake_std,
                                  Steering_u, Steering_std, 
                                  BRNext)
    pCorrData <- pCorrData[!is.na(pCorrData$BRNext), ]
    corNames <- CORRELATION_NAMES_BR
  }

  col <- rev(brewer.pal(n = 10, name = "RdBu"))
  # Correlation
  corMatrix <- cor(pCorrData)
  # Significant test
  pTest <- cor.mtest(pCorrData, conf.level = .95)
  
  # Store to behavioral matrix
  rowCorPP <- corMatrix[nrow(corMatrix), ]
  rowPValue <- pTest$p[nrow(corMatrix), ]
  
  behavioralMatrix[rowNo, ] <<- c(
    paste0("S", p),
    round(rowCorPP[["Speed_u"]], digits = 3),
    round(rowCorPP[["Speed_std"]], digits = 3),
    round(rowCorPP[["Acc_u"]], digits = 3),
    round(rowCorPP[["Acc_std"]], digits = 3),
    round(rowCorPP[["Brake_u"]], digits = 3),
    round(rowCorPP[["Brake_std"]], digits = 3),
    round(rowCorPP[["Steering_u"]], digits = 3),
    round(rowCorPP[["Steering_std"]], digits = 3)
  )
  behavioralMatrixWithPValue[rowNo, ]  <<- c(
    paste0("S", p),
    paste0(round(rowCorPP[["Speed_u"]], digits = 3), getSignificanceDenote(round(rowPValue[[1]], digits = 3))),
    paste0(round(rowCorPP[["Speed_std"]], digits = 3), getSignificanceDenote(round(rowPValue[[2]], digits = 3))),
    paste0(round(rowCorPP[["Acc_u"]], digits = 3), getSignificanceDenote(round(rowPValue[[3]], digits = 3))),
    paste0(round(rowCorPP[["Acc_std"]], digits = 3), getSignificanceDenote(round(rowPValue[[4]], digits = 3))),
    paste0(round(rowCorPP[["Brake_u"]], digits = 3), getSignificanceDenote(round(rowPValue[[5]], digits = 3))),
    paste0(round(rowCorPP[["Brake_std"]], digits = 3), getSignificanceDenote(round(rowPValue[[6]], digits = 3))),
    paste0(round(rowCorPP[["Steering_u"]], digits = 3), getSignificanceDenote(round(rowPValue[[7]], digits = 3))),
    paste0(round(rowCorPP[["Steering_std"]], digits = 3), getSignificanceDenote(round(rowPValue[[8]], digits = 3)))
  )

  # Draw
  if (!skipPlot) {
    if (savePlot) {
      res <- ifelse(response=="ppNext", "PP", ifelse(response=="HRNext", "HR", "BR"))
      jpeg(str_interp("./plots/correlation/${res}/corrMatrix_P${p}_${tPre}s_Next_${tNext}s.jpg", list(p=p, res=res, tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS)),
           width = 960,height = 1040, res=140)
    }
    title <- paste0("Correlation Matrix of Subject #", p)
    
    # Draw corr plot
    corrplot(corMatrix, p.mat=pTest$p, method = "circle", type = "lower", title = "", mar = c(1, 6, 7, 0), col = col, tl.col = "white", tl.pos='n',
             insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "black")
    # Draw texts
    text(-0.2, 9:1, corNames, cex=1.4)
    text(1:9, c(10:2) + 0.2, corNames, srt=90, cex=1.4)
    # Draw emphasized box
    delta <- 0.5
    rect(0 + delta, 0 + delta + 0.02, 8 + delta, 1 + delta, border="red", lwd=2)
    if (savePlot) {
      dev.off()
    }
  }
}

# Compute and draw correlation matrix of all subject
# @param all: Data frame of all subjects
# @param skipPlot: disable plotting function
# @returns: void
computeAndPlotCorrelationOfAllSubjects <- function(all, window=5, skipPlot=F, savePlot=F, response="ppNext", group=NA, groupName='All') {
  # Select data
  pCorrData <- all %>% select(
    ppNext,
    Speed_u, Speed_std,
    Acc_u, Acc_std, 
    Brake_u, Brake_std, 
    Steering_u, Steering_std,
    Subject
  )
  
  # Sample data
  pCorrData <-getSampleSegmentedData(NA, pCorrData, window)
  
  pCorrData$PP <- pCorrData$ppNext
  pCorrData$ppNext <- NULL

  pCorrData <- pCorrData[!is.na(pCorrData$PP), ]
  
  # Filter
  if (!is.na(group) && length(group) > 0) {
    pCorrData <- pCorrData[pCorrData$Subject %in% group,]
  }
  pCorrData$Subject <- NULL

  col <- rev(brewer.pal(n = 10, name = "RdBu"))
  corMatrix <- cor(pCorrData)
  # Significant test
  pTest <- cor.mtest(pCorrData, conf.level = .95)

  if (!skipPlot) {
    if (savePlot) {
      res <- ifelse(response=="ppNext", "PP", ifelse(response=="HRNext", "HR", "BR"))
      jpeg(str_interp("./plots/correlation/${res}/corrMatrix_${groupName}_${tPre}s_Next_${tNext}s.jpg", list(res=res, groupName=groupName, tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS)),
           width = 960,height = 1040, res=140)
    }
    title <- paste0("Correlation Matrix of All Subjects")
    corNames <- CORRELATION_NAMES
    corrplot(corMatrix, p.mat=pTest$p, method = "circle", type = "lower", title = "", mar = c(1, 6, 7, 0), col = col, tl.col = "white", tl.pos='n',
             insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "black")
    text(-0.2, 9:1, corNames, cex=1.3)
    text(1:9, c(10:2) + 0.2, corNames, srt=90, cex=1.3)
    if (savePlot) {
      dev.off()
    }
  }
}

################ 4. Clustering #########################
getClusterName <- function(s, clusters) {
  sID <- str_replace(s, "Subject ", "")
  sID <- str_replace(sID, "#", "S")
  # if (str_sub(sID, 1, 1) != "#") {
  #   sID <- paste0("#", sID)
  # }
  return(paste0("C", clusters[sID]))
}


################ 5. Linear Model #######################
# Plot Linear Model for each subject
# @param p: subject ID
# @param all: data frame of all subject
# @param usePhysiological: whether using other physiological information such as heart rate, breath rate
# @returns: model
plotLinearModel <- function(p, all, window=5, usePhysiological = F) {
  pData <- getSampleSegmentedData(p, all, window)
  if (usePhysiological) {
    linearModel <- lm(ppLogNormalized ~ 
                        Speed_u + Speed_std + 
                        Acc_u + Acc_std + 
                        Brake_u + Brake_std + 
                        Steering_u + Steering_std + HR + BR, data = pData)
    # Diagnostic
    plot(linearModel, caption = paste0("Linear Model (with Physiological) of Subject #", p), which = 1)
    return(linearModel)
  } else {
    linearModel <- lm(ppLogNormalized ~ 
                        Speed_u + Speed_std + 
                        Acc_u + Acc_std + 
                        # Brake_u + Brake_std + 
                        Steering_u + Steering_std, data = pData)
    # Diagnostic
    plot(linearModel, caption = paste0("Linear Model (without Physiological) of Subject #", p), which = 1)
    return(linearModel)
  }
}

# Plot Linear Model for all subjects
# @param all: data frame of all subject
# @param usePhysiological: whether using other physiological information such as heart rate, breath rate
# @param removeIncompletedSubject: remove specific subjects who having insufficient data
# @returns: model
plotLinearModelForAllSubjects <- function(all, window=5, usePhysiological = F, removeIncompletedSubject = T) {
  linearModelAll <- NULL
  pData <- getSampleSegmentedData(NA, all, window)
  if (removeIncompletedSubject) {
    pData <- pData[!(pData$Subject %in% c("11")), ]
  }
  if (usePhysiological) {
    linearModelAll <- lmer(ppLogNormalized ~ (1 | Subject) + 
                             Speed_u + Speed_std + 
                             Acc_u + Acc_std + 
                             Brake_u + Brake_std + 
                             Steering_u + Steering_std + 
                             HR + BR, data = pData, REML = T)
  } else {
    linearModelAll <- lmer(ppLogNormalized ~ (1 | Subject) + 
                             Speed_u + Speed_std + 
                             Acc_u + Acc_std + 
                             # Brake_u + Brake_std + 
                             Steering_u + Steering_std, data = pData, REML = T)
  }
  return(linearModelAll)
}


################## 6. ML Model #########################
computePerformanceResults <- function(sdat){
  sdat = sdat[complete.cases(sdat),]
  acc = sum(sdat[,1] == sdat[,2])/nrow(sdat)
  conf_mat = table(sdat)
  specif = conf_mat[1,1]/sum(conf_mat[,1])
  sensiv = conf_mat[2,2]/sum(conf_mat[,2])
  preci =  conf_mat[2,2]/sum(conf_mat[2,])
  npv =    conf_mat[1,1]/sum(conf_mat[1,])
  return(c(acc,specif,sensiv,preci,npv))
}

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
    delay = delay
  )
}

# Draw plot for important figure
drawFeatureImportantPlot <- function(importanceDf) {
  yAxis <- list(
    title = 'Importance',
    range=c(0.0, 1.0)
  )
  xAxis <- list(
    title = 'Feature'
  )
  
  importanceDf$Feature <- factor(importanceDf$Feature, levels = importanceDf[order(-Gain),]$Feature)
  fig_Importance <- plot_ly(importanceDf, x = ~Feature, y = ~Gain, type = 'bar', name = 'Gain', width=600) %>%
    add_trace(y = ~Cover, name = 'Cover') %>% 
    add_trace(y = ~Frequency, name = 'Frequency') %>% 
    layout(yaxis = yAxis, xaxis=xAxis, barmode = 'group', title="Feature Importance") %>% 
    config(.Last.value, mathjax = 'cdn')
  orca(fig_Importance, file=paste0("./plots/importance/", p, ".png"))
}
