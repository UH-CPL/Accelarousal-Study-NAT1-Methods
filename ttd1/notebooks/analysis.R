### Configuration
GROUP_THRESHOLD = 0.20
TIME_PREV_SECONDS <- 30
TIME_NEXT_SECONDS <- 5
DRIVE_MODE <- 2


## Preparation
### Install packages
if(!require("plotly")) {install.packages("plotly")}

# install.packages("latex2exp")
# install.packages("BiocManager") 
# install.packages("corrplot")
# BiocManager::install("EBImage")

if(!require("lme4")){install.packages("lme4")}
if(!require("lmerTest")){install.packages("lmerTest")}
if(!require("nlme")){install.packages("nlme")}
if(!require("formattable")){install.packages("formattable")}
if(!require("xgboost")){install.packages("xgboost")}

library(plotly)
library(lme4)
library(lmerTest)
library(nlme)
library(formattable)
library(xgboost)


### Load libraries
library(EBImage)
library(ggplot2)
library(stringr)
library(gridExtra)
library(latex2exp)
packageVersion('plotly')
Sys.setenv("plotly_username"="thuynh32")
Sys.setenv("plotly_api_key"="xcSv1yzujDc1IGEwQlr2")

library(corrplot)
library(dplyr)

if(!require("RColorBrewer")) {install.packages("RColorBrewer")}
library(RColorBrewer)

if(!require('dendextend')) {install.packages('dendextend')}
library('dendextend')

if(!require('htmltools')) {install.packages('htmltools')}
if(!require('webshot')) {install.packages('webshot')}
library(htmltools)
library(webshot)
library(cluster)

### Load data
# Notes: 
# - Cannot read data of Subjects "08", "10", "23", "28" due to no Data for Drive 2 and 3
# - Temporarily eliminate #26 due to unable to compute Correlation

colorBlue = "#007fff"
colorRed = "#ff7f7f"
colorGray = "#cccccc"
colorGreen = "#11ff00"

persons = c("01", "02", "03", "04", "05", "06", "07", "09",
            "12", "13", "15", "16", "17", "18",
            "22", "24", "29",
            "30", "31", "32", "41")

# behavioralColumns <- c("Subject", "Speed (u)", "Speed (std.)", "Acc (u)", "Acc (std.)", 
#                        "Brake (u)", "Brake (std.)", "Steering (u)", "Steering (std.)")
behavioralColumns <- c("Subject", "Acc (u)", "Acc (std.)", 
                      "Steering (u)", "Steering (std.)")


getFilePath <- function(DRIVE_MODE=1, TIME_PREV_SECONDS=30, TIME_NEXT_SECONDS=5) {
  return(str_interp("../../../data/TT1/preprocessed/All/TT1_Drive_${drive}_${prevSec}s_${nextSec}s.csv", 
           list(drive=DRIVE_MODE, prevSec=TIME_PREV_SECONDS, nextSec=TIME_NEXT_SECONDS)))
}

getData <- function(DRIVE_MODE=1, TIME_PREV_SECONDS=30, TIME_NEXT_SECONDS=5) {
  filePath <- getFilePath(DRIVE_MODE, TIME_PREV_SECONDS, TIME_NEXT_SECONDS)
  all <- read.csv(filePath)
  return(all)
}

## Correlation

computeCorrelationForEachSubject <- function(all, DRIVE_MODE=1, TIME_PREV_SECONDS = 10, TIME_NEXT_SECONDS = 5, export=T) {
  behavioralMatrix <- matrix(nrow=length(persons), ncol = length(behavioralColumns))
  rowNo <- 1
  for (p in persons) {
    pData <- all[all$Subject==as.integer(p) | all$Subject==p,]
    # Correlation
    # pCorrData <- pData %>% select(ppNext, Speed, Acceleration, Braking, Steering,
    #                               Speed_u, Acc_u, Brake_u, Steering_u,
    #                               Speed_std, Acc_std, Brake_std, Steering_std)
    pCorrData <- pData %>% select(ppNext, Speed, Acceleration,  Steering,
                                  Acc_u, Steering_u,
                                  Acc_std, Steering_std)
    pCorrData$PP <- pCorrData$ppNext
    pCorrData$ppNext <- NULL
    
    pCorrData <- pCorrData[!is.na(pCorrData$PP),]
    
    col<- rev(brewer.pal(n=10, name="RdBu"))
    corMatrix <-cor(pCorrData)
    # Store to behavioral matrix
    rowCorPP <- corMatrix[nrow(corMatrix),]
    behavioralMatrix[rowNo, ] <- c(paste0("Subject #", p), 
                                    # round(rowCorPP[["Speed_u"]], digits=5),
                                    # round(rowCorPP[["Speed_std"]], digits=5),
                                    round(rowCorPP[["Acc_u"]], digits=5), 
                                    round(rowCorPP[["Acc_std"]], digits=5),
                                    # round(rowCorPP[["Brake_u"]], digits=5), 
                                    # round(rowCorPP[["Brake_std"]], digits=5), 
                                    round(rowCorPP[["Steering_u"]], digits=5), 
                                    round(rowCorPP[["Steering_std"]], digits=5)
    )
    
    # Draw
    if (export == T) {
      
      dir.create(file.path(paste0('../figures/correlation/Drive_', DRIVE_MODE, '/persons'), p), recursive = T, showWarnings = TRUE)
      fname <- str_interp('../figures/correlation/Drive_${drive}/persons/${p}/corr_Prev_${tPre}s_Next_${tNext}s.jpg', 
                          list(drive=DRIVE_MODE, tPre=TIME_PREV_SECONDS, tNext=TIME_NEXT_SECONDS, p=p)) 
      jpeg(fname)
      print(fname)
      corrplot(corMatrix, method="circle", type="lower", 
               title = paste0("Correlation Matrix of Subject #", p), 
               mar=c(0,0,4,0), col=col, tl.col="black")
      
      dev.off()
    }
    
    rowNo <- rowNo + 1
  }
  return(behavioralMatrix)
}


# All subject
computeCorrelationForAllSubject <- function(all, export=T) {
  # pCorrData <- all %>% select(ppNext, Speed, Acceleration, Braking, Steering, Speed_u, Speed_std, Acc_u, Acc_std, Brake_u, Brake_std, Steering_u, Steering_std)
  pCorrData <- all %>% select(ppNext, Speed, Acceleration, Steering, Speed_u, Speed_std, Acc_u, Acc_std, Steering_u, Steering_std)
  pCorrData$PP <- pCorrData$ppNext
  pCorrData$ppNext <- NULL
  
  pCorrData <- pCorrData[!is.na(pCorrData$PP),]
  
  col<- rev(brewer.pal(n=10, name="RdBu"))
  corMatrix <-cor(pCorrData)
  
  if (export == T) {
    corrplot(corMatrix, method="circle", type="lower", title = paste0("Correlation Matrix of All Subjects"), mar=c(0,0,4,0), col=col, tl.col="black")
  }
}



## Grouping
consolidateDrivingBehaviorData <- function(behavioralMatrix) {
  behavioralDf <- as.data.frame(behavioralMatrix)
  names(behavioralDf) <- behavioralColumns
  for (col in behavioralColumns) {
    if (col != "Subject") {
      behavioralDf[,col] <- as.numeric(as.character(behavioralDf[, col]))
    }
  }
  # head(behavioralDf)
  
  formattable(
    behavioralDf, 
    align=c("c", "r", "r", "r", "r", "r", "r", "r", "r"),
    list(
      `Subject` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
      `Speed (u)` = color_tile(colorBlue, colorRed),
      `Speed (std.)` = color_tile(colorBlue, colorRed),
      `Acc (u)` = color_tile(colorBlue, colorRed),
      `Acc (std.)` = color_tile(colorBlue, colorRed),
      # `Brake (u)` = color_tile(colorBlue, colorRed),
      # `Brake (std.)` = color_tile(colorBlue, colorRed),
      `Steering (u)` = color_tile(colorBlue, colorRed),
      `Steering (std.)` = color_tile(colorBlue, colorRed)
    ) 
  )

  return(behavioralDf)
}


doClustering <- function(behavioralDf, export=T, DRIVE_MODE=1, TIME_PREV_SECONDS=30, TIME_NEXT_SECONDS=5) {
  CLUSTER_NOS <- c(3,3,3,4)
  THRESHOLDS <- c(1.5, 1.5, 1.5, 2.0)
  
  CLUSTER_THRESHOLD <- THRESHOLDS[DRIVE_MODE]
  NUMBER_OF_CLUSTERS <- CLUSTER_NOS[DRIVE_MODE]
  CLUSTER_BRANCH_COLORS <- c("red", "blue", "#999999", colorGreen)
  CLUSTER_LABEL_COLORS <- c("red", "blue", "black", colorGreen)
  MAX_DISTANCE <- 2.5
  
  drivingDf <- behavioralDf
  drivingDf$Subject <- NULL
  
  # Eliminate unrelated factors
  drivingDf$Speed_u <- NULL
  drivingDf$Speed_std <- NULL
  # drivingDf$Brake_u <- NULL
  # drivingDf$Brake_std <- NULL
  # drivingDf$Acceleration_u <- NULL
  # drivingDf$Acceleration_std <- NULL
  # drivingDf$Steering_u <- NULL
  # drivingDf$Steering_std <- NULL
  
  behavioralMatrixClustering <- as.matrix(drivingDf)
  rownames(behavioralMatrixClustering) <- paste0("#", persons)
  distMatrix <- dist(behavioralMatrixClustering)
  hresults <- distMatrix %>% hclust
  clusters <- cutree(hresults, h=CLUSTER_THRESHOLD)
  hc <- hresults %>% as.dendrogram %>% 
    set("nodes_cex", NUMBER_OF_CLUSTERS) %>%
    set("labels_col", value = CLUSTER_LABEL_COLORS, k=NUMBER_OF_CLUSTERS) %>%
    # set("leaves_pch", 19) %>%
    # set("leaves_col", value = c("gray"), k=NUMBER_OF_CLUSTERS) %>%    
    set("branches_k_color", value=CLUSTER_BRANCH_COLORS, k=NUMBER_OF_CLUSTERS)
  
  # Store to a file
  if (export == T) {
    dir.create(file.path('../figures/clustering/', paste0('Drive_', DRIVE_MODE)), showWarnings = FALSE)
    
    # Hierachical clusterring
    fname <- str_interp('../figures/clustering/Drive_${drive}/clustering_Prev_${tPre}s_Next_${tNext}s_Dendogram.jpg', list(drive=DRIVE_MODE, tPre=TIME_PREV_SECONDS, tNext=TIME_NEXT_SECONDS)) 
    jpeg(fname)
    plot(hc,hang = -1, main=paste0("Drive=", DRIVE_MODE ,"\nHierachical Clustering (Previous ", TIME_PREV_SECONDS, "s, Next ", TIME_NEXT_SECONDS, "s)"), ylab="Distance Between Clusters", horiz = F, xlab="Subjects", ylim = c(0, MAX_DISTANCE))
    abline(h = CLUSTER_THRESHOLD, lty = 2, )
    text(length(persons)/2, CLUSTER_THRESHOLD + 0.15, paste0("Threshold = ", CLUSTER_THRESHOLD), col="orange")
    dev.off()
    
    # K-Mean
    fname <- str_interp('../figures/clustering/Drive_${drive}/clustering_Prev_${tPre}s_Next_${tNext}s_KMean.jpg', list(drive=DRIVE_MODE, tPre=TIME_PREV_SECONDS, tNext=TIME_NEXT_SECONDS)) 
    jpeg(fname)
    fit <- kmeans(drivingDf, 2)
    clusplot(drivingDf, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
    dev.off()
  }
  
  return(clusters)
}


# Grouping
getClusterName <- function(clusters, s) {
  sID <- str_replace(s, "Subject ", "")
  if (str_sub(sID[1], 1,1) != "#") {
    sID <- paste0("#", sID)
  }
  return(paste0("C", clusters[sID]))
}


drawClusteringData <- function(behavioralDf, clusters) {
  behavioralDf <- behavioralDf %>% mutate(Group=getClusterName(clusters, Subject))
  
  CLUSTER_COLORS = list(
    C1=colorRed,
    C2=colorBlue,
    C3=colorGray,
    C4=colorGreen
  )
  
  textColorformatter <- formatter("span", style = x ~ style(
    color = ifelse(x >= GROUP_THRESHOLD, colorRed, ifelse(x <= -GROUP_THRESHOLD, colorBlue, "grey"))), 
    x ~ icontext(ifelse(x >= GROUP_THRESHOLD, "arrow-up", ifelse(x <= -GROUP_THRESHOLD, "arrow-down", "")), x))
  
  ftable <- formattable(
    behavioralDf, 
    # align=c("c", "r", "r", "r", "r", "r", "r", "r", "r", "c"),
    align=c("c", "r", "r", "r", "r", "r", "r", "r", "r"),
    list(
      `Subject` = formatter("span", style = ~ style(color = "black",font.weight = "bold")),
      # `Speed (u)` = textColorformatter,
      # `Speed (std.)` = textColorformatter,
      `Acc (u)` = textColorformatter,
      `Acc (std.)` = textColorformatter,
      # `Brake (u)` = textColorformatter,
      # `Brake (std.)` = textColorformatter,
      `Steering (u)` = textColorformatter,
      `Steering (std.)` = textColorformatter,
      `Group` = formatter("span", style = x ~ style("background-color" = CLUSTER_COLORS[x], "border-radius" = "3px", "padding" = "1px 10px", font.weight = "bold"))
    ) 
  )
  return(ftable)
}

exportFormatTableToFile <- function(f, file, width = "100%", height = NULL, background = "white", delay = 0.2) {
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url, file = file, selector = ".formattable_widget", delay = delay)
}

# Export
exportBehaviorTable <- function(ftable, DRIVE_MODE=1, TIME_PREV_SECONDS=30, TIME_NEXT_SECONDS=5) {
  fname <- str_interp('../figures/correlation/Drive_${drive}/corrTable_Prev_${tPre}s_Next_${tNext}s.jpg', 
                      list(drive=DRIVE_MODE, tPre=TIME_PREV_SECONDS, tNext=TIME_NEXT_SECONDS)) 
  exportFormatTableToFile(ftable, fname)
}


processAnExperiment <- function(DRIVE_MODE=1, TIME_PREV_SECONDS=30, TIME_NEXT_SECONDS=5, export=T) {
  df <- getData(DRIVE_MODE, TIME_PREV_SECONDS, TIME_NEXT_SECONDS)
  beMat <- computeCorrelationForEachSubject(df, DRIVE_MODE = DRIVE_MODE, TIME_PREV_SECONDS = TIME_PREV_SECONDS, TIME_NEXT_SECONDS = TIME_NEXT_SECONDS, export=T)
  beDf <- consolidateDrivingBehaviorData(beMat)
  
  fname <- str_interp('../data/output/Drive_${drive}/corr_Prev_${tPre}s_Next_${tNext}s.csv', 
                      list(drive=DRIVE_MODE, tPre=TIME_PREV_SECONDS, tNext=TIME_NEXT_SECONDS)) 
  write.csv(beDf, fname, row.names = F)
  
  clusterRtn <- doClustering(beDf, export=T, DRIVE_MODE = DRIVE_MODE, TIME_PREV_SECONDS = TIME_PREV_SECONDS, TIME_NEXT_SECONDS = TIME_NEXT_SECONDS)
  ftbl <- drawClusteringData(beDf, clusterRtn)
  exportBehaviorTable(ftbl, DRIVE_MODE, TIME_PREV_SECONDS, TIME_NEXT_SECONDS)
}


# Test with diff range of time for Drive 3 and Drive 4
driving_modes <- c(1, 2, 3)
time_prevs <-c(25, 20, 15, 10, 5)
time_nexts <- c(10)

for (drv_mode in driving_modes) {
  for(t_prev in time_prevs) {
    for(t_next in time_nexts) {
      print(paste0("Processing Drive=", drv_mode, " (Prev=", t_prev, "s, Next=", t_next, "s)"))
      processAnExperiment(DRIVE_MODE = drv_mode, TIME_PREV_SECONDS = t_prev, TIME_NEXT_SECONDS = t_next)
    }
  }
}

### Draw all PP

# plot_all_PP <- plot_ly(all, x = ~Time, y = ~ppNext, name = ~Subject, 
#                        type = 'scatter', mode = 'lines', width = 800, line=list(width=1.5),
#                        color = ~Subject) %>% layout(title="", xaxis=list(title="Time [s]"), yaxis=list(title=paste0("Avg. Persperation (Next ", TIME_NEXT_SECONDS, " seconds)")))


### Draw all Speed

# plot_all_Speed <- plot_ly(all, x = ~Time, y = ~Speed_u, name = ~Subject, 
#                           type = 'scatter', mode = 'lines', width = 800, line=list(width=1.5),
#                           color = ~Subject) %>% layout(title="", xaxis=list(title="Time [s]"), yaxis=list(title=paste0("Avg. Speed (Last ", TIME_PREV_SECONDS," seconds)")))




