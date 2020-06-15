###################### IMPORT COMMON FUNCTIONS ################
source("./settings/settings.R")
source("./scripts/commonFunctions.R")
DEBUG_MODE <- F

###################### WORKING DIRECTORY ######################
# setwd('./')
print(paste0("Working directory: ", getwd()))

###################### LIBRARY LOADING ########################
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

###################### VARIABLE DECLAREATION ##################

persons <- SELECTED_SUBJECTS
datas <- vector(mode = "list", length = length(persons))
datas_baseline <- vector(mode = "list", length = length(persons))
pp_means <- vector(mode = "list", length = length(persons))

names(datas_baseline) <- persons
names(pp_means) <- persons
names(datas) <- persons

extra <- 0.0000000001
all <- data.frame()
all_baseline <- data.frame()

###################### DATA LOADING ###########################
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

if (DEBUG_MODE) {
  print(length(all$Time))
  print(length(all_baseline$Time))
}

####################### PROCESS ###########################
### Process the temporal information
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
    for (i in TIME_PREV_SECONDS:n) {
      sfrom <- i - TIME_PREV_SECONDS
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
    for (i in 1:(n - TIME_NEXT_SECONDS)) {
      sfrom <- i + 1
      sto <- i + TIME_NEXT_SECONDS
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

if (DEBUG_MODE) {
  print(length(all$Time))
}

#################### CORRELLATION ###########################
behavioralColumns <- c("Subject", "Speed (u)", "Speed (std.)", "Acc (u)", "Acc (std.)", "Brake (u)", "Brake (std.)", "Steering (u)", "Steering (std.)")
behavioralMatrix <- matrix(nrow = length(persons), ncol = length(behavioralColumns))

plotCorrelation <- function(p, rowNo = 1) {
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
  corrplot(corMatrix, method = "circle", type = "lower", title = paste0("Correlation Matrix of Subject #", p), mar = c(0, 0, 4, 0), col = col, tl.col = "black")
}

### 1. Correlation of all Subjects
# All subject
pCorrData <- all %>% select(ppNext, Speed, Accelerator, Brake, HR, BR, Steering, Speed_u, Speed_std, Acc_u, Acc_std, Brake_u, Brake_std, Steering_u, Steering_std)
pCorrData$PP <- pCorrData$ppNext
pCorrData$ppNext <- NULL

pCorrData <- pCorrData[!is.na(pCorrData$PP), ]

col <- rev(brewer.pal(n = 10, name = "RdBu"))
corMatrix <- cor(pCorrData)
corrplot(corMatrix, method = "circle", type = "lower", title = paste0("Correlation Matrix of All Subjects"), mar = c(0, 0, 4, 0), col = col, tl.col = "black")

### 2. Correlation of each Subject
for (p in persons) {
  plotCorrelation(p, rowNo = match(p, persons))
}

######################### CLUSTERING ##############################
## Grouping
behavioralDf <- as.data.frame(behavioralMatrix)
names(behavioralDf) <- behavioralColumns
for (col in behavioralColumns) {
  if (col != "Subject") {
    behavioralDf[,col] <- as.numeric(as.character(behavioralDf[, col]))
  }
}
head(behavioralDf)

colorBlue = "#007fff"
colorRed = "#ff7f7f"
colorGray = "#cccccc"

formattable(
  behavioralDf, 
  align=c("c", "r", "r", "r", "r", "r", "r", "r", "r"),
  list(
    `Subject` = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")),
    `Speed (u)` = color_tile(colorBlue, colorRed),
    `Speed (std.)` = color_tile(colorBlue, colorRed),
    `Acc (u)` = color_tile(colorBlue, colorRed),
    `Acc (std.)` = color_tile(colorBlue, colorRed),
    `Brake (u)` = color_tile(colorBlue, colorRed),
    `Brake (std.)` = color_tile(colorBlue, colorRed),
    `Steering (u)` = color_tile(colorBlue, colorRed),
    `Steering (std.)` = color_tile(colorBlue, colorRed)
  ) 
)

### Hierachical Clustering & Grouping
CLUSTER_THRESHOLD <- 1.0
NUMBER_OF_CLUSTERS <- 2
CLUSTER_BRANCH_COLORS <- c("red", "blue", "#999999")
CLUSTER_LABEL_COLORS <- c("red", "blue", "black")

behavioralMatrixClustering <- as.matrix(behavioralDf)
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
fname <- str_interp('./plots/clustering/clustering_Prev_${tPre}s_Next_${tNext}s.jpg', list(tPre=TIME_PREV_SECONDS, tNext=TIME_NEXT_SECONDS)) 
jpeg(fname)
plot(hc,hang = -1, main=paste0("Hierachical Clustering (Previous ", TIME_PREV_SECONDS, "s, Next ", TIME_NEXT_SECONDS, "s)"), ylab="Distance Between Clusters", horiz = F, xlab="Subjects", ylim = c(0, 1.4))
abline(h = CLUSTER_THRESHOLD, lty = 2, )
text(4, CLUSTER_THRESHOLD + 0.05, paste0("Threshold = ", CLUSTER_THRESHOLD), col="orange")
dev.off()

# Print-out
plot(hc,hang = -1, main=paste0("Hierachical Clustering (Previous ", TIME_PREV_SECONDS, "s, Next ", TIME_NEXT_SECONDS, "s)"), ylab="Distance Between Clusters", horiz = F, xlab="Subjects", ylim = c(0, 1.4))
abline(h = CLUSTER_THRESHOLD, lty = 2, )
text(4, CLUSTER_THRESHOLD + 0.05, paste0("Threshold = ", CLUSTER_THRESHOLD), col="orange")

# Grouping
getClusterName <- function(s) {
  sID <- str_replace(s, "Subject ", "")
  if (str_sub(sID, 1,1) != "#") {
    sID <- paste0("#", sID)
  }
  return(paste0("C", clusters[sID]))
}
behavioralDf <- behavioralDf %>% mutate(Group=getClusterName(Subject))

CLUSTER_COLORS = list(
  C1=colorBlue,
  C2=colorRed,
  C3=colorRed
)
textColorformatter <- formatter("span", style = x ~ formattable::style(
  color = ifelse(x >= GROUP_THRESHOLD, colorRed, ifelse(x <= -GROUP_THRESHOLD, colorBlue, "grey"))), 
  x ~ icontext(ifelse(x >= GROUP_THRESHOLD, "arrow-up", ifelse(x <= -GROUP_THRESHOLD, "arrow-down", "")), x))

ftable <- formattable(
  behavioralDf, 
  # align=c("c", "r", "r", "r", "r", "r", "r", "r", "r", "c"),
  align=c("c", "r", "r", "r", "r", "r", "r", "r", "r"),
  list(
    `Subject` = formatter("span", style = ~ formattable::style(color = "black",font.weight = "bold")),
    `Speed (u)` = textColorformatter,
    `Speed (std.)` = textColorformatter,
    `Acc (u)` = textColorformatter,
    `Acc (std.)` = textColorformatter,
    `Brake (u)` = textColorformatter,
    `Brake (std.)` = textColorformatter,
    `Steering (u)` = textColorformatter,
    `Steering (std.)` = textColorformatter,
    `Group` = formatter("span", style = x ~ formattable::style("background-color" = CLUSTER_COLORS[x], "border-radius" = "3px", "padding" = "1px 10px", font.weight = "bold"))
  ) 
)
ftable

### Save table
exportFormatTable <- function(f, file, width = "100%", height = NULL, 
                              background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

# Export
fname <- str_interp('./plots/correllation/corrTable_Prev_${tPre}s_Next_${tNext}s.jpg', list(tPre=TIME_PREV_SECONDS, tNext=TIME_NEXT_SECONDS)) 
exportFormatTable(ftable, fname)


