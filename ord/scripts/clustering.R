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
res <- getResponseAbbr(RESPONSE_VAR)

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
behavioralMatrixWithPValue <- matrix(nrow = length(persons), ncol = length(behavioralColumns))

###################### 1. Correlation of all Subjects ###################
computeAndPlotCorrelationOfAllSubjects(all, window=TIME_PREV_SECONDS, skipPlot = T)

###################### 2. Correlation of each Subject ####################
for (p in persons) {
  computeAndPlotCorrelation(p, all, behavioralMatrix, behavioralMatrixWithPValue, response=RESPONSE_VAR, window=TIME_PREV_SECONDS, rowNo = match(p, persons), skipPlot = T)
}
print(behavioralMatrixWithPValue)
######################### CLUSTERING ##############################
## Grouping
behavioralDf <- as.data.frame(behavioralMatrix)
behavioralDfWithPValue <- as.data.frame(behavioralMatrixWithPValue)

names(behavioralDf) <- behavioralColumns
names(behavioralDfWithPValue) <- behavioralColumns
for (col in behavioralColumns) {
  if (col != "Subject") {
    behavioralDf[, col] <- as.numeric(as.character(behavioralDf[, col]))
  }
}
clusteringDf <- behavioralDf
# head(behavioralDf)

formattable(
  behavioralDf,
  align = c("c", "r", "r", "r", "r", "r", "r", "r", "r"),
  list(
    `Subject` = formatter("span", style = ~ formattable::style(color = "grey", font.weight = "bold")),
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
behavioralMatrixClustering <- as.matrix(behavioralDf %>% select(-Subject))
rownames(behavioralMatrixClustering) <- paste0("S", persons)
distMatrix <- dist(behavioralMatrixClustering)
hresults <- distMatrix %>% hclust()
clusters <- cutree(hresults, h = CLUSTER_THRESHOLD)

hc <- hresults %>%
  as.dendrogram(hang = -1) %>%
  set("nodes_cex", NUMBER_OF_CLUSTERS) %>%
  set("labels_col", value = CLUSTER_LABEL_COLORS, k = NUMBER_OF_CLUSTERS) %>%
  # set("leaves_pch", 19) %>%
  # set("leaves_col", value = c("gray"), k=NUMBER_OF_CLUSTERS) %>%
  set("branches_k_color", value = CLUSTER_BRANCH_COLORS, k = NUMBER_OF_CLUSTERS)

# Store to a file
plotTitle <- paste0("Hierachical Clustering (Previous ", TIME_PREV_SECONDS, "s, Next ", TIME_NEXT_SECONDS, "s)")
fname <- str_interp("./plots/clustering/${res}/clustering_Prev_${tPre}s_Next_${tNext}s.jpg", list(res=res, tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS))
jpeg(fname, width=560, height=720, res=144)
plot(hc, main = "", ylab = "Distance Between Clusters", horiz = F, xlab = "Subject", ylim = c(0, 1.4), xaxs="i", yaxs="i")
abline(h = CLUSTER_THRESHOLD, lty = 2, )
text(4, CLUSTER_THRESHOLD + 0.075, paste0("Threshold = ", CLUSTER_THRESHOLD), col = "black")
dev.off()

# Print-out
plot(hc, main = plotTitle, ylab = "Distance Between Clusters", horiz = F, xlab = "Subject", ylim = c(0, 1.4))
abline(h = CLUSTER_THRESHOLD, lty = 2, )
text(4.1, CLUSTER_THRESHOLD + 0.075, paste0("Threshold = ", CLUSTER_THRESHOLD), col = "orange")

# Grouping
behavioralDf <- behavioralDf %>% mutate(Group = getClusterName(Subject, clusters))
behavioralDfWithPValue <- behavioralDfWithPValue %>% mutate(Group = getClusterName(Subject, clusters))

CLUSTER_COLORS <- list(
  C1 = colorBlue,
  C2 = colorRed,
  C3 = colorRed
)
textColorformatter <- formatter("span",
  style = x ~ formattable::style(
    color = ifelse(isSignificant(x), ifelse(getValue(x) > 0, colorRed, colorBlue), "grey")
  ),
  x ~ icontext(ifelse(isSignificant(x), ifelse(getValue(x) > 0, "arrow-up", "arrow-down"), ""), x)
)

ftable <- formattable(
  behavioralDfWithPValue,
  # align=c("c", "r", "r", "r", "r", "r", "r", "r", "r", "c"),
  align = c("c", "r", "r", "r", "r", "r", "r", "r", "r"),
  list(
    `Subject` = formatter("span", style = ~ formattable::style(color = "black", font.weight = "bold")),
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

# Export
fname <- str_interp("./plots/correlation/${res}/corrTable_Prev_${tPre}s_Next_${tNext}s.jpg", list(res=res, tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS))
exportFormatTable(ftable, fname)

# Export to CSV
csvFname <- str_interp("./outputs/correlation/${res}/corrTable_Prev_${tPre}s_Next_${tNext}s.csv", list(res=res, tPre = TIME_PREV_SECONDS, tNext = TIME_NEXT_SECONDS))
write.csv(behavioralDf, csvFname)

# Evaluate Clustering method
k <- 2:7
clusteringDf$Subject <- NULL

# 1. Silhouette
silhouette_score <- function(k){
  km <- kmeans(clusteringDf, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(clusteringDf))
  mean(ss[, 3])
}
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)



# 2. Elbow
# Function to compute total within-cluster sum of square 
library(purrr)
wss <- function(k) {
  kmeans(clusteringDf, k, nstart = 10 )$tot.withinss
}
wss_values <- map_dbl(k, wss)
plot(k, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



