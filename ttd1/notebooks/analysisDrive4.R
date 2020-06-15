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
if(!require("processx")) {install.packages("processx")}

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

all_Drive4 <- read.csv('../../../data/TT1/preprocessed/All/TT1_Drive_4_30s_5s.csv')
all_Drive4$Subject <- as.factor(all_Drive4$Subject)

persons = c("01", "02", "03", "04", "05", "06", "07", "09",
            "12", "13", "15", "16", "17", "18",
            "22", "24", "29",
            "30", "31", "32", "41")

DRIVE_MODE = 4

for (p in persons) {
  pData <- all_Drive4[all_Drive4$Subject==as.integer(p) | all_Drive4$Subject==p,]
  dir.create(file.path('../figures/drive/', paste0('Drive_', DRIVE_MODE)), showWarnings = FALSE)
  fname <- str_interp('../figures/drive/Drive_${drive}/P${person}.svg', list(drive=DRIVE_MODE, person=p)) 
  plot_PP <- plot_ly(pData, x = ~Time, y = ~Acceleration, name = ~Subject,
                         type = 'scatter', mode = 'lines', width = 800, line=list(width=1.5)) %>% layout(title="", xaxis=list(title="Time [s]"), yaxis=list(title="Acceleration"))
  # orca(plot_PP, fname)
  plot_PP
}

