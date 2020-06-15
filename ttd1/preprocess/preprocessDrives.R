library(rjson)
library(tidyverse)

source("utils/downsample.R")
source("utils/denoise.R")

persons = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
            "12", "13", "15", "16", "17", "18",
            "22", "23", "24", "26", "28", "29", 
            "30", "31", "32", "41")

drives = c(1,2,3,4)
# decay_wave_lengths = c(2.5, 5, 7.5, 10, 25, 35, 50, 75)

## Driving data
dataOutDir = "./data/processed/drives/"

for (p in persons) {
  print(paste0("Subject #", p))
  df <- read.csv(str_interp("./data/processed/combined/T0${person}.csv", list(person=p)))
  
  for (d in drives) {
    print(paste0("- Drive: ", d))
    df_drive <- df[df$Drive==d,]
    df_drive <- subset(df_drive, select=c("Time", "Perspiration", "Speed",
                              "Acceleration", "Braking", "Steering", 
                              "Phase", "Activity", "Failure", "Distance"))

    df_downresample <- downsample_using_mean(df_drive, c("Perspiration", "Speed",
                                                         "Acceleration", "Braking", "Steering", 
                                                         "Phase", "Activity", "Failure", "Distance"))
    
    
    
    if (d == 4) {
      df_out <- df_downresample
    } else {
      df_out <- df_downresample
      # df_out <- df_downresample[df_downresample$Phase != 0 & df_downresample$Activity != 0,]
    }
    
    subDir = str_interp("T0${person}", list(person=p))
    filePath = str_interp("./data/processed/drives/T0${person}/T0${person}_Drive_${drive}.csv", list(person=p, drive=d))
    dir.create(file.path(dataOutDir, subDir), showWarnings = FALSE)
    print(paste0("- Writing: ", nrow(df_out), " rows"))
    write.csv(df_out, filePath, row.names = FALSE)
  }
}
