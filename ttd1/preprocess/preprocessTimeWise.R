###################### WORKING DIRECTORY ###############################
# setwd('./')
print(paste0("Working directory: ", getwd()))

###################### IMPORT COMMON LIBRARIES/ FUNCTIONS ##############
source("settings/settings.R")
source("preprocess/commonFunctions.R")
DEBUG_MODE <- F

###################### DATA LOADING ####################################
persons <- SELECTED_SUBJECTS
timeXD4 <- c(62, 81, 70, 69, 71, 62, 69, 73, 64, 60, 63, 63, 74, 65, 76, 64, 68, 69, 65, 65, 61)

processTT1Data <- function(DRIVE_MODE = 1, TIME_PREV_SECONDS = 30, TIME_NEXT_SECONDS = 5, export = TRUE) {
  datas <- vector(mode = "list", length = length(persons))
  datas_baseline <- vector(mode = "list", length = length(persons))
  pp_means <- vector(mode = "list", length = length(persons))
  pp_plots <- vector(mode = "list", length = length(persons))

  names(datas) <- persons
  names(datas_baseline) <- persons
  names(pp_means) <- persons

  for (p in persons) {
    print(paste0("Reading data of subject #", p))
    datas[[p]] <- read.csv(str_interp("./data/processed/drives/T0${person}/T0${person}_Drive_${drive}.csv", list(person = p, drive = DRIVE_MODE)))
    datas_baseline[[p]] <- read.csv(str_interp("./data/processed/drives/T0${person}/T0${person}_Drive_1.csv", list(person = p)))

    # Compute the mean
    p_data <- datas_baseline[[p]]
    p_pp_nr <- p_data[p_data$Phase != 0 & p_data$Activity != 0, ]$Perspiration
    p_pp_nr <- p_pp_nr[!is.na(p_pp_nr)]
    pp_means[[p]] <- mean(p_pp_nr)
  }

  ### Merge PP Data
  extra <- 0.0000000001

  all <- data.frame()
  all_baseline <- data.frame()
  pIdx <- 1
  for (p in persons) {
    # On-road
    df_p <- datas[[p]]

    if (DRIVE_MODE == 4) {
      fromTime <- timeXD4[pIdx] - 30
      df_p <- df_p[df_p$Time >= fromTime, ]
    }

    print(paste("Length of #", p, " - Time: ", nrow(df_p)))

    df_p$ppNormalized <- df_p$Perspiration - pp_means[[p]]
    df_p$ppLogNormalized <- log(df_p$Perspiration + extra) - log(pp_means[[p]] + extra)
    df_p$Subject <- p

    all <- rbind(all, df_p)

    # Baseline
    df_p_baseline <- datas_baseline[[p]]
    df_p_baseline$Subject <- p
    all_baseline <- rbind(all_baseline, df_p_baseline)

    pIdx <- pIdx + 1
  }
  print(length(all$Time))
  print(length(all_baseline$Time))


  ### Common functions
  calculateMean <- function(l) {
    return(mean(l, na.rm = T))
  }

  calculateStd <- function(l) {
    return(sd(l, na.rm = T))
  }


  ### Process the temporal information


  temp_all <- all
  all <- data.frame()
  for (p in persons) {
    # On-road
    p_data <- temp_all[temp_all$Subject == p, ]
    minTime <- min(p_data$Time)
    maxTime <- max(p_data$Time)

    n <- nrow(p_data)

    if (n > 0) {
      # Add Speed_u, Speed_std, Acc_u, Acc_std, Brake_u, Brake_std, Steering_u, Steering_std
      p_data$Speed_u <- rep(NA, n)
      p_data$Speed_std <- rep(NA, n)
      p_data$Acc_u <- rep(NA, n)
      p_data$Acc_std <- rep(NA, n)

      p_data$Braking <- NULL
      # p_data$Brake_u <- rep(NA, n)
      # p_data$Brake_std <- rep(NA, n)

      p_data$Steering_u <- rep(NA, n)
      p_data$Steering_std <- rep(NA, n)

      p_data$ppNext <- rep(NA, n)

      # Driving stat info of prev seconds
      for (i in 2:n) {
        ti <- p_data$Time[i]
        sfrom <- ifelse(ti - TIME_PREV_SECONDS > 0, ti - TIME_PREV_SECONDS, 1)
        sto <- ti

        prev_data <- p_data[p_data$Time >= sfrom & p_data$Time < sto, ]

        if (nrow(prev_data) > 0) {
          p_data$Speed_u[i] <- calculateMean(prev_data$Speed)
          p_data$Acc_u[i] <- calculateMean(prev_data$Acceleration)
          # p_data$Brake_u[i] <- calculateMean(prev_data$Braking)
          p_data$Steering_u[i] <- calculateMean(prev_data$Steering)

          p_data$Speed_std[i] <- calculateStd(prev_data$Speed)
          p_data$Acc_std[i] <- calculateStd(prev_data$Acceleration)
          # p_data$Brake_std[i] <- calculateStd(prev_data$Braking)
          p_data$Steering_std[i] <- calculateStd(prev_data$Steering)
        }
      }

      # PP of next seconds
      for (i in 1:n) {
        ti <- p_data$Time[i]
        sfrom <- ti
        sto <- ti + TIME_NEXT_SECONDS
        next_data <- p_data[p_data$Time >= sfrom & p_data$Time <= sto, ]

        if (nrow(next_data) > 0) {
          p_data$ppNext[i] <- calculateMean(next_data$ppLogNormalized)
        }
      }

      all <- rbind(all, p_data)
    }
  }

  # Remove NA value
  all <- all[!is.na(all$Speed_u) & !is.na(all$Speed_std)
  & !is.na(all$Acc_u) & !is.na(all$Acc_std)
  # & !is.na(all$Brake_u) & !is.na(all$Brake_std)
  & !is.na(all$Steering_u) & !is.na(all$Steering_std)
  & !is.na(all$ppNext), ]

  print(length(all$Time))

  # Export to CSV
  if (export) {
    fPath <- str_interp(
      "./data/processed/timewise/TT1_Drive_${drive}_${prevSec}s_${nextSec}s.csv",
      list(
        drive = DRIVE_MODE, prevSec = TIME_PREV_SECONDS,
        nextSec = TIME_NEXT_SECONDS
      )
    )
    write.csv(all, fPath, row.names = FALSE)
  }
  return(all)
}

processTT1DataWithAllDrivingModes <- function() {
  modes <- c(1, 2, 3, 4)
  prevSec <- c(5, 10, 15, 20, 25)
  nextSec <- c(5, 10)

  for (mode in modes) {
    for (ps in prevSec) {
      for (ns in nextSec) {
        processTT1Data(DRIVE_MODE = mode, TIME_PREV_SECONDS = ps, TIME_NEXT_SECONDS = ns)
      }
    }
  }
}

processTT1DataWithAllDrivingModes()
