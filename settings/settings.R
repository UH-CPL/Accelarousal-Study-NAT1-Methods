#####################################################
# Constants                                         #
#####################################################
# Participants
SELECTED_SUBJECTS <- c("01", "02", "03", "04", "05", "06", "07", "08", "10", "11", "12")

# Time-series Prediction
GROUP_THRESHOLD <- 0.20
TIME_PREV_SECONDS <- 20
TIME_NEXT_SECONDS <- 5

# Clipped Distance
CLIPPED_DISTANCE <- -250
STARTING_DISTANCE <- list("01"=-375, "02"=-100, 
                          "03"=-85, "04"=-90, 
                          "05"=-75, "06"=-60, 
                          "07"=-75, "08"=-85, 
                          "10"=-65, "11"=-65, 
                          "12"=-95)
ENDING_DISTANCE <- 22000
# Response
RESPONSE_VAR <- "ppNext" # Accept: "ppNext" "HRNext", "BRNext"

# Analysis
BEHAVIORAL_COLUMNS <- c(
  "Subject", 
  "Speed (u)", "Speed (std.)", 
  "Acc (u)", "Acc (std.)", 
  # "Jerk (u)", "Jerk (std.)",
  "Brake (u)", "Brake (std.)", 
  "Steering (u)", "Steering (std.)"
)
CORRELATION_NAMES <- c(TeX("$\\bar{x}_{speed}$"), TeX("$\\s_{speed}$"),
                          TeX("$\\bar{x}_{accel}$"), TeX("$\\s_{accel}$"),
                          # TeX("$\\bar{x}_{jerk}$"), TeX("$\\s_{jerk}$"),
                          TeX("$\\bar{x}_{brake}$"), TeX("$\\s_{brake}$"),
                          TeX("$\\bar{x}_{steer}$"), TeX("$\\s_{steer}$"),
                          TeX("$\\textbf{_{Arousal}}$"))

CORRELATION_NAMES_PP <- c(TeX("$\\bar{x}_{speed}$"), TeX("$\\s_{speed}$"),
                          TeX("$\\bar{x}_{accel}$"), TeX("$\\s_{accel}$"),
                          # TeX("$\\bar{x}_{jerk}$"), TeX("$\\s_{jerk}$"),
                          TeX("$\\bar{x}_{brake}$"), TeX("$\\s_{brake}$"),
                          TeX("$\\bar{x}_{steer}$"), TeX("$\\s_{steer}$"),
                          TeX("$\\textbf{_{Arousal}}$"))

CORRELATION_NAMES_HR <- c(TeX("$\\bar{x}_{speed}$"), TeX("$\\s_{speed}$"),
                          TeX("$\\bar{x}_{accel.}$"), TeX("$\\s_{accel.}$"),
                          # TeX("$\\bar{x}_{jerk}$"), TeX("$\\s_{jerk}$"),
                          TeX("$\\bar{x}_{brake}$"), TeX("$\\s_{brake}$"),
                          TeX("$\\bar{x}_{steering}$"), TeX("$\\s_{steering}$"),
                          TeX("$\\textbf{_{HR}}$"))

CORRELATION_NAMES_BR <- c(TeX("$\\bar{x}_{speed}$"), TeX("$\\s_{speed}$"),
                          TeX("$\\bar{x}_{accel.}$"), TeX("$\\s_{accel.}$"),
                          # TeX("$\\bar{x}_{jerk}$"), TeX("$\\s_{jerk}$"),
                          TeX("$\\bar{x}_{brake}$"), TeX("$\\s_{brake}$"),
                          TeX("$\\bar{x}_{steering}$"), TeX("$\\s_{steering}$"),
                          TeX("$\\textbf{_{BR}}$"))


LINEAR_MODEL_VARIABLES_LATEX <- c("$\\bar{x}_{speed}$", "$\\s_{speed}$",
                                  "$\\bar{x}_{accel.}$", "$\\s_{accel.}$",
                                  "$\\bar{x}_{brake}$", "$\\s_{brake}$",
                                  "$\\bar{x}_{steering}$", "$\\s_{steering}$")


# Clustering
CLUSTER_THRESHOLDS <- list("3"=0.85, "5"=0.85, "10"=0.95, "15"=1.00, "20"=1.00, "30"=1.10)
CLUSTER_THRESHOLD <- as.double(CLUSTER_THRESHOLDS[as.character(TIME_PREV_SECONDS)][1])
NUMBER_OF_CLUSTERS <- 2
CLUSTER_BRANCH_COLORS <- c("red", "blue", "#999999")
CLUSTER_LABEL_COLORS <- c("red", "blue", "black")

# ML
FEATURE_SELECTION_OPTION <- "WithoutBrake" # All

# Plotly
PLOTLY_USERNAME <- "<Plotly Username>"
PLOTLY_API_KEY <- "<Plotly API Key>"

Sys.setenv("plotly_username" = PLOTLY_USERNAME)
Sys.setenv("plotly_api_key" = PLOTLY_API_KEY)

# XTable
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# Colors
colorBlue <- "#007fff"
colorRed <- "#ff7f7f"
colorGray <- "#cccccc"

# Mapbox
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoidGh1eW5oMzIiLCJhIjoiY2p6endwbXY3MjJ0cTNtbWwzN3E0enZuciJ9.fpzzkDjWHb0LOcTog8B6Lg')

