#####################################################
# Constants                                         #
#####################################################
# Participants
SELECTED_SUBJECTS <- c("01", "04", "05", "06", "07", "08", "10", "11")

# Analysis
BEHAVIORAL_COLUMNS <- c(
  "Subject", "Speed (u)", "Speed (std.)", "Acc (u)", "Acc (std.)",
  "Brake (u)", "Brake (std.)", "Steering (u)", "Steering (std.)"
)
CORRELATION_NAMES <- c(TeX("$\\mu_{Speed}$"), TeX("$\\sigma_{Speed}$"),
                       TeX("$\\mu_{Accel.}$"), TeX("$\\sigma_{Accel.}$"),
                       TeX("$\\mu_{Brake}$"), TeX("$\\sigma_{Brake}$"),
                       TeX("$\\mu_{Steering}$"), TeX("$\\sigma_{Steering}$"),
                       TeX("$\\textbf{_{Arousal}}$"))
LINEAR_MODEL_VARIABLES_LATEX <- c("$\\mu_{Speed}$", "$\\sigma_{Speed}$",
                                  "$\\mu_{Accel.}$", "$\\sigma_{Accel.}$",
                                  "$\\mu_{Brake}$", "$\\sigma_{Brake}$",
                                  "$\\mu_{Steering}$", "$\\sigma_{Steering}$")

# Time-series Prediction
GROUP_THRESHOLD <- 0.20
TIME_PREV_SECONDS <- 5
TIME_NEXT_SECONDS <- 5

# Clustering
CLUSTER_THRESHOLDS <- list("3"=1, "5"=1, "10"=1.05, "15"=1.10, "30"=1.15)
CLUSTER_THRESHOLD <- as.double(CLUSTER_THRESHOLDS[as.character(TIME_PREV_SECONDS)][1])
NUMBER_OF_CLUSTERS <- 2
CLUSTER_BRANCH_COLORS <- c("red", "blue", "#999999")
CLUSTER_LABEL_COLORS <- c("red", "blue", "black")

# Plotly
PLOTLY_USERNAME <- "thuynh32"
PLOTLY_API_KEY <- "xcSv1yzujDc1IGEwQlr2"

Sys.setenv("plotly_username" = PLOTLY_USERNAME)
Sys.setenv("plotly_api_key" = PLOTLY_API_KEY)

# XTable
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# Colors
colorBlue <- "#007fff"
colorRed <- "#ff7f7f"
colorGray <- "#cccccc"
