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
CORRELATION_NAMES <- c(TeX("$\\bar{x}_{Speed}$"), TeX("$\\s_{Speed}$"),
                          TeX("$\\bar{x}_{Accel.}$"), TeX("$\\s_{Accel.}$"),
                          TeX("$\\bar{x}_{Brake}$"), TeX("$\\s_{Brake}$"),
                          TeX("$\\bar{x}_{Steering}$"), TeX("$\\s_{Steering}$"),
                          TeX("$\\textbf{_{Arousal}}$"))

CORRELATION_NAMES_PP <- c(TeX("$\\bar{x}_{Speed}$"), TeX("$\\s_{Speed}$"),
                          TeX("$\\bar{x}_{Accel.}$"), TeX("$\\s_{Accel.}$"),
                          TeX("$\\bar{x}_{Brake}$"), TeX("$\\s_{Brake}$"),
                          TeX("$\\bar{x}_{Steering}$"), TeX("$\\s_{Steering}$"),
                          TeX("$\\textbf{_{Arousal}}$"))

CORRELATION_NAMES_HR <- c(TeX("$\\bar{x}_{Speed}$"), TeX("$\\s_{Speed}$"),
                          TeX("$\\bar{x}_{Accel.}$"), TeX("$\\s_{Accel.}$"),
                          TeX("$\\bar{x}_{Brake}$"), TeX("$\\s_{Brake}$"),
                          TeX("$\\bar{x}_{Steering}$"), TeX("$\\s_{Steering}$"),
                          TeX("$\\textbf{_{HR}}$"))

CORRELATION_NAMES_BR <- c(TeX("$\\bar{x}_{Speed}$"), TeX("$\\s_{Speed}$"),
                          TeX("$\\bar{x}_{Accel.}$"), TeX("$\\s_{Accel.}$"),
                          TeX("$\\bar{x}_{Brake}$"), TeX("$\\s_{Brake}$"),
                          TeX("$\\bar{x}_{Steering}$"), TeX("$\\s_{Steering}$"),
                          TeX("$\\textbf{_{BR}}$"))


LINEAR_MODEL_VARIABLES_LATEX <- c("$\\bar{x}_{Speed}$", "$\\s_{Speed}$",
                                  "$\\bar{x}_{Accel.}$", "$\\s_{Accel.}$",
                                  "$\\bar{x}_{Brake}$", "$\\s_{Brake}$",
                                  "$\\bar{x}_{Steering}$", "$\\s_{Steering}$")

# Time-series Prediction
GROUP_THRESHOLD <- 0.20
TIME_PREV_SECONDS <- 20
TIME_NEXT_SECONDS <- 5

# Response
RESPONSE_VAR <- "ppNext" # Accept: "ppNext" "HRNext", "BRNext"

# Clustering
CLUSTER_THRESHOLDS <- list("3"=0.9, "5"=0.90, "10"=0.95, "15"=1.00, "30"=1.10)
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
