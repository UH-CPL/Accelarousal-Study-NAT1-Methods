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

# Time-series Prediction
GROUP_THRESHOLD <- 0.20
TIME_PREV_SECONDS <- 30
TIME_NEXT_SECONDS <- 5

# Clustering
CLUSTER_THRESHOLD <- 1.0
NUMBER_OF_CLUSTERS <- 2
CLUSTER_BRANCH_COLORS <- c("red", "blue", "#999999")
CLUSTER_LABEL_COLORS <- c("red", "blue", "black")

# Plotly
PLOTLY_USERNAME <- "<Plotly Username>"
PLOTLY_API_KEY <- "<Plotly API Key>"

Sys.setenv("plotly_username" = PLOTLY_USERNAME)
Sys.setenv("plotly_api_key" = PLOTLY_API_KEY)

# Colors
colorBlue <- "#007fff"
colorRed <- "#ff7f7f"
colorGray <- "#cccccc"
