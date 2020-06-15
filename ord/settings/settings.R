#####################################################
# Constants                                         #
#####################################################
# Participants
SELECTED_SUBJECTS <- c("01", "04", "05", "06", "07", "08", "10", "11")

# Time-series Prediction
GROUP_THRESHOLD = 0.20
TIME_PREV_SECONDS <- 30
TIME_NEXT_SECONDS <- 5

#### Plotly 
PLOTLY_USERNAME <- "thuynh32"
PLOTLY_API_KEY <- "xcSv1yzujDc1IGEwQlr2"

Sys.setenv("plotly_username"=PLOTLY_USERNAME)
Sys.setenv("plotly_api_key"=PLOTLY_API_KEY)
