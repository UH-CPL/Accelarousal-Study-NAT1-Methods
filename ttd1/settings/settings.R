#####################################################
# Constants                                         #
#####################################################
# Partitcipants
# SELECTED_SUBJECTS <- c("01", "04", "05", "06", "07", "08", "10", "11", "12") 
# Notes: Cannot read data of Subjects "08", "10", "23", "28" due to no Data for Drive 2 and 3
SELECTED_SUBJECTS <- c("01", "02", "03", "04", "05", "06", "07", "09", "12", "13", "15", "16", "17", "18", "22", "24", "29", "30", "31", "32", "41")

# Plotly
PLOTLY_USERNAME <- "thuynh32"
PLOTLY_API_KEY <- "xcSv1yzujDc1IGEwQlr2"

Sys.setenv("plotly_username" = PLOTLY_USERNAME)
Sys.setenv("plotly_api_key" = PLOTLY_API_KEY)