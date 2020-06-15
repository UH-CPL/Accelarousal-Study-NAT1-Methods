# Test Track Driving Study 1

## Getting Started
### Prerequisites
- R and RStudio
- Required packages

### Installing R Packages
To install required R packages, run script `setup.R` in each study folder.

## Folder Structure
- `data`: Raw and processed data.
- `plots`: Output plots of correlation matrices, clustering dendrogram, linear models, machine learning models.
- `outputs`: CSV/Text outputs.
- `preprocess`: Preprocessing scripts.
- `settings`: Common setting of the project.
- `scripts`: Main script for analysis and data modeling.
- `utils`: Common ultility functions.

## Configuration
All global settings are placed in `settings/settings.R`.

### Plotly
As the project is using Plotly as a visualization tool, you can set up you Plotly account with follow constants.
```R
PLOTLY_USERNAME <- "<Your Plotly Username>"
PLOTLY_API_KEY <- "<Your Plotly API Key>"
```

## Preprocessing Script Set
Run following scripts sequentially.
### 1. Process and disjoin data of drives
Run `preprocess/preprocessDrives.R`.
### 2. Merge data
- Time-wise preprocessing
Run `preprocess/preprocessTimeWise.R`.
- Distance-wise preprocessing
Run `preprocess/preprocessDistanceWise.R`.

## Main Script Set
### 1. Drive 1 (No stressor) Analysis
Run the notebook `notebooks/analysisDrive1_Distance.Rmd`.
### 2. Drive 2 (Cognitive stressor) Analysis
Run the notebook `notebooks/analysisDrive2_Distance.Rmd`.
### 3. Drive 3 (No stressor) Analysis
Run the notebook `notebooks/analysisDrive3_Distance.Rmd`.
### 4. Drive 4 (Unintended acceleration event at the end) Analysis
This drive is aka `Failure Drive`.  
Run the notebook `notebooks/analysisDrive4_Distance.Rmd`.

## Additional Tools
### Style Guide
http://jef.works/R-style-guide/

### Install Styler
In RStudio, run following commands:
```r
install.packages("styler")
```