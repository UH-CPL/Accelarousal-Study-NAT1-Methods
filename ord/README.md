# On-road Driving Study

## Getting Started
### Prerequisites
- R (3.6.1)
- RStudio (1.2.5019)
- Required packages

### Installing R Packages
To install required R packages, run script `setup.R` in each study folder.

## Folder Structure
- `data`: Raw and processed data.
- `plots`: Output plots of correlation matrices, clustering dendrogram, linear models, machine learning models.
- `outputs`: CSV/Text outputs.
- `settings`: Common setting of the project.
- `scripts`: Main script for analysis and data modeling.
- `utils`: Common ultility functions.

## Configuration
All global settings are placed in `settings/settings.R`.

### Analysis Timing Settings
The default time-series anlysis is using data of 30 seconds before to predict the class of next 5 seconds. You can change these values for different analysis.
```R
TIME_PREV_SECONDS <- 30  # [5, 10, 15, 30]
TIME_NEXT_SECONDS <- 5
```

### Plotly
As the project is using Plotly as a visualization tool, you can set up you Plotly account with follow constants.
```R
PLOTLY_USERNAME <- "<Your Plotly Username>"
PLOTLY_API_KEY <- "<Your Plotly API Key>"
```

## Script set
### 1. Correllation Analysis
- Run `scripts/correlation.R`
### 2. Clustering
- Run `scripts/clustering.R`
### 3. Linear Model
- Run `scripts/linearModel.R`
### 4. ML Model
- Run `scripts/mlModel.R`
- There are 4 possible XGBoost models that run on different sets of variables:
    - `All`: Use all variables generated from Speed, Acceleration, Braking, and Steering.
    - `SpeedOnly`: Use only variables generated from Speed.
    - `AccOnly`: Use only variables generated from Acceleration.
    - `SpeedAndAcc`: Use variables generated from Speed, Acceleration (the two show significant P-value in linear Model).

## Additional Tools
### Style Guide
http://jef.works/R-style-guide/

### Install Styler
In RStudio, run following commands:
```r
install.packages("styler")
```