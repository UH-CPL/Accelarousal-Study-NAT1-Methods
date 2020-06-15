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
### 1. Time-wise preprocessing
Run `preprocess/preprocessTimeWise.R`.

### 2. Distance-wise preprocessing
Run `preprocess/preprocessDistanceWise.R`.

## Main Script Set
(TBU)

## Additional Tools
### Style Guide
http://jef.works/R-style-guide/

### Install Styler
In RStudio, run following commands:
```r
install.packages("styler")
```