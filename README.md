---
---
---

### ST5188-Advanced Data Science Project

### Project Group PG01: Forecasting Urban Heat Effect with Spatio-temporal Gaussian Processes

------------------------------------------------------------------------

### Overview

Focusing on Singapore region, this project is focused on mid-term LST forecasting (6-24 months ahead) by using 562 satellite images (TIF files) from Google Earth Engine (GEE) as our dataset. Exploratory Data Analysis (EDA) is performed on the data initially to understand the different characteristics present in the data.

[[Yet to add in the later analysis info also]]

------------------------------------------------------------------------

### Satellite Data

[Data/](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data "5188-Satellite Data") directory contains the raw data and intermediate processed files.

-   Landsat Satellite Images:

    -   [Data/Landsat/GEE_Landsat7](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Landsat/GEE_landsat7):Landsat 7 satellite images (TIF files)

    -   [Data/Landsat/GEE_Landsat8](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Landsat/GEE_landsat8): Landsat 8 satellite images (TIF files)

-   Shape file:

    -   [Data/Misc/Subzone/MP14_SUBZONE_NO_SEA_PL.shp](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Misc/Subzone/MP14_SUBZONE_NO_SEA_PL.shp): Shape file containing the boundaries of Singapore's subzones.

-   Summary statistics of (missing values):

    -   [Data/Misc/CHANGI_NA_Results.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Misc/CHANGI_NA_Results.csv)

    -   [Data/Misc/JURONG EAST_NA_Results.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Misc/JURONG%20EAST_NA_Results.csv)

    -    [Data/Misc/JURONG WEST_NA_Results.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Misc/JURONG%20WEST_NA_Results.csv)

-    intermediate RDS Cache

    -   [Misc/SavedRDS/](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Misc/SavedRDS): Directory to store intermediate RDS files during processing

------------------------------------------------------------------------

### Preprocessing Explanatory Data Analysis

-   [Code/Preprocessing/](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing): This directory contains the R and Python scripts used for data preprocessing and EDA.

    -   [`load_libraries.R`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/load_libraries.R): Loads necessary R libraries for data manipulation, spatial analysis, and plotting.
    -   [`combine_img.R`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/combine_img.R): It allows automation of reading multiple satellite images, extracts and imputs LST data for a specific area, and merges this data into a single, manageable data frame for further analysis. It allows applying [`extract_impute`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/extract_impute.R) on all images.
    -   [`extract_impute.R`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/extract_impute.R): It takes raw images focuses on a subzone, aligns the LST data to a common grid. It also handles the temperatures which are negative or even extremely low, followed by imputing any missing values. It also skips images where LST data is completely missing.
    -   [`impute_img.R`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/impute_img.R): Imputes missing LST values within images using spatial KNN method.
    -   [`merged_ldst.R`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/merged_ldst.R): Merges LST data extracted from different satellite sources -Landsat 7 and 8.
    -   [`st_aggregate.R`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/st_aggregate.R): Aggregates the data over time, here bi-monthly period yearly, for every spatial location. The data is also processed into long format and wide format.
    -   [`create_lst_animation.py`](create_lst_animation.py): This is a Python script used to generate animated GIFs showing the temporal changes in LST for a given subzone.
    -   [`eda.R`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/eda.R): This does not only generate summary statistics but also distribution of LST, variation across different periods, LST average over time, spatial distribution and temporal dependencies and plots for LST data.
    -   [`count_NA.R`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/count_NA.R): Analyses the images individually to determine the amount of missing Land Surface Temperature (LST) data (in terms of pixel count and percentage) for the specified subzone within that image.
    -   [`distribution_NA.R`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/distribution_NA.R): Here it tries to pinpoint exactly where within the subzone missing data is most concentrated. This is achieved by dividing the subzone into broad geographic areas and counting the NAs in each of those areas.
    -   [`create_na_animation.py`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/create_na_animation.py): This shows the visualisation of missing data over time each specified region.

    ------------------------------------------------------------------------

-   R (Version 4.4.1) Dependencies-Libraries used for this portion with their versions are:

    -   terra \|1.8.21\| Provides methods for manipulating spatial data in raster and vector data types

    -   sf \|1.0.16 \| Allows encoding and analysing of spatial data

    -   dplyr \| 1.1.4 \| Simplifies data manipulation

    -   data.table\| 1.16.0 \| For aggregating large datasets

    -   pbapply\| 1.7.2 \| To obtain progress bar

-   Python Dependencies-The Python script [`create_lst_animation.py`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/create_lst_animation.py) depends on the following Python libraries:

    -   matplotlib\| 3.9.4\| For creating plots and animations. Specifically, it uses \`matplotlib.pyplot\` for plotting functions and \`matplotlib.colors\` for color map manipulation. 
    -   matplotlib.colors.LinearSegmentedColormap \|3.9.4\| Used for defining custom color maps for visualization. 
    -   matplotlib.patches.Patch \| 3.9.4 \| for creating legend elements or other graphical patches.
    -   numpy \| 1.26.4 \| For numerical operations, especially handling arrays of image data. 
    -   os\| built in \| For interacting with the operating system, such as handling file paths. 
    -   imageio \| 2.37.0 \|For reading and writing image files, which is essential for creating the animated GIFs.

    [`create_na_animation.py`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/create_na_animation.py) depends on additionally these 2 libraries:

    -   geopandas \|1.0.1\| for working with geospatial vector data (like shapefiles or GeoJSON) in Python extending the functionalities of pandas to handle geographic data.

    -   pandas \| \>=1.4.0\|This is a fundamental library for data manipulation and analysis providing data structures like DataFrames and Series, which are essential for working with tabular data.

------------------------------------------------------------------------

## Baseline Models:

### Autoregressive Integrated Moving Average (ARIMA)

#### Data Preparation:

Reads imputed training ([imp_train_set.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation/imp_train_set.csv)) and testing ([imp_test_set.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation/imp_test_set.csv)) datasets from [Data/Final/Imputation](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation).

-   [unoptimized ARIMA.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/unoptimized%20ARIMA.R): to handle spatio temporal forecasting for temperature data across geographic points. The main functions under used in this portion are as follows.

    -   process_neighborhood: executes the whole process ARIMA modeling workflow for the area by handling data loading, spatial sampling, model fitting and also forecasting

    -    convert_period_to_date: converts period in string format to Date as objects.

-   [optimized ARIMA.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/optimized%20ARIMA.R): This provides automated optimized ARIMA modeling for 30 geographic points also generating seasonally adjusted temperature forcats with accuracy metrics

    -   convert_period_to_date: to convert date from string format to object type.

    <!-- -->

    -   calculate_rmse: Calculates the Root Mean Squared Error (RMSE) values using the observed and forcasted value.

    -    Grid Search loop: Tests best ARIMA order point by iterating over each point and returning the RMSE value.

-   [Optimized ARIMA with RMSE.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/Optimized%20ARIMA%20with%20RMSE.R) : This is performing the model evaluation and comparison by having its primary output as RMSE matrix and validation as quantified accuracy across specified horizons.

    It has the similar custom functions created in [optimized_ARIMA.R/](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/optimized%20ARIMA.R)

-   [Rolling window and RMSE.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/Rolling%20window%20and%20RMSE.R):It is implementing time based cross validation by testing models on multiple training windows (9-13 years) by taking 10 random samples per window. It also calculates RMSE for each forecast horizon (1/3/9/12 months) across all 30 points.

    Even this reusing custom function already created in [optimized_ARIMA.R/](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/optimized%20ARIMA.R)

-   [Changi imputed and restructured.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/CHANGI%20IMPUTED%20DATA%20RESTRUCTURE.R) :It restructures imputed Land Surface Temperature data into a complete data format with consistent formatting and no missing location data combinations.

-   R (Version 4.4.1) :Libraries along with their versions used in this section are:

    -   dplyr \| 1.1.4

    -   readr \|2.1.5

    -   tidyr \| 1.3.1

    -   forecast\| 8.23.0

    -   urca \|1.3-4

    -   ggplot2 \| 3.5.1

    -   cluster \| 2.1.6

    -   lubridate\| 1.9.3

    -   here\| 1.0.1

    -   tibble \| 3.2.1
