---
---
---

## ST5188-Advanced Data Science Project

## Project Group PG01: Forecasting Urban Heat Effect with Spatio-temporal Gaussian Processes

## Overview

Focusing on Singapore region, this project is focused on mid-term LST forecasting (6-24 months ahead) by using 562 satellite images (TIF files) from Google Earth Engine (GEE) as our dataset. Exploratory Data Analysis (EDA) is performed on the data initially to understand the different characteristics present in the data.

[[Yet to add in the later analysis info also]]

Data:

-   **`./Data/`**: This directory contains the raw data and intermediate processed files.

    -   `./Data/Landsat/GEE_landsat7/`: Contains Landsat 7 satellite images (TIF files).

    -   `./Data/Landsat/GEE_landsat8/`: Contains Landsat 8 satellite images (TIF files).

    -   `./Data/Misc/Subzone/MP14_SUBZONE_NO_SEA_PL.shp`: Shape file containing the boundaries of Singapore's subzones.

    -   `./Data/Misc/CHANGI_NA_Results.csv`, `./Data/Misc/JURONG EAST_NA_Results.csv`, etc.: CSV files containing summary statistics of missing values for each subzone.

    -   `./Data/Misc/SavedRDS/`: Directory to store intermediate RDS files.

Preprocessing:

-   **`./Code/Preprocessing/`**: This directory contains the R and Python scripts used for data preprocessing and EDA.

    -   `load_libraries.R`: Loads necessary R libraries for data manipulation, spatial analysis, and plotting.

    <!-- -->

    -   `combine_img.R`: It allows automation of reading multiple satellite images, extracts and imputs LST data for a specific area, and merges this data into a single, manageable data frame for further analysis. It allows applying `extract_impute` on all images.

    <!-- -->

    -   `extract_impute.R`: It takes raw images focuses on a subzone, aligns the LST data to a common grid. It also handles the temperatures which are negative or even extremely low, followed by imputing any missing values. It also skips images where LST data is completely missing.

    -   `impute_img.R`: Imputes missing LST values within images using spatial KNN method.

    -   `merged_ldst.R`: Merges LST data extracted from different satellite sources -Landsat 7 and 8.

    -   `st_aggregate.R`: Aggregates the data over time, here bi-monthly period yearly, for every spatial location. The data is also processed into long format and wide format.

    -   `create_lst_animation.py`: This is a Python script used to generate animated GIFs showing the temporal changes in LST for a given subzone.

    -   `eda.R`: This does not only generate summary statistics but also distribution of LST, variation across different periods, LST average over time, spatial distribution and temporal dependencies and plots for LST data.

    -   `count_NA.R`: Analyses the images individually to determine the amount of missing Land Surface Temperature (LST) data (in terms of pixel count and percentage) for the specified subzone within that image.

    -   `distribution_NA.R`: Here it tries to pinpoint exactly where within the subzone missing data is most concentrated. This is achieved by dividing the subzone into broad geographic areas and counting the NAs in each of those areas.

    -   `create_na_animation.py`: This shows the visualisation of missing data over time each specified region.

-   R Dependencies-Libraries used for this portion are:

    -   terra

    -   sf

    -   dplyr

    -   data.table

    -   pbapply: To obtain progress bar

-   Python Dependencies-The Python script `create_lst_animation.py` depends on the following Python libraries:

    -   matplotlib: For creating plots and animations. Specifically, it uses \`matplotlib.pyplot\` for plotting functions and \`matplotlib.colors\` for color map manipulation. 
    -   matplotlib.colors.LinearSegmentedColormap: Used for defining custom color maps for visualization. 

    <!-- -->

    -   matplotlib.patches.Patch: Potentially used for creating legend elements or other graphical patches.

    -   numpy: For numerical operations, especially handling arrays of image data. 

    -   os: For interacting with the operating system, such as handling file paths. 

    -   imageio: For reading and writing image files, which is essential for creating the animated GIFs.

    `create_na_animation.py` depends on additionally these 2 libraries:

    -   geopandas as gpd: for working with geospatial vector data (like shapefiles or GeoJSON) in Python. It also extends the functionalities of pandas to handle geographic data.

    -   pandas as pd: This is a fundamental library for data manipulation and analysis providing data structures like DataFrames and Series, which are essential for working with tabular data.
