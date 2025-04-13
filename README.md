### ST5188-Advanced Data Science Project

### Project Group PG01: Forecasting Urban Heat Effect with Spatio-temporal Gaussian Processes

------------------------------------------------------------------------

### Overview

Focusing on Singapore region, this project is focused on mid-term LST forecasting (6-24 months ahead) by using 562 satellite images (TIF files) from Google Earth Engine (GEE) as our dataset. Exploratory Data Analysis (EDA) is performed on the data initially to understand the different characteristics present in the data.

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

    -   [Data/Misc/JURONG WEST_NA_Results.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Misc/JURONG%20WEST_NA_Results.csv)

-   intermediate RDS Cache

    -   [Misc/SavedRDS/](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Misc/SavedRDS): Directory to store intermediate RDS files during processing

------------------------------------------------------------------------

### Preprocessing & Explanatory Data Analysis

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

    ### Dependencies

-   **R (Version 4.4.1)** -Packages used for this portion with their versions are:

    -   terra \|1.8.21\| Provides methods for manipulating spatial data in raster and vector data types

    -   sf \|1.0.16 \| Allows encoding and analysing of spatial data

    -   dplyr \| 1.1.4 \| Simplifies data manipulation

    -   data.table\| 1.16.0 \| For aggregating large datasets

    -   pbapply\| 1.7.2 \| To obtain progress bar

-   **Python (Version 3.9.6)**-The Python script [`create_lst_animation.py`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/create_lst_animation.py) depends on the following Python libraries:

    -   matplotlib\| 3.9.4\| For creating plots and animations. Specifically, it uses \`matplotlib.pyplot\` for plotting functions and \`matplotlib.colors\` for color map manipulation. 
    -   matplotlib.colors.LinearSegmentedColormap \|3.9.4\| Used for defining custom color maps for visualization. 
    -   matplotlib.patches.Patch \| 3.9.4 \| For creating legend elements or other graphical patches.
    -   numpy \| 1.26.4 \| For numerical operations, especially handling arrays of image data. 
    -   os\| built in \| For interacting with the operating system, such as handling file paths. 
    -   imageio \| 2.37.0 \|For reading and writing image files, which is essential for creating the animated GIFs.

    [`create_na_animation.py`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/create_na_animation.py) depends on additionally these 2 libraries:

    -   geopandas \|1.0.1\| for working with geospatial vector data (like shapefiles or GeoJSON) in Python extending the functionalities of pandas to handle geographic data.

    -   pandas \| \>=1.4.0\|This is a fundamental library for data manipulation and analysis providing data structures like DataFrames and Series, which are essential for working with tabular data.

------------------------------------------------------------------------

## Baseline Models

### Autoregressive Integrated Moving Average (ARIMA)

#### Data Source: [Data/Final/Imputation](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation)

-   [imp_train_set.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation/imp_train_set.csv): Training dataset

-   [imp_test_set.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation/imp_test_set.csv): Testing dataset

#### Scripts:

-   [Changi imputed and restructured.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/CHANGI%20IMPUTED%20DATA%20RESTRUCTURE.R) :Restructures imputed Land Surface Temperature data into a complete data format with consistent formatting and no missing location data combinations.

-   [unoptimized ARIMA.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/unoptimized%20ARIMA.R): to handle basic spatio temporal forecasting for temperature data across geographic points.

    -   `process_neighborhood()`: Main function for training and forecasting per location

    -   `convert_period_to_date()`: Converts period in string format to `Date` as objects.

-   [optimized ARIMA.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/optimized%20ARIMA.R): Automates optimized ARIMA modeling for 30 geographic points also generating seasonally adjusted temperature forcats with accuracy metrics

    -   `calculate_rmse()`: Calculates the Root Mean Squared Error (RMSE) values using the observed and forcasted value.

    -   Grid Search loop: Tests best ARIMA order point by iterating over each point and returning the RMSE value.

-   [Optimized ARIMA with RMSE.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/Optimized%20ARIMA%20with%20RMSE.R) : Model is evaluated and compared by having primary output as RMSE matrices across horizons

-   [Rolling window and RMSE.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/Rolling%20window%20and%20RMSE.R): Implements time-based cross validation by testing models on multiple training windows (9-13 years) by taking 10 random samples per window. It also calculates RMSE for each forecast horizon (1/3/9/12 months) across all 30 points.

    #### For Code Reproductibility:

    [Run_ARIMA.R](https://github.com/ataraxiart/ST5188/blob/0b034138d2feb9137eda013210dd461794bfdcfc/Code/ARIMA/Run_ARIMA.R): Master file to execute the scripts for ARIMA portion

    #### Dependencies:

-   **R (Version 4.4.1)** :Libraries along with their versions used in this section are:

    -   dplyr \| 1.1.4

    -   readr \|2.1.5

    -   tidyr \| 1.3.1

    -   forecast\| 8.23.0 \| ARIMA modeling and forecasting

    -   urca \|1.3-4 \| Unit root tests

    -   ggplot2 \| 3.5.1

    -   cluster \| 2.1.6

    -   lubridate\| 1.9.3 \| Date-time manipulation

    -   here\| 1.0.1

    -   tibble \| 3.2.1

### LSTMs

#### \>Without Sliding Window

#### Data Source: [Data/Final/](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation)

-   [Imputation/imp_train_set.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation/imp_train_set.csv): True values (changi_df)

-   [TT Split/changi_test_long.csv](https://github.com/ataraxiart/ST5188/blob/6dcff65326979106e7bccc3ff97c09c62da37469/Data/Final/TT%20Split/changi_test_long.csv): Predicted values ( pred_df)

#### Custom Functions created:

-   `install_and_import` : To check if packages required are installed else installs it.
-   `LSTMPredictor` :defines the LSTM model in 2 different approaches
    -   **Without dropout**- Utilizes all neurons fully during training, potentially leading to better training accuracy but a higher risk of overfitting.

    -   **With dropout**- Randomly deactivates neurons during training to prevent overfitting and improve generalization to new data.
-   **Training loop**: It is the model's learning phase where it refines predictions over 100 tries (epochs), using Adam optimization to minimize its mistakes, and shows progress every 10 tries.

#### For Code Reproductibility:

[LSTM without sliding window.ipynb](https://github.com/ataraxiart/ST5188/blob/6dcff65326979106e7bccc3ff97c09c62da37469/Code/LSTMs/Without%20rolling%20window/LSTM%20without%20sliding%20window.ipynb)

#### Output files

-   [rmse_lstm_no_dropout.csv](https://github.com/ataraxiart/ST5188/blob/6dcff65326979106e7bccc3ff97c09c62da37469/Code/LSTMs/Without%20rolling%20window/rmse_lstm_no_dropout.csv) : RMSE values for baseline model across forecast horizons (1 -12 months)

-   [rmse_lstm_dropout.csv](https://github.com/ataraxiart/ST5188/blob/6dcff65326979106e7bccc3ff97c09c62da37469/Code/LSTMs/Without%20rolling%20window/rmse_lstm_dropout.csv): RMSE values for dropout performed model

#### Dependencies

-   **Python (Version 3.9.6)** :Libraries along with their versions used in this section

    -   PyTorch \| 2.6.0

    -   Pandas \| 2.0.3

    -   NumPy \| 1.23.5

#### \>With Rolling Windows

In this LSTM is evaluated by using different historical window sizes (9 to 13 years) for forecasting Land Surface Temperature (LST) 2 years ahead (12 bimonthly steps). Each window size is tested with and without dropout regularization.

#### Data Source: [Data/Final/](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation)

-   [Imputation/changi_imp_final.csv](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Data/Final/Imputation/changi_imp_final.csv) :Bimonthly LST values for Changi, Singapore, 2000–2020 (df)

#### Custom Functions:

#### Data Preparation

-   `install_and_import` : To check if packages required are installed else installs it.
-   `create_time_index()` :Allows the reformatting of time index in dataset
-   `filter_by_geography()` :Filters 100 coordinates into the bounding box
-   `create_lstm_sequences()` : Prepares sequences for LSTMs

#### Model Fitting of `LSTMPredictor` :

-   **Without Dropout**- 3 layer LSTM (`hidden_size=64`)

-   **With Dropout**- Adds `dropout=0.2` and `weight_decay=1e-6`

#### **Experiment Design:**

-   **Window Sizes**: 9, 10, 11, 12, 13 years (each year = 6 bimonthly periods).

-   **Forecast Horizon**: 12 steps (2 years).

-   **Training**:

    -   10 random train-test splits per window size.

    -   Train: `window_size * 6` steps → Test: Next 12 steps.

-   **Metrics**: RMSE at 1st, 3rd, 6th, 9th, 12th steps and overall.

#### For Code Reproductibility:

-   [lstm w window = 9.ipynb](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/lstm%20w%20window%20%3D%209.ipynb)

-   [lstm w window = 10.ipynb](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/lstm%20w%20window%20%3D%2010.ipynb)

-   [lstm w window = 11.ipynb](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/lstm%20w%20window%20%3D%2011.ipynb)

-   [lstm w window = 12.ipynb](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/lstm%20w%20window%20%3D%2012.ipynb)

-   [lstms w window = 13.ipynb](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/lstms%20w%20window%20%3D%2013.ipynb)

#### Output files:

| **Window Size** | **Dropout** | **Output File**       |
|:----------------|:------------|:----------------------|
| 9               | No          | `rmse_9.csv`          |
| 9               | Yes         | `rmse_dropout_9.csv`  |
| 10              | No          | `rmse_10.csv`         |
| 10              | Yes         | `rmse_dropout_10.csv` |
| 11              | No          | `rmse_11.csv`         |
| 11              | Yes         | `rmse_dropout_11.csv` |
| 12              | No          | `rmse_12.csv`         |
| 12              | Yes         | `rmse_dropout_12.csv` |
| 13              | No          | `rmse_13.csv`         |
| 13              | Yes         | `rmse_dropout_13.csv` |

#### Dependencies:

-   Python (Version 3.9.6): Libraries used in this portion are

    -   PyTorch \| 2.6.0

    -   NumPy \| 1.26.4

    -   Pandas \| 2.2.3

    -   GeoPandas \| 1.0.1

    -   Shapely \| 2.0.7

    -   Scikit-learn \| 1.6.1

#### \>For Merging RMS stats

#### Data Source: [LSTMs/With rolling window](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window)

-   Without Dropout: `rmse_9.csv`, `rmse_10.csv`, ..., `rmse_13.csv`

-   With Dropout: `rmse_dropout_9.csv`, `rmse_dropout_10.csv`, ..., `rmse_dropout_13.csv`

#### For Code Reproductibility:

[code to merge merge the rmse stats](https://github.com/ataraxiart/ST5188/blob/80c76fe6abc5eccbad0ca3edb13266603fb7df4f/Code/LSTMs/With%20rolling%20window/code%20to%20merge%20the%20rmse%20stats.ipynb)

#### Output files:

-   [merged_rmse_df](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/merged_rmse.csv) :Combined RMSE results obtained by models without dropout

-   [merged_rmse_dropout_df](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/merged_rmse_dropout.csv) : Combined RMSE results obtained by models with dropout

#### Dependencies

-   **Python (Version 3.9.6) :**

    -   Pandas \|2.2.3
