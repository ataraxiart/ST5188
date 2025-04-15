### ST5188-Advanced Data Science Project

### Project Group PG01: Forecasting Urban Heat Effect with Spatio-temporal Gaussian Processes

------------------------------------------------------------------------

### Overview

Focusing on Singapore region, this project is focused on mid-term LST forecasting (6-24 months ahead) by using 562 satellite images (TIF files) from Google Earth Engine (GEE) as our dataset. Exploratory Data Analysis (EDA) is performed on the data initially to understand the different characteristics present in the data.

Baseline models like ARIMA and LSTMs are used. At the end, our solution is a Bayesian GP (Gaussian Process) Model.



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

    ### For Code Reproducibility:

    Knit [preprocess_main.rmd](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/preprocess_main.rmd)

    ### Dependencies

-   **R (Version 4.4.1)**:

| Packages      | Version  | Comments                                                  |
|---------------|----------|-----------------------------------------------------------|
| `terra`       | 1.8.21   | Manipulating spatial data in raster and vector data types |
| `sf`          | 1.0-16   | Encoding and analysing spatial data                       |
| `dplyr`       | 1.1.4    | Simplifies data manipulation                              |
| `data.table`  | 1.16.0   | Aggregating large datasets                                |
| `pbapply`     | 1.7-2    | For progress bar                                          |
| `reticulate`  | 1.41.0   | To convert `.ipynb` files to `.py` format                 |


-   **Python (Version 3.9.6)**-For Python script [`create_lst_animation.py`](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/Preprocessing/create_lst_animation.py)

| Packages     | Version   | Comments                                                        | Key Classes                                      |
|--------------|-----------|------------------------------------------------------------------|--------------------------------------------------|
| `matplotlib` | 3.9.4     | For creating plots and animations.                              | - `pyplot`<br>- `colors.LinearSegmentedColormap`<br>- `patches` |
| `numpy`      | 1.26.4    | For numerical operations, like handling arrays of image data    | —                                                |
| `os`         | built-in  | For interacting with operating system, like handling file paths | —                                                |
| `imageio`    | 2.37.0    | For reading and writing image files essential for animated GIFs | —                                                |


- For [`create_na_animation.py`](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Code/Preprocessing/create_na_animation.py),it depends on additionally these 2 libraries:

| Packages     | Version   | Comments                                                                                                                                      |
|--------------|-----------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| `geopandas`  | 1.0.1     | For geospatial vector data (like shapefiles or GeoJSON); extends functionality of pandas to handle geographic data                          |
| `pandas`     | >=1.4.0   | Fundamental library for data manipulation and analysis, providing data structures like DataFrames and Series — essential for tabular data   |


------------------------------------------------------------------------

## Baseline Models

### Autoregressive Integrated Moving Average (ARIMA)

### Data Source: [Data/Final/Imputation](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation)

-   [imp_train_set.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation/imp_train_set.csv): Training dataset

-   [imp_test_set.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation/imp_test_set.csv): Testing dataset

### Scripts :

-   [changi_imputed_and_restructured.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/CHANGI%20IMPUTED%20DATA%20RESTRUCTURE.R) :Restructures imputed Land Surface Temperature data into a complete data format with consistent formatting and no missing location data combinations.

-   [unoptimized_ARIMA.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/unoptimized%20ARIMA.R): to handle basic spatio temporal forecasting for temperature data across geographic points.

    -   `process_neighborhood()`: Main function for training and forecasting per location

    -   `convert_period_to_date()`: Converts period in string format to `Date` as objects.

-   [optimized_ARIMA.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/optimized%20ARIMA.R): Automates optimized ARIMA modeling for 30 geographic points also generating seasonally adjusted temperature forecasts with accuracy metrics

    -   `calculate_rmse()`: Calculates the Root Mean Squared Error (RMSE) values using the observed and forecasted value.

    -   Grid Search loop: Tests best ARIMA order point by iterating over each point and returning the RMSE value.

-   [optimized_ARIMA_with_RMSE.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/Optimized%20ARIMA%20with%20RMSE.R) : Model is evaluated and compared by having primary output as RMSE matrices across horizons

-   [rolling_window_and_RMSE.R](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Code/ARIMA/Rolling%20window%20and%20RMSE.R): Implements time-based cross validation by testing models on multiple training windows (9-13 years) by taking 10 random samples per window. It also calculates RMSE for each forecast horizon (1/3/9/12 months) across all 30 points.

    ### For Code Reproducibility:

    [run_ARIMA.R](https://github.com/ataraxiart/ST5188/blob/0b034138d2feb9137eda013210dd461794bfdcfc/Code/ARIMA/Run_ARIMA.R): Master file to execute the scripts for ARIMA portion

    ### Dependencies:

-   **R (Version 4.4.1)** :

    | Packages    | Version | Comments                       |
    |-------------|---------|--------------------------------|
    | `dplyr`     | 1.1.4   |                                |
    | `readr`     | 2.1.5   |                                |
    | `tidyr`     | 1.3.1   |                                |
    | `forecast`  | 8.23.0  | ARIMA modeling and forecasting |
    | `urca`      | 1.3-4   | Unit root tests                |
    | `ggplot2`   | 3.5.1   |                                |
    | `cluster`   | 2.1.6   |                                |
    | `lubridate` | 1.9.3   | Date-time manipulation         |
    | `here`      | 1.0.1   |                                |
    | `tibble`    | 3.2.1   |                                |

### LSTMs

### \>Without Sliding Window

### Data Source: [Data/Final/](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation)

-   [Imputation/imp_train_set.csv](https://github.com/ataraxiart/ST5188/blob/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation/imp_train_set.csv): True values (changi_df)

-   [TT Split/changi_test_long.csv](https://github.com/ataraxiart/ST5188/blob/6dcff65326979106e7bccc3ff97c09c62da37469/Data/Final/TT%20Split/changi_test_long.csv): Predicted values ( pred_df)

### Custom Functions created:

-   `install_and_import` : To check if packages required are installed else installs it.
-   `LSTMPredictor` :defines the LSTM model in 2 different approaches
    -   **Without dropout**- Utilizes all neurons fully during training, potentially leading to better training accuracy but a higher risk of overfitting.

    -   **With dropout**- Randomly deactivates neurons during training to prevent overfitting and improve generalization to new data.
-   **Training loop**: It is the model's learning phase where it refines predictions over 100 tries (epochs), using Adam optimization to minimize its mistakes, and shows progress every 10 tries.

### For Code Reproducibility:

[LSTM without sliding window.ipynb](https://github.com/ataraxiart/ST5188/blob/6dcff65326979106e7bccc3ff97c09c62da37469/Code/LSTMs/Without%20rolling%20window/LSTM%20without%20sliding%20window.ipynb)

### Output files

-   [rmse_lstm_no_dropout.csv](https://github.com/ataraxiart/ST5188/blob/6dcff65326979106e7bccc3ff97c09c62da37469/Code/LSTMs/Without%20rolling%20window/rmse_lstm_no_dropout.csv) : RMSE values for baseline model across forecast horizons (1 -12 months)

-   [rmse_lstm_dropout.csv](https://github.com/ataraxiart/ST5188/blob/6dcff65326979106e7bccc3ff97c09c62da37469/Code/LSTMs/Without%20rolling%20window/rmse_lstm_dropout.csv): RMSE values for dropout performed model

### Dependencies

-   **Python (Version 3.9.6)**

    | Packages  | Version |
    |-----------|---------|
    | `pyTorch` | 2.6.0   |
    | `pandas`  | 2.0.3   |
    | `numPy`   | 1.23.5  |

### \>With Rolling Windows

In this LSTM is evaluated by using different historical window sizes (9 to 13 years) for forecasting Land Surface Temperature (LST) 2 years ahead (12 bimonthly steps). Each window size is tested with and without dropout regularization.

### Data Source: [Data/Final/](https://github.com/ataraxiart/ST5188/tree/c0f4a45fb91d42df8bf57621511178a3d47f6983/Data/Final/Imputation)

-   [Imputation/changi_imp_final.csv](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Data/Final/Imputation/changi_imp_final.csv) :Bimonthly LST values for Changi, Singapore, 2000–2020 (df)

### Custom Functions:

#### Data Preparation

-   `install_and_import` : To check if packages required are installed else installs it.
-   `create_time_index()` :Allows the reformatting of time index in dataset
-   `filter_by_geography()` :Filters 100 coordinates into the bounding box
-   `create_lstm_sequences()` : Prepares sequences for LSTMs

#### Model Fitting using `LSTMPredictor()` :

-   **Without Dropout**- 3 layer LSTM (`hidden_size=64`)

-   **With Dropout**- Adds `dropout=0.2` and `weight_decay=1e-6`

#### **Experiment Design:**

-   **Window Sizes**: 9, 10, 11, 12, 13 years (each year = 6 bimonthly periods).

-   **Forecast Horizon**: 12 steps (2 years).

-   **Training**:

    -   10 random train-test splits per window size.

    -   Train: `window_size * 6` steps → Test: Next 12 steps.

-   **Metrics**: RMSE at 1st, 3rd, 6th, 9th, 12th steps and overall.

### For Code Reproducibility:

-   [lstm w window = 9.ipynb](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/lstm%20w%20window%20%3D%209.ipynb)

-   [lstm w window = 10.ipynb](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/lstm%20w%20window%20%3D%2010.ipynb)

-   [lstm w window = 11.ipynb](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/lstm%20w%20window%20%3D%2011.ipynb)

-   [lstm w window = 12.ipynb](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/lstm%20w%20window%20%3D%2012.ipynb)

-   [lstms w window = 13.ipynb](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/lstms%20w%20window%20%3D%2013.ipynb)

### Output files:

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

### Dependencies:

-   Python (Version 3.9.6): Libraries used in this portion are

    | Packages       | Version |
    |----------------|---------|
    | `PyTorch`      | 2.6.0   |
    | `Numpy`        | 1.26.4  |
    | `Pandas`       | 2.2.3   |
    | `GeoPandas`    | 1.0.1   |
    | `Shapely`      | 2.0.7   |
    | `Scikit-learn` | 1.6.1   |

### \>For Merging RMS stats

#### Data Source: [LSTMs/With rolling window](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window)

-   Without Dropout: `rmse_9.csv`, `rmse_10.csv`, ..., `rmse_13.csv`

-   With Dropout: `rmse_dropout_9.csv`, `rmse_dropout_10.csv`, ..., `rmse_dropout_13.csv`

### For Code Reproducibility:

[code to merge merge the rmse stats](https://github.com/ataraxiart/ST5188/blob/80c76fe6abc5eccbad0ca3edb13266603fb7df4f/Code/LSTMs/With%20rolling%20window/code%20to%20merge%20the%20rmse%20stats.ipynb)

### Output files:

-   [merged_rmse_df](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/merged_rmse.csv) :Combined RMSE results obtained by models without dropout

-   [merged_rmse_dropout_df](https://github.com/ataraxiart/ST5188/blob/eeeb4d161c39c31ce857e4c3e009be959bd07b5e/Code/LSTMs/With%20rolling%20window/merged_rmse_dropout.csv) : Combined RMSE results obtained by models with dropout

### Dependencies

-   **Python (Version 3.9.6)** with Pandas 2.2.3

------------------------------------------------------------------------

## Gaussian Process: Spatial- Temporal Modeling

### Data Source: Data/Final

-   [final_CHANGI_long.csv](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Data/Final/final_CHANGI_long.csv)

### Functions:

-   **Date/Time**

    -   `get_date_index ()` : Creates date index for the bimonthly periods (Jan-Feb 2000 to Nov-Dec 2024)

    -   `filter_by_date_index()`:filter by date index in given data

    -   `get_date_range()` : Obtain range of date index in data

    -   `get_bimonth_year_columns()` : Extracts bimonth and year columns

-   **Spatial**

    -   `get_resolution()` : Calculates the spatial resolution for the data

    -   `get_bounding_box()` :To create spatial bounding box polygon

    -   `filter_by_geography()`:To filter by geographic area

-   **For Lag/Window**

    -   `get_lags()` :creates lagged values for the time series data

    -   `get_valid_rows()` :eliminates rows with NA values

    -   `get_data_by_window()` :returns a list of rolling time windows for the whole data

    -   `filter_lst_by_index()` :return data from list to be used in prediction

-   **Data Processing:** `get_processed_data()` internally calls

    -   `impute_spline()` :performs spline interpolation per (x, y) group to handle missing LST values

-   **Validation**: `get_validation_df()` internally calls

    -   `get_RMSE_for_specific_time_step()` :To calculate RMSE for specific forecast time steps.

### [Initial Stage Experimentation]{.underline}

#### Scripts:

-   [lag_experiment.R](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Code/spTimer/GP%20Experiment%20Files/Initial%20Experiments/lag_experiment.R) :Determines the suitable number of lag terms to be considered by testing lags 1-6 using rolling windows. It returned RMSE at forecast horizons (1,3,6,9,12 months)

-   [covariance_experiment.R](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Code/spTimer/GP%20Experiment%20Files/Initial%20Experiments/covariance_experiment.R) :Performs tests using covariance functions (exponential, gaussian,spherical and matern) using the optimal lag count from `lag_experiment.R`

    Returns RMSE values to identify the best performing function.

    **Key Function:** `run_cov_experiment()` evaluates the covariance functions by using 3 lags on rolling windows and outputs RMSE results for forecast steps (1,3,6,9,12) and overall performance

-   [spatial_decay_grid_search_changi_experiment.R](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Code/spTimer/GP%20Experiment%20Files/Initial%20Experiments/spatial_decay_grid_search_changi_experiment.R) :Tests using fixed spatial decay ($\phi \epsilon [0.1,2]$) and taking the assumptions from the previous scripts for best lag count and covariance function. This returns reasonable range for $\phi$

-   [spatial_decay_bayesian_mcmc_changi_experiment.R](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Code/spTimer/GP%20Experiment%20Files/Initial%20Experiments/spatial_decay_bayesian_mcmc_changi_experiment.R) :Tests Bayesian priors for spatial decay (Gamma distributions) and returns best prior distribution for $\phi$

#### Output Files:

-   `cov_experiment.csv` - RMSE results for covariance function comparison
-   `lag_experiment.csv` - RMSE results for lag term comparison
-   `grid_search_changi_experiment.csv` - RMSE results for spatial decay parameter grid search
-   `hyperparameter_changi_experiment.csv` - RMSE results for Bayesian prior comparison

#### For Code Reproducibility:

Run [Run_Initial_Experimentation.R](https://github.com/ataraxiart/ST5188/blob/761379f4ba26832af02c9727b4bfe28c24011a99/Code/spTimer/GP%20Experiment%20Files/Initial%20Experiments/Run_Initial_Experimentation.R)

### [Final Stage Experimentation]{.underline}

#### Scripts with its specific functions:

-   [hyperparameter_changi_experiment.R](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Code/spTimer/GP%20Experiment%20Files/hyperparameter_changi_experiment.R): This is for initial Bayesian exploration of spatial decay $(\phi)$ priors by testing for 7 Gamma prior distributions for $\phi$.

    -   `run_hyperparameter_experiment()` :tests different Bayesian priors for the spatial decay parameter ($\phi$) to identify which prior distribution yields the best model performance.

    -   `get_RMSE_for_specific_time_step()` :Calculates the RMSE for predictions at specific forecast horizons across all spatial locations.

-   [final_gp_rolling_window_experiment.R](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Code/spTimer/GP%20Experiment%20Files/final_gp_rolling_window_experiment.R) :

    -   `run_final_gp()` :Performs cross-window validation of the optimized GP model across multiple historical window sizes (9-13 years) to assess temporal robustness.

-   [final_gp_23_2_experiment.R](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Code/spTimer/GP%20Experiment%20Files/final_gp_23_2_experiment.R)

    -   `run_bayesian_phi()` :To use Bayesian to obtain $\phi$ list

    -   `run_grid_search_experiment()` : To run Grid Search to obtain optimal phi with these internal functions

    #### For Code Reproducibility:

    Run [Run_Final_Experimentation.R](https://github.com/ataraxiart/ST5188/blob/761379f4ba26832af02c9727b4bfe28c24011a99/Code/spTimer/GP%20Experiment%20Files/Run_Final_Experimentation.R)

### Dependencies for both stages:

-   **R (Version 4.4.1)**

    | Packages  | Version |
    |-----------|---------|
    | dplyr     | 1.1.4   |
    | lubridate | 1.9.4   |
    | sf        | 1.0-20  |
    | spTimer   | 3.3.3   |
    | here      | 1.0.1   |
    | tidyr \*  | 1.3.1   |
    | purrr \*  | 1.0.4   |

    : \* indicates that these packages are used in the Final Stage Experimentation, that is [final_gp_rolling_window_experiment.R](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Code/spTimer/GP%20Experiment%20Files/final_gp_rolling_window_experiment.R) and [final_gp_23_2_experiment.R](https://github.com/ataraxiart/ST5188/blob/8c9dcaf66ee1c07458b2ab1da49eb53a81b0cad8/Code/spTimer/GP%20Experiment%20Files/final_gp_23_2_experiment.R) for efficient data manipulation, iteration over model parameters, and structuring the results.

------------------------------------------------------------------------
