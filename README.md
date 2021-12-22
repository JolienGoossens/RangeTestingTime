# RangeTestingTime

This repository contains the code to assess range and its impacting factors with a novel approach that is aimed towards studies making use of binary presence/absence metrics. Usin an extensive empirical dataset, we applied the new method to explore and quantify the performance of acoustic receivers in a set-up in a Belgian offshore windfarm. 

Full details on the approach and the analysis are outlined in:
**Goossens J., Buyse J., Bruneel S., Verhelst P., Goethals P., Torreele E., Moens T., Reubens J.  - Taking the time for range testing - An approach to account for temporal resolution in acoustic telemetry detection range assessments.** (submitted for publication)

Data are available through DOI: 

The following protocol executes the analysis outlined in the publication. Scripts can be found in the folder src.


## 1) Get data
Script in folder **src/data**. 

*Make_folders.R*: Create necessary folders

*Get_data.R*: Load the raw data. Detection and event data were corrected for time drift using the linear time correction in VUE. Use the [LifeWatch Rstudio](https://rstudio.lifewatch.be/) to extract ETN data (also added in the DOI).

## 2) Prepare data for exploration, analysis and visualization
Scripts in folder **src/features**. Execute the preparation scripts in the following order:

### 2.1 Format raw data
*1a_Preparation_detection_data.R*: Format detection data and create subsets for different purposes.

*1b_Preparation_event_data.R*: Format event data to get receiver measurements of tilt, noise, temperature and depth.

*1c_Preparation_deploy_metadata.R*: Format deployment and receiver metadata (using event data to add median depth of receivers).

*1d_Preparation_envdata.R*: Prepare hourly and daily dataframes for wind and current data.

### 2.2 Link transmissions to detections

*2_Preparation_detection_data_synctags.R*: Format sync tag detection data. For every transmitted signal of built-in transmitters, it is calculated how many receivers detected this signal. The computation of the number of detections of each transmitted signal involves staged applications over lists within lists and takes some time. 

### 2.3 Make hourly and daily subsets of the sync

*3_Preparation_sync_hourday.R*: Group and summarise data per hour and day


## 3) Model

Scripts in folder **src/models**.

*1_Exploration.R*: Data exploration before modelling: check variance of independent variables and relationships between dependent and independent variables following a protocol for data exploration (Zuur 2010). Data for exploration are loaded with the script **src/backfun/Readandformat_model.R**.

*2_Model_detectionrange_hour.R* and *2_Model_detectionrange_day.R*: Make and select binomial GLMs for hourly and daily resolution.

*2_Model_detectionrange_hour_validation.R* and *2_Model_detectionrange_day.R_validation*: Validate binomial GLMs for hourly and daily resolution. 

*3_Model_detectiontraintest.R*: Assess different range testing scenarios by comparing the performance of the classified presence or absence as estimated by a GLM with effect sizes estimated from different training sets.

## 4) Plotting
Scripts in folder **src/visualization**.

*Fig1_pbinom.R*: Illustrate the concept of cumulating the probability of detecting a single transmission.

*Fig3_model.R*: Plot binomial model output over distance for low and high transmitting power output.

*Fig4_Fig5_assessment.R*: Assess the classification performance of different range assessment scenarios.

*Fig6_Fig7_range.R*: Visualize range as estimated for different values of n.


*Funding note: The development of the frame was financed by VLIZ as part of the Flemish contribution to the LifeWatch ESFRI funded by the Research Foundation – Flanders (FWO). Jolien Goossens holds a doctoral grant from FWO.*
