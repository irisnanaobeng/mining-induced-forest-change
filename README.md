## Thesis Topic: Mining-Induced Forest Degradation in Ghana’s Pra River Basin : A Multi-Temporal Remote Sensing Analysis

**MSc Thesis – Global Change Ecology & Sustainable Development**
University of Bologna (UniBo), Italy

**Author:** Iris Nana Obeng
**Supervisor:** Prof. Duccio Rocchini
**Department:** Biological, Geological and Environmental Sciences (BiGeA)


## Project Overview

This repository contains the full workflow, analysis, and outputs for my MSc thesis investigating **mining-induced forest degradation** in Ghana’s **Pra River Basin**.

The study integrates **multi-temporal Sentinel-2 imagery (2018, 2020, 2022)** with advanced spatial analysis techniques in R to:

* Detect mining activity
* Quantify vegetation loss
* Analyse spectral changes in forest ecosystems over time

The Pra River Basin is a highly impacted tropical system in which artisanal and industrial gold mining significantly alters land cover, making it an ideal case for remote sensing-based environmental monitoring.


##  Research Objectives

1. Quantify vegetation and forest cover change (2018 → 2020 → 2022)
2. Detect mining-induced disturbance using NDVI and spectral signals
3. Map spatial distribution of active mining areas
4. Analyse temporal changes in vegetation condition
5. Apply **spectral distance modelling** to assess ecosystem degradation
6. Develop a **reproducible remote sensing workflow in R**


## Study Area

The study focuses on the **Pra River Basin (Southern Ghana)**, covering parts of:

* Ashanti Region
* Eastern Region
* Central Region

This region is characterised by:

* Dense tropical forest ecosystems
* Riverine landscapes
* Intensive gold mining activities


## Data

* **Satellite:** Sentinel-2 (Level-2A)
* **Years:** 2018, 2020, 2022
* **Spatial Resolution:** 10 m
* **Bands Used:**

  * Blue (B02)
  * Green (B03)
  * Red (B04)
  * Near Infrared (B08)


## Methods

### 1. Image Preprocessing

* Reprojection to common CRS
* Spatial alignment
* Clipping to study area
* Cloud filtering


### 2. True & False Colour Composites

* **True Colour (RGB):** visual interpretation
* **False Colour (NIR-Red-Green):** vegetation detection

Used to identify:

* Forest cover
* Bare soil expansion
* Mining disturbances


### 3. NDVI Analysis

[
NDVI = \frac{NIR - Red}{NIR + Red}
]

* Generated for 2018, 2020, 2022
* Used to assess vegetation health and density


### 4. NDVI Change Detection

* NDVI Difference (2022 − 2018)
* Classification into:

  * Vegetation Increase
  * Vegetation Decrease
  * No Change


### 5. Land Cover Classification

NDVI-based thresholds used to classify:

| Class | Description          |
| ----- | -------------------- |
| 1     | Water Bodies         |
| 2     | Active Mining        |
| 3     | Healthy Vegetation   |
| 4     | Disturbed Vegetation |
| 5     | Bare Soil            |

Mining is identified using:

* Low NDVI values
* Strong decline from previous years


### 6. Spectral Distance Analysis (Fuzzy Classification)

A key innovation of this study:

* **K-means clustering (2018 baseline)**
* Fixed spectral centroids applied to 2020 and 2022
* Euclidean distance used to measure similarity

This allows:

* Detection of mining signatures
* Quantification of forest degradation
* Tracking spectral shifts over time


### 7. Mining Detection & Mapping

* Mining pixels extracted from classification
* Spatial thinning applied for visualization
* Overlaid on NDVI background


### 8. Statistical Summaries

* NDVI change distribution (% area)
* Spectral distance density plots
* Comparative temporal analysis


## Key Outputs

All outputs are stored in the **`maps/`** folder:

* NDVI time series maps
* NDVI change detection maps
* Land cover classification maps
* Mining distribution maps
* Spectral analysis plots (mining & forest)

Example:

![NDVI Time Series](maps/ndvi_timeseries.png)


## Repository Structure

```
mining-induced-forest-change/
├── maps/                         # Output figures (PNG maps & plots)
├── centroids_2018.csv           # Spectral reference clusters
├── pra_mining_forest_analysis.Rmd
├── pra_mining_forest_analysis.md
└── README.md
```

3. Install required packages:

```r
install.packages(c(
  "terra", "ggplot2", "sf", "dplyr",
  "patchwork", "viridis", "RStoolbox",
  "rnaturalearth", "ggspatial", "scales"
))
```

## Key Findings (Preliminary)

* Mining expansion is strongly associated with **NDVI decline**
* Forest areas show increasing **spectral distance from baseline conditions**
* NDVI alone is not sufficient — combining with spectral distance improves detection
* Mining hotspots are clearly identifiable spatially and temporally


## Tools & Libraries

* `terra`
* `ggplot2`
* `sf`
* `patchwork`
* `RStoolbox`
* `viridis`
* `dplyr`


## Data Source

* Copernicus Sentinel-2 Imagery Open Access Hub

## Project Status

**Last Updated:** March 2026

* Data processing complete
* NDVI & classification analysis complete
* Spectral analysis implemented
* Interpretation and thesis writing ongoing



# Author: **Iris Nana Obeng**
GitHub: https://github.com/irisnanaobeng

## Acknowledgements

* University of Bologna (UniBo)



