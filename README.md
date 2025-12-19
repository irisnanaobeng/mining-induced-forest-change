# Mining Induced Forest Change
Spatial Ecology Thesis - Assessing Mining-Induced Forest Change in Pra River Basin using Sentinel-2 imagery

MSc Thesis – Global Change Ecology & Sustainable Development
University of Bologna (UniBo), Italy.
Supervisor: Prof. Duccio Rocchini
Department of Biological, Geological and Environmental Sciences (BiGeA)

## Project Overview

This repository contains the workflow, code, and outputs for my MSc thesis project assessing mining-induced forest vegetation change in the Pra River Basin (Ghana).
The project uses Sentinel-2 imagery, RGB composites, and spatial ecology analyses in R to quantify vegetation loss, identify mining hotspots, and explore spatial–temporal degradation patterns.
The Pra River Basin is heavily impacted by artisanal and industrial gold mining, making it a relevant case study for ecological monitoring, conservation planning, and sustainable land management.

## Research Objectives

1. Quantify forest cover and vegetation change across three time periods: 2018 → 2020 → 2022.
2. Detect mining-induced vegetation disturbance using spectral indices and RGB interpretation.
3. Compare landscape changes across the three years to identify trends and hotspots of degradation.
4. Produce RGB composites and NDVI maps for each year to visualize pre-mining, active mining, and current conditions.
5. Develop a reproducible spatial-ecology workflow in R for detecting forest change using Sentinel-2.

## Repository Structure
```
mining-induced-forest-change/
├── data/ # Shapefiles, Sentinel-2 tiles, boundaries, etc.
├── scripts/ # R scripts (preprocessing, indices, mapping)
├── maps/ # Output maps and figures
├── pra_mining_forest_analysis.Rmd    
├── pra_mining_forest_analysis.md  
└── README.md # Project documentation
```
(Folders will be progressively filled as the thesis advances.)

## Methods (Planned – Subject to Supervisor Guidance)

## Methods

### 1. True Color & False Color Composites
- Visual comparison of land cover conditions across years
- Helps identify bare ground expansion and vegetation loss

### 2. NDVI Time Series
- NDVI calculated for 2018, 2020, and 2022
- Side-by-side comparison to observe vegetation health trends

### 3. NDVI Difference Analysis
- NDVI Difference = **NDVI (2022) − NDVI (2018)**
- Interpreted as:
  - **Negative values (red/pink): vegetation loss**
  - **Near-zero (white): little or no change**
  - **Positive values (green): vegetation recovery**

### 4. NDVI Change Classes
NDVI difference values were grouped into classes:
- Vegetation Increase
- Vegetation Decrease
- No Significant Change

### 5. Vegetation & Mining Classification
Five land-cover change classes were defined:
1. Healthy Vegetation  
2. Disturbed Vegetation  
3. Bare Ground / Non-Forest  
4. Water  
5. Active Mining Areas  

Classes were derived using NDVI thresholds and temporal change logic.

### 6. Mining Hotspot Mapping
- Active mining areas identified by large NDVI drops
- Mining pixels emphasized using point overlays
- Spatial thinning applied for clearer visualization

### 7. Bar Chart Summary
- Percentage of study area under:
  - Vegetation Increase
  - Vegetation Decrease
  - No Change
- Supports map-based interpretation with quantitative evidence

### 7. Summary Figures
- Figures and maps for thesis and publication
  

## Software & Tools
- **Primary Environment:** RStudio
- **Key R Packages (planned):**
`terra`, `Rstoolbox`, `raster`, `patchwork`, `tidyverse`, `ggplot2`, `dplyr`, `scales`

## Data Sources (Planned)

- Sentinel-2 Level-2A imagery (2017, 2022, 2024)
  

## Current Status

Updated: December 2025

- Repository setup completed
- Preparing study area boundaries and Sentinel data collection strategy
- Full workflow to be developed after meeting with supervisor (Jnauary 2026)



