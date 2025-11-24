# Mining Induced Forest Change
Spatial Ecology Thesis - Assessing Mining-Induced Forest Change in Pra River Basin using Sentinel-2 imagery

MSc Thesis – Global Change Ecology & Sustainable Development
University of Bologna (UniBo), Italy
Supervisor: Prof. Duccio Rocchini
Department of Biological, Geological and Environmental Sciences (BiGeA)

## Project Overview

This repository contains the workflow, code, and outputs for my MSc thesis project assessing mining-induced forest vegetation change in the Pra River Basin (Ghana).
The project uses Sentinel-2 imagery, RGB composites, and spatial ecology analyses in R to quantify vegetation loss, identify mining hotspots, and explore spatial–temporal degradation patterns.
The Pra River Basin is heavily impacted by artisanal and industrial gold mining, making it a relevant case study for ecological monitoring, conservation planning, and sustainable land management.

## Research Objectives

1. Quantify forest cover and vegetation change across three time periods: 2017 → 2022 → 2024.
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
├── forest_change_analysis.Rmd # Main analysis workflow (in progress)
├── forest_change_analysis.md # Rendered version of the analysis
└── README.md # Project documentation
```
(Folders will be progressively filled as the thesis advances.)

## Methods (Planned – Subject to Supervisor Guidance)

### 1. Preprocessing
- Downloading and organizing Sentinel-2 imagery (2017, 2022, 2024)
- Cloud and shadow masking
- Cropping to study area
- Creation of RGB composites for each year

### 2. Spectral Analysis
**Confirmed indices:**
- RGB composites for visual detection
- NDVI (Normalized Difference Vegetation Index) for vegetation health

**Potential additional indices/methods** (subject to supervisor guidance)

### 3. Mining Impact Identification
- Visual identification of mining scars through RGB
- NDVI-based vegetation loss mapping
- Extraction of disturbed zones

### 4. Change Detection
- Multi-year NDVI differencing (2017 → 2022 → 2024)
- Temporal trend analysis
- Classification-based methods 

### 5. Spatial Ecology Analysis
- Quantifying total disturbed area
- Hotspot mapping

### 6. Mapping & Visualization
- RGB maps for 2017, 2022, 2024
- NDVI trend maps

### 7. Summary Figures
- Figures and maps for thesis and publication
  

## Software & Tools
- **Primary Environment:** RStudio
- **Key R Packages (planned):**
`terra`, `Rstoolbox`, `raster`, `patchwork`, `tidyverse`, `ggplot2`

## Data Sources (Planned)

- Sentinel-2 Level-2A imagery (2017, 2022, 2024)
  

## Current Status

Updated: November 2025

- Repository setup completed
- Preparing study area boundaries and Sentinel data collection strategy
- Full workflow to be developed after meeting with supervisor (Dec 2025)



