---
title: "Detecting Mining-Induced Forest Degradation Through Spectral Distance Analysis of Sentinel-2 Imagery in the Pra River Basin"
author: "Iris Nana Obeng"
date: "2026-03-02"
output: github_document:
toc: true
toc_depth: 3
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.path = "maps/", echo = TRUE, warning = FALSE, message = FALSE)
  
Introduction

The PRA River Basin in Ghana has experienced significant vegetation changes due to mining activities.
This study uses Sentinel-2 imagery from 2018, 2020, and 2022 to analyze forest vegetation change, compute NDVI, and detect mining-induced disturbances.

Data Acquisition

Sentinel-2 imagery for 2018, 2020, and 2022 was pre-processed for atmospheric corrections and clipped to the study area.

## 1.Load Required Packages

The analysis uses several R packages for spatial data processing and visualization:
  
terra — for raster processing
ggplot2 — for plotting
patchwork — arranging multiple plots
scales — color scaling
RStoolbox — remote sensing functions
sf — vector data handling
dplyr — data manipulation

  ```{r load-packages}
library(terra)
library(ggplot2)
library(patchwork)
library(scales)
library(RStoolbox)
library(sf)
library(dplyr)
```
2. Custom Visualization Function
The im.ggplotRGB function creates true and false color images using ggplot2.
This function takes a 3-band raster stack and converts it to an RGB plot with:
- Automatic downsampling for faster rendering
- linear stretching for better contrast
  ```{r im-ggplotRGB-function}
  im.ggplotRGB <- function(img, r = 1, g = 2, b = 3,
                           stretch = TRUE, downsample = 6,
                           show_axes = TRUE, title = "True Colour") {
    
    # Combine bands into a single SpatRaster if not already (expects 3 bands)
    if (nlyr(img) < 3) stop("img must be a 3-band SpatRaster (R,G,B or NIR,R,G).")
    
    # Downsample for faster plotting
    img_small <- terra::aggregate(img, fact = downsample)
    df <- as.data.frame(img_small, xy = TRUE, na.rm = TRUE)
    names(df)[3:5] <- c("R","G","B")
    
    # Apply linear stretch if requested
    if (stretch) {
      df$R <- scales::rescale(df$R, to = c(0,1))
      df$G <- scales::rescale(df$G, to = c(0,1))
      df$B <- scales::rescale(df$B, to = c(0,1))
    }
    
    # Create ggplot with RGB colors
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_raster(aes(fill = rgb(R, G, B))) +
      scale_fill_identity() +
      coord_equal() +
      ggtitle(title) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            panel.grid = element_blank())
    
    # Optionally hide axes for cleaner maps
    if (!show_axes) {
      p <- p + theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank())
    }
    
    return(p)
  }
  ```
  3. Single-Band Visualization Function
  This function plots single-band rasters (like NDVI) using ggplot2.
  It includes downsampling for large datasets and uses the viridis color palette.
 
   ```{r im-ggplot-singleband-function}
  
   plot_singleband_gg <- function(r, downsample = 8, title = "") {
     r_small <- terra::aggregate(r, fact = downsample)
     df <- as.data.frame(r_small, xy = TRUE, na.rm = TRUE)
     names(df)[3] <- "val"
     ggplot(df, aes(x = x, y = y, fill = val)) +
       geom_raster() +
       scale_fill_viridis_c(option = "D", na.value = "transparent") +
       coord_equal() +
       ggtitle(title) +
       labs(fill = "NDVI") +
       theme_minimal() +
       theme(plot.title = element_text(hjust = 0.5))
   }
   ```
4. Load and Visualize Sentinel-2 Imagery
   
  ```{r load-sentinel-imagery}
  
  2018 Bands (Pre-mining baseline)
  {r}
  blue_2018  <- rast("./S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B02_10m.jp2")
  green_2018 <- rast("./S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B03_10m.jp2")
  red_2018   <- rast("./S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B04_10m.jp2")
  nir_2018   <- rast("./S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B08_10m.jp2")
  

  2020 Bands (Intermediate monitoring)
  {r}
  blue_2020  <- rast("./S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B02_10m.jp2")
  green_2020 <- rast("./S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B03_10m.jp2")
  red_2020   <- rast("./S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B04_10m.jp2")
  nir_2020   <- rast("./S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B08_10m.jp2")
  
  2022 Bands (Post-mining assessment)
  {r}
  `blue_2022  <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B02_10m.jp2")
  green_2022 <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B03_10m.jp2")
  red_2022   <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B04_10m.jp2")
  nir_2022   <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B08_10m.jp2")
  ````
##5. Create RGB Composites and Visualize

- True Color (RGB): Red, Green, Blue bands - shows natural color representation
-  False Color (NIR-R-G): NIR, Red, Green bands - highlights vegetation health
False color is particularly useful for vegetation studies as healthy plants strongly reflect NIR.

 {r}
# True Color composites (Red-Green-Blue)
tc_2018 <- c(red_2018, green_2018, blue_2018)
tc_2020 <- c(red_2020, green_2020, blue_2020)
tc_2022 <- c(red_2022, green_2022, blue_2022)

# False Color composites (NIR-Red-Green)
fc_2018 <- c(nir_2018, red_2018, green_2018)
fc_2020 <- c(nir_2020, red_2020, green_2020)
fc_2022 <- c(nir_2022, red_2022, green_2022)

##6. True Color Visualization
True color images show the landscape as it appears to the human eye.
This helps identify visible changes like deforestation and bare soil exposure.
  ```{r true-color-visualization, fig.width=10, fig.height=6}
  p_tc_2018 <- im.ggplotRGB(tc_2018, title = "True Colour 2018")
  p_tc_2020 <- im.ggplotRGB(tc_2020, title = "True Colour 2020")
  p_tc_2022 <- im.ggplotRGB(tc_2022, title = "True Colour 2022")
  
  combined_true <- (p_tc_2018 | p_tc_2020 | p_tc_2022) +
    plot_annotation(title = "True Colour Composites PRA Basin Forest (2018, 2020, 2022)",
                    theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
  
  combined_true
  
  ggsave("maps/true_color_comparison.png", combined_true, width = 15, height = 6, dpi = 300)
  
 ## 7. False Color Visualization
  False color images use near-infrared (NIR) band instead of blue.
  Healthy vegetation appears bright red, water appears dark, and urban areas appear cyan.
  This is excellent for monitoring vegetation health and detecting stress.
  ```{r false-color-visualization, fig.width=10, fig.height=6}
  p_fc_2018 <- im.ggplotRGB(fc_2018, title = "False Colour 2018")
  p_fc_2020 <- im.ggplotRGB(fc_2020, title = "False Colour 2020")
  p_fc_2022 <- im.ggplotRGB(fc_2022, title = "False Colour 2022")
  
  combined_false <- (p_fc_2018 | p_fc_2020 | p_fc_2022) +
    plot_annotation(title = "False Colour Composites PRA Basin Forest (2018, 2020, 2022)",
                    theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
  
  combined_false
  
  ggsave("maps/false_color_comparison.png", combined_false, width = 15, height = 6, dpi = 300)
  ```
 ## 7. NDVI Calculation and Analysis
  NDVI (Normalized Difference Vegetation Index) = (NIR - Red) / (NIR + Red)
  This index quantifies vegetation greenness and health:
    - Values range from -1 to +1
  - High values (>0.5) indicate dense, healthy vegetation
  - Low values (<0.2) indicate bare soil or water
  - Negative values typically indicate water
  {r}
  # NDVI calculation function
  ndvi_calc <- function(nir, red) {
    nd <- (nir - red) / (nir + red)
    names(nd) <- "NDVI"
    return(nd)
  }
  
  # Calculate NDVI for each year
  ndvi_2018 <- ndvi_calc(nir_2018, red_2018)
  ndvi_2020 <- ndvi_calc(nir_2020, red_2020)
  ndvi_2022 <- ndvi_calc(nir_2022, red_2022)
      
  ```
  8. NDVI Visualization
  Visualize NDVI maps for each year to assess vegetation health and changes over time.
  ```{r ndvi-visualization, fig.width=10, fig.height=6}
  p_ndvi_2018 <- plot_singleband_gg(ndvi_2018, title="NDVI 2018")
  p_ndvi_2020 <- plot_singleband_gg(ndvi_2020, title="NDVI 2020")
  p_ndvi_2022 <- plot_singleband_gg(ndvi_2022, title="NDVI 2022")
  
  (p_ndvi_2018 | p_ndvi_2020 | p_ndvi_2022) +
    plot_annotation(title="NDVI Composites PRA Basin Forest (2018, 2020, 2022)")
  ```
  
  9. NDVI Change Detection
  ndvi_diff_18_22 <- ndvi_2022 - ndvi_2018
  ndvi_change_18_22_class <- ndvi_change_class(ndvi_diff_18_22)
  
  p_ndvi_diff_18_22 <- plot_singleband_gg(ndvi_diff_18_22, title="NDVI Change (2022-2018)")
  p_ndvi_change_18_22_class <- plot_singleband_gg(ndvi_change_18_22_class, title="NDVI Change Classes (2018-2022)")
  
  (p_ndvi_diff_18_22 | p_ndvi_change_18_22_class) +
    plot_annotation(title="NDVI Change Detection PRA Basin Forest (2018-2022)
    
  ggsave("maps/ndvi_change_detection_2018_2022.png", width = 12, height = 6, dpi = 300)
  
  10. Vegetation Classification & Mining Hotspots
  Classify NDVI values into vegetation health categories to identify mining-impacted areas.
  ```{r ndvi-classification-function}
  healthy_veg <- ndvi_2022 >= 0.45
disturbed_veg <- (ndvi_2018 >= 0.45) & (ndvi_2022 < ndvi_2018) & (ndvi_2022 >= 0.25)
bare_soil <- ndvi_2022 < 0.25
water <- ndvi_2022 < 0
active_mining <- (ndvi_2018 >= 0.45) & (ndvi_2022 < 0.25)

class_raster <- ndvi_2022*0
values(class_raster) <- NA
class_raster[healthy_veg] <- 1
class_raster[disturbed_veg] <- 2
class_raster[bare_soil] <- 3
class_raster[water] <- 4
class_raster[active_mining] <- 5

class_small <- aggregate(class_raster, fact=10, fun=modal, na.rm=TRUE)
df_class <- as.data.frame(class_small, xy=TRUE)
df_class <- na.omit(df_class)
colnames(df_class) <- c("x","y","class")

class_colors <- c("1"="forestgreen","2"="goldenrod1","3"="peru","4"="deepskyblue","5"="red")
class_labels <- c("Healthy Vegetation","Disturbed Vegetation","Bare Ground/Non-Forest","Water","Active Mining Areas")

ggplot(df_class) +
geom_raster(aes(x=x, y=y, fill=factor(class))) +
scale_fill_manual(values=class_colors, labels=class_labels, name="Vegetation Class") +
coord_equal() +
labs(title="Forest Vegetation Change Map (2018 → 2022)",
subtitle="Classification of vegetation health, bare ground exposure, and mining hotspots") +
theme(plot.title=element_text(hjust=0.5, size=15, face="bold"),
plot.subtitle=element_text(hjust=0.5, size=11),
axis.text=element_text(size=9),
panel.grid=element_blank())
  ggsave("maps/vegetation_change_classification_2018_2022.png", width=10, height=6, dpi=300)
  ```
  
set.seed(123)

uc_2022 <- unsuperClass(
  img_2022,
  nClasses = 5,
  nStarts = 25
)

uc_2022

uc_map <- uc_2022$map

uc_small <- aggregate(uc_map, fact = 10, fun = modal, na.rm = TRUE)

df_uc <- as.data.frame(uc_small, xy = TRUE, na.rm = TRUE)
colnames(df_uc) <- c("x", "y", "class")

ggplot(df_uc) +
  geom_raster(aes(x = x, y = y, fill = factor(class))) +
  scale_fill_viridis_d(name = "Spectral Class") +
  coord_equal() +
  labs(
    title = "Unsupervised Land Cover Classification (2022)",
    subtitle = "5 spectral clusters (Sentinel-2)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )
ggsave("maps/unsupervised_classification_2022.png", width = 10, height = 6, dpi = 300)
```
  
Conclusion
This analysis demonstrates significant vegetation changes in the PRA River Basin from 2018 to 2022 due to mining activities.
  
