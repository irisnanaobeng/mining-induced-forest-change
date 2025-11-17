Mining-Induced Forest Change in Pra River Basin
================
Iris Nana Obeng
2025-11-17

## 1. Load Required Packages

\#The following packages support raster processing, visualization, and
change analysis: - **terra** — for reading and manipulating Sentinel-2
rasters  
- **ggplot2** — plotting and visualization  
- **patchwork** — arranging plots side-by-side  
- **RStoolbox** — RGB rendering of multispectral imagery

``` r
library(terra)
```

    ## terra 1.8.80

``` r
library(ggplot2)
library(patchwork)
```

    ## 
    ## Attaching package: 'patchwork'

    ## The following object is masked from 'package:terra':
    ## 
    ##     area

``` r
library(RStoolbox)
```

    ## This is version 1.0.2.2 of RStoolbox

## 2. Load Sentinel-2 Bands

### 2017 Bands

``` r
blue_2017  <- rast("./S2A_MSIL2A_20170127T102301_N0500_R065_T30NXM_20230921T203317.SAFE/T30NXM_20170127T102301_B02_10m.jp2")
green_2017 <- rast("./S2A_MSIL2A_20170127T102301_N0500_R065_T30NXM_20230921T203317.SAFE/T30NXM_20170127T102301_B03_10m.jp2")
red_2017   <- rast("./S2A_MSIL2A_20170127T102301_N0500_R065_T30NXM_20230921T203317.SAFE/T30NXM_20170127T102301_B04_10m.jp2")
nir_2017   <- rast("./S2A_MSIL2A_20170127T102301_N0500_R065_T30NXM_20230921T203317.SAFE/T30NXM_20170127T102301_B08_10m.jp2")
```

### 2020 Bands

``` r
blue_2020  <- rast("./S2A_MSIL2A_20200112T102401_N0500_R065_T30NXM_20230424T091251.SAFE/T30NXM_20200112T102401_B02_10m.jp2")
green_2020 <- rast("./S2A_MSIL2A_20200112T102401_N0500_R065_T30NXM_20230424T091251.SAFE/T30NXM_20200112T102401_B03_10m.jp2")
red_2020   <- rast("./S2A_MSIL2A_20200112T102401_N0500_R065_T30NXM_20230424T091251.SAFE/T30NXM_20200112T102401_B04_10m.jp2")
nir_2020   <- rast("./S2A_MSIL2A_20200112T102401_N0500_R065_T30NXM_20230424T091251.SAFE/T30NXM_20200112T102401_B08_10m.jp2")
```

### 2022 Bands

``` r
blue_2022  <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXM_20240506T042828.SAFE/T30NXM_20220126T102209_B02_10m.jp2")
green_2022 <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXM_20240506T042828.SAFE/T30NXM_20220126T102209_B03_10m.jp2")
red_2022   <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXM_20240506T042828.SAFE/T30NXM_20220126T102209_B04_10m.jp2")
nir_2022   <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXM_20240506T042828.SAFE/T30NXM_20220126T102209_B08_10m.jp2")
```

## 3. Create Image Composites

\#RGB = True color \#NIR–R–G = False color (highlights vegetation
health)

``` r
# RGB Stacks (True Color)
rgb_2017 <- c(red_2017, green_2017, blue_2017)
rgb_2020 <- c(red_2020, green_2020, blue_2020)
rgb_2022 <- c(red_2022, green_2022, blue_2022)

# False Color Stacks (NIR-Red-Green)
false_2017 <- c(nir_2017, red_2017, green_2017)
false_2020 <- c(nir_2020, red_2020, green_2020)
false_2022 <- c(nir_2022, red_2022, green_2022)
```

## 4. True Color Visualization

``` r
tc_2017 <- ggRGB(rgb_2017, r=1, g=2, b=3, stretch="lin") +
  ggtitle("2017") +
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

tc_2020 <- ggRGB(rgb_2020, r=1, g=2, b=3, stretch="lin") +
  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

tc_2022 <- ggRGB(rgb_2022, r=1, g=2, b=3, stretch="lin") +
  ggtitle("2022") +
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

true_color_panel <- (tc_2017 | tc_2020 | tc_2022) +
  plot_annotation(
    title = "True Color RGB – Pra River Basin (2017, 2020, 2022)",
    theme = theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))
  )

true_color_panel
```

<img src="forest_change_analysis_files/figure-gfm/true_color_panel-1.png" width="100%" />

``` r
ggsave("maps/true_color_panel.png", true_color_panel, width = 15, height = 6, dpi = 300)
```

## 5. False Color Visualization

\#False color imagery uses NIR to display vegetation health. Healthy
vegetation appears bright red; stressed vegetation appears dull.

``` r
fc_2017 <- ggRGB(false_2017, r=1, g=2, b=3, stretch="lin") +
  ggtitle("2017") +
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

fc_2020 <- ggRGB(false_2020, r=1, g=2, b=3, stretch="lin") +
  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

fc_2022 <- ggRGB(false_2022, r=1, g=2, b=3, stretch="lin") +
  ggtitle("2022") +
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

false_color_panel <- (fc_2017 | fc_2020 | fc_2022) +
  plot_annotation(
    title = "False Color (NIR-R-G): Vegetation Health",
    theme = theme(plot.title = element_text(hjust=0.5, size=18, face="bold"))
  )

false_color_panel
```

<img src="forest_change_analysis_files/figure-gfm/false_color_panel-1.png" width="100%" />

``` r
ggsave("maps/false_color_panel.png", false_color_panel, width = 15, height = 6, dpi = 300)
```

## 6. NDVI Calculation and Analysis

\#NDVI = (NIR – Red) / (NIR + Red) This index quantifies vegetation
greenness and essential for detecting forest loss or stress.

``` r
# Calculate NDVI
ndvi_2017 <- (nir_2017 - red_2017) / (nir_2017 + red_2017)
```

    ## |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          

``` r
ndvi_2020 <- (nir_2020 - red_2020) / (nir_2020 + red_2020)
```

    ## |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          

``` r
ndvi_2022 <- (nir_2022 - red_2022) / (nir_2022 + red_2022)
```

    ## |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          

``` r
# Downsample for memory management
ndvi_2017s <- aggregate(ndvi_2017, fact=10)
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
ndvi_2020s <- aggregate(ndvi_2020, fact=10)
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
ndvi_2022s <- aggregate(ndvi_2022, fact=10)
```

    ## |---------|---------|---------|---------|=========================================                                          

## 7. Change Detection Analysis

\#-New exposed soil (mining expansion) \#-Degraded / disturbed
vegetation \#-No change

\#Thresholds: -Bare soil: NDVI \< 0.25 -Healthy forest: NDVI \> 0.45

``` r
# Identify vegetation changes
bare17 <- ndvi_2017 < 0.25
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
bare22 <- ndvi_2022 < 0.25
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
new_bare <- bare22 & (!bare17)
```

    ## |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          

``` r
# Degraded / disturbed vegetation (drop from healthy to stressed)
degraded <- (ndvi_2022 < ndvi_2017) & 
            (ndvi_2022 >= 0.25) & 
            (ndvi_2017 >= 0.45)
```

    ## |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          |---------|---------|---------|---------|=========================================                                          

``` r
# Create change classification
change <- ndvi_2022 * 0
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
change[] <- 0
change[new_bare] <- 1
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
change[degraded] <- 2
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
# Downsample for visualization
change_small <- aggregate(change, fact = 10, fun = modal, na.rm = TRUE)
```

## 8. Final Change Map

``` r
# Prepare data for plotting
change_df <- as.data.frame(change_small, xy = TRUE)
colnames(change_df) <- c("x", "y", "class")

change_df$class <- factor(change_df$class,
                          levels = c(0, 1, 2),
                          labels = c("No Change",
                                     "Exposed Soil (Mining)",
                                     "Degraded / Disturbed Vegetation"))


# ---Plot change map ---
change_map <- ggplot(change_df, aes(x = x, y = y, fill = class)) +
  geom_raster() +
  scale_fill_manual(values = c(
    "No Change" = "honeydew1",
    "Exposed Soil (Mining)" = "red",
    "Degraded / Disturbed Vegetation" = "goldenrod1"
  )) +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Mining-Induced Forest Vegetation Change (2017 → 2022)",
    subtitle = "Pra River Basin — Exposed Soil & Disturbed Vegetation",
    fill = "Class"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

change_map
```

![](forest_change_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggsave("maps/change_map_3class.png", plot = change_map, width = 10, height = 7, dpi = 300)
```

## Results Summary

This analysis successfully identifies three key change classes:

- **Mining-Induced Bare Soil (Red)**: Areas of active mining showing
  complete vegetation removal.

- **Degraded/Disturbed Forest (Yellow)**: Forest areas under stress from
  mining activities; reflects indirect mining impacts such as soil and
  water pollution, partial canopy loss, or nearby mining disturbance.

- **No Change (Light Cream)**: Stable forest vegetation unaffected by
  mining

The workflow demonstrates the effectiveness of Sentinel-2 NDVI analysis
for mapping mining impacts on forest ecosystems.
