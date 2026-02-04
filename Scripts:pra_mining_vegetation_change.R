############################################################
# Mining-Induced Forest Vegetation Change
# PRA River Basin using Sentinel-2 Imagery
#
# Author: Iris Nana Obeng
# Date: 2025-12-15
############################################################


# Create output directory for maps if it doesn't exist
if (!dir.exists("maps")) dir.create("maps")

set.seed(123)


############################
# 1. Load Required Packages
############################

library(terra)
library(ggplot2)
library(patchwork)
library(scales)
library(RStoolbox)
library(sf)
library(dplyr)


############################################################
# 2. Custom RGB Visualization Function (ggplot-based)
############################################################

im.ggplotRGB <- function(img, r = 1, g = 2, b = 3,
                         stretch = TRUE, downsample = 6,
                         show_axes = TRUE,
                         title = "RGB Composite") {
  
  if (nlyr(img) < 3) {
    stop("Input raster must have at least 3 bands.")
  }
  
  # Downsample for speed
  img_small <- terra::aggregate(img, fact = downsample)
  df <- as.data.frame(img_small, xy = TRUE, na.rm = TRUE)
  names(df)[3:5] <- c("R", "G", "B")
  
  # Linear stretch
  if (stretch) {
    df$R <- scales::rescale(df$R, to = c(0, 1))
    df$G <- scales::rescale(df$G, to = c(0, 1))
    df$B <- scales::rescale(df$B, to = c(0, 1))
  }
  
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_raster(aes(fill = rgb(R, G, B))) +
    scale_fill_identity() +
    coord_equal() +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid = element_blank()
    )
  
  if (!show_axes) {
    p <- p + theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  }
  
  return(p)
}


############################################################
# 3. Single-band Raster Visualization Function (NDVI)
############################################################

plot_singleband_gg <- function(r, downsample = 8, title = "") {
  r_small <- terra::aggregate(r, fact = downsample)
  df <- as.data.frame(r_small, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "val"
  
  ggplot(df, aes(x = x, y = y, fill = val)) +
    geom_raster() +
    scale_fill_viridis_c(option = "D", na.value = "transparent") +
    coord_equal() +
    labs(title = title, fill = "NDVI") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}


############################################################
# 4. Load Sentinel-2 Imagery
############################################################

# ---- 2018 (Pre-mining baseline) ----
blue_2018  <- rast("S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B02_10m.jp2")
green_2018 <- rast("S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B03_10m.jp2")
red_2018   <- rast("S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B04_10m.jp2")
nir_2018   <- rast("S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B08_10m.jp2")

# ---- 2020 (Intermediate monitoring) ----
blue_2020  <- rast("S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B02_10m.jp2")
green_2020 <- rast("S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B03_10m.jp2")
red_2020   <- rast("S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B04_10m.jp2")
nir_2020   <- rast("S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B08_10m.jp2")

# ---- 2022 (Post-mining assessment) ----
blue_2022  <- rast("S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B02_10m.jp2")
green_2022 <- rast("S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B03_10m.jp2")
red_2022   <- rast("S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B04_10m.jp2")
nir_2022   <- rast("S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B08_10m.jp2")


############################################################
# 5. Create RGB Composites
############################################################

# True color (R-G-B)
tc_2018 <- c(red_2018, green_2018, blue_2018)
tc_2020 <- c(red_2020, green_2020, blue_2020)
tc_2022 <- c(red_2022, green_2022, blue_2022)

# False color (NIR-R-G)
fc_2018 <- c(nir_2018, red_2018, green_2018)
fc_2020 <- c(nir_2020, red_2020, green_2020)
fc_2022 <- c(nir_2022, red_2022, green_2022)


############################################################
# 6. True Color Visualization
############################################################

p_tc_2018 <- im.ggplotRGB(tc_2018, title = "True Colour 2018")
p_tc_2020 <- im.ggplotRGB(tc_2020, title = "True Colour 2020")
p_tc_2022 <- im.ggplotRGB(tc_2022, title = "True Colour 2022")

combined_true <- (p_tc_2018 | p_tc_2020 | p_tc_2022) +
  plot_annotation(
    title = "True Colour Composites – PRA Basin (2018–2022)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )

ggsave("maps/true_color_comparison.png", combined_true,
       width = 15, height = 6, dpi = 300)


############################################################
# 7. False Color Visualization
############################################################

p_fc_2018 <- im.ggplotRGB(fc_2018, title = "False Colour 2018")
p_fc_2020 <- im.ggplotRGB(fc_2020, title = "False Colour 2020")
p_fc_2022 <- im.ggplotRGB(fc_2022, title = "False Colour 2022")

combined_false <- (p_fc_2018 | p_fc_2020 | p_fc_2022) +
  plot_annotation(
    title = "False Colour Composites – PRA Basin (2018–2022)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )

ggsave("maps/false_color_comparison.png", combined_false,
       width = 15, height = 6, dpi = 300)


############################################################
# 8. NDVI Calculation
############################################################

ndvi_calc <- function(nir, red) {
  nd <- (nir - red) / (nir + red)
  names(nd) <- "NDVI"
  nd
}

ndvi_2018 <- ndvi_calc(nir_2018, red_2018)
ndvi_2020 <- ndvi_calc(nir_2020, red_2020)
ndvi_2022 <- ndvi_calc(nir_2022, red_2022)


############################################################
# 9. NDVI Visualization
############################################################

p_ndvi_2018 <- plot_singleband_gg(ndvi_2018, title = "NDVI 2018")
p_ndvi_2020 <- plot_singleband_gg(ndvi_2020, title = "NDVI 2020")
p_ndvi_2022 <- plot_singleband_gg(ndvi_2022, title = "NDVI 2022")

combined_ndvi <- (p_ndvi_2018 | p_ndvi_2020 | p_ndvi_2022) +
  plot_annotation(
    title = "NDVI Comparison – PRA Basin (2018–2022)",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

ggsave("maps/ndvi_comparison.png", combined_ndvi,
       width = 15, height = 6, dpi = 300)


############################################################
# 10. NDVI Change Detection (2018–2022)
############################################################

ndvi_diff_18_22 <- ndvi_2022 - ndvi_2018

p_ndvi_diff <- plot_singleband_gg(
  ndvi_diff_18_22,
  title = "NDVI Change (2022 − 2018)"
)

ggsave("maps/ndvi_change_2018_2022.png",
       p_ndvi_diff, width = 10, height = 6, dpi = 300)


############################################################
# 11. Vegetation & Mining Classification
############################################################

healthy_veg    <- ndvi_2022 >= 0.45
disturbed_veg  <- (ndvi_2018 >= 0.45) & (ndvi_2022 < ndvi_2018) & (ndvi_2022 >= 0.25)
bare_soil      <- ndvi_2022 < 0.25
water          <- ndvi_2022 < 0
active_mining  <- (ndvi_2018 >= 0.45) & (ndvi_2022 < 0.25)

class_raster <- ndvi_2022 * NA
class_raster[healthy_veg]   <- 1
class_raster[disturbed_veg] <- 2
class_raster[bare_soil]     <- 3
class_raster[water]         <- 4
class_raster[active_mining] <- 5

class_small <- aggregate(class_raster, fact = 10, fun = modal, na.rm = TRUE)
df_class <- as.data.frame(class_small, xy = TRUE, na.rm = TRUE)
colnames(df_class) <- c("x", "y", "class")

class_colors <- c(
  "1" = "forestgreen",
  "2" = "goldenrod1",
  "3" = "peru",
  "4" = "deepskyblue",
  "5" = "red"
)

class_labels <- c(
  "Healthy Vegetation",
  "Disturbed Vegetation",
  "Bare Ground / Non-Forest",
  "Water",
  "Active Mining Areas"
)

p_class <- ggplot(df_class) +
  geom_raster(aes(x = x, y = y, fill = factor(class))) +
  scale_fill_manual(values = class_colors,
                    labels = class_labels,
                    name = "Vegetation Class") +
  coord_equal() +
  labs(
    title = "Forest Vegetation Change (2018 → 2022)",
    subtitle = "Mining-induced disturbance detection"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )

ggsave("maps/vegetation_change_classification_2018_2022.png",
       p_class, width = 10, height = 6, dpi = 300)


############################################################
# 12. Unsupervised Classification (2022)
############################################################

img_2022 <- c(blue_2022, green_2022, red_2022, nir_2022)

uc_2022 <- unsuperClass(
  img_2022,
  nClasses = 5,
  nStarts = 25
)

uc_map <- uc_2022$map
uc_small <- aggregate(uc_map, fact = 10, fun = modal, na.rm = TRUE)

df_uc <- as.data.frame(uc_small, xy = TRUE, na.rm = TRUE)
colnames(df_uc) <- c("x", "y", "class")

p_uc <- ggplot(df_uc) +
  geom_raster(aes(x = x, y = y, fill = factor(class))) +
  scale_fill_viridis_d(name = "Spectral Class") +
  coord_equal() +
  labs(
    title = "Unsupervised Land Cover Classification (2022)",
    subtitle = "Sentinel-2 spectral clustering"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )

ggsave("maps/unsupervised_classification_2022.png",
       p_uc, width = 10, height = 6, dpi = 300)


############################################################
# END OF SCRIPT
############################################################