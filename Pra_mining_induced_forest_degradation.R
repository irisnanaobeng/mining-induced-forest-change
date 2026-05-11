# Mining-Induced Forest Degradation in Ghana’s Pra River Basin
# Multi-Temporal Remote Sensing Analysis
# Author: Iris Nana Obeng
# Date: 2026-05-10

# ============================================================
# 1. SETUP
# ============================================================

# Consistent color palette for all plots
palette <- list(
  mining = "#FF0000",
  healthy_forest = "#1B7837",
  disturbed_forest = "#FFC125",
  bare_soil = "#C7A76C",
  water_bodies = "#4F94CD",
  no_change = "#C1CDC1",
  decrease = "#B2182B",
  increase = "#008B45",
  africa_fill = "#F4A460",
  study_area = "#B04A65",
  ghana_bg = "#FFE4B5"
)

# Create output directory
if (!dir.exists("maps")) dir.create("maps")

# Load libraries
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(terra)
library(ggplot2)
library(patchwork)
library(scales)
library(RStoolbox)
library(sf)
library(dplyr)
library(viridis)
library(MASS)
library(grid)


# ============================================================
# 2. STUDY AREA MAP
# ============================================================

# Ghana and Africa boundary data
ghana <- ne_states(country = "Ghana", returnclass = "sf")
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Pra River Basin approximate bounding box
pra_bbox <- st_bbox(
  c(xmin = -1.5, xmax = -0.5,
    ymin = 5.5, ymax = 6.5),
  crs = st_crs(4326)
)
pra_basin <- st_as_sfc(pra_bbox)

# Major cities
cities <- data.frame(
  city = c("Accra", "Kumasi", "Cape Coast", "Takoradi"),
  lon = c(-0.19, -1.62, -1.28, -1.76),
  lat = c(5.60, 6.69, 5.13, 4.90)
)

ghana_fill <- "#FFE4B5"
basin_fill <- "#B04A65"
basin_outline <- "#7A0019"
africa_fill <- "#F4A460"

# Inset map
inset_map <- ggplot() +
  geom_sf(data = africa, fill = africa_fill, color = "grey70", linewidth = 0.2) +
  geom_sf(data = ghana, fill = basin_outline, color = "black", linewidth = 0.25) +
  coord_sf(xlim = c(-20, 60), ylim = c(-40, 40), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.6),
    plot.margin = margin(3, 3, 3, 3)
  )

# Main Ghana map
main_map <- ggplot() +
  geom_sf(data = ghana, fill = ghana_fill, color = "grey65", linewidth = 0.35) +
  geom_sf(
    data = pra_basin,
    fill = basin_fill,
    alpha = 0.65,
    color = basin_outline,
    linewidth = 0.9
  ) +
  geom_point(
    data = cities,
    aes(x = lon, y = lat),
    shape = 21,
    size = 3.2,
    fill = "white",
    color = "#7A0019",
    stroke = 0.8
  ) +
  geom_text(
    data = cities,
    aes(x = lon, y = lat, label = city),
    nudge_y = 0.18,
    size = 3.6
  ) +
  annotate(
    "text",
    x = -1.05, y = 6.35,
    label = "Pra River Basin",
    size = 5,
    fontface = "bold",
    color = basin_outline
  ) +
  annotation_scale(location = "bl", width_hint = 0.28) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  labs(
    title = "Study Area: Pra River Basin, Ghana",
    subtitle = "Context map for mining impact analysis",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(xlim = c(-3.5, 1.5), ylim = c(4.5, 11.5), expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 8)),
    panel.grid = element_line(color = "grey88", linewidth = 0.4),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

final_map <- main_map +
  inset_element(inset_map, left = 0.68, bottom = 0.64, right = 0.98, top = 0.98)

print(final_map)

ggsave(
  "maps/pra_river_basin_ghana_location.png",
  final_map,
  width = 16,
  height = 10,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 3. LOAD SENTINEL-2 BANDS
# ============================================================

# 2018 bands
blue_2018  <- rast("./S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B02_10m.jp2")
green_2018 <- rast("./S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B03_10m.jp2")
red_2018   <- rast("./S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B04_10m.jp2")
nir_2018   <- rast("./S2A_MSIL2A_20180112T102401_N0500_R065_T30NXN_20230717T153523.SAFE/T30NXN_20180112T102401_B08_10m.jp2")

# 2020 bands
blue_2020  <- rast("./S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B02_10m.jp2")
green_2020 <- rast("./S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B03_10m.jp2")
red_2020   <- rast("./S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B04_10m.jp2")
nir_2020   <- rast("./S2A_MSIL2A_20200102T102421_N0500_R065_T30NXN_20230425T023320.SAFE/T30NXN_20200102T102421_B08_10m.jp2")

# 2022 bands
blue_2022  <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B02_10m.jp2")
green_2022 <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B03_10m.jp2")
red_2022   <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B04_10m.jp2")
nir_2022   <- rast("./S2B_MSIL2A_20220126T102209_N0510_R065_T30NXN_20240506T042828.SAFE/T30NXN_20220126T102209_B08_10m.jp2")


# ============================================================
# 4. CREATE RGB COMPOSITES
# ============================================================

# True Color composites: Red-Green-Blue
tc_2018 <- c(red_2018, green_2018, blue_2018)
tc_2020 <- c(red_2020, green_2020, blue_2020)
tc_2022 <- c(red_2022, green_2022, blue_2022)

# False Color composites: NIR-Red-Green
fc_2018 <- c(nir_2018, red_2018, green_2018)
fc_2020 <- c(nir_2020, red_2020, green_2020)
fc_2022 <- c(nir_2022, red_2022, green_2022)


# ============================================================
# 5. CUSTOM FUNCTIONS
# ============================================================

im.ggplotRGB <- function(img, r = 1, g = 2, b = 3,
                         stretch = TRUE, downsample = 6,
                         show_axes = TRUE, title = "True Colour") {
  
  if (nlyr(img) < 3) stop("img must be a 3-band SpatRaster.")
  
  img_small <- terra::aggregate(img, fact = downsample)
  df <- as.data.frame(img_small, xy = TRUE, na.rm = TRUE)
  names(df)[3:5] <- c("R", "G", "B")
  
  if (stretch) {
    df$R <- scales::rescale(df$R, to = c(0, 1))
    df$G <- scales::rescale(df$G, to = c(0, 1))
    df$B <- scales::rescale(df$B, to = c(0, 1))
  }
  
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_raster(aes(fill = rgb(R, G, B))) +
    scale_fill_identity() +
    coord_equal() +
    ggtitle(title) +
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

ndvi_change_class <- function(ndvi_diff) {
  class_r <- ndvi_diff * 0
  class_r[ndvi_diff > 0.1]  <- 1
  class_r[ndvi_diff < -0.1] <- 2
  return(class_r)
}

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

prepare_raster_df <- function(raster_layer, downsample_factor = 12) {
  raster_small <- terra::aggregate(raster_layer, fact = downsample_factor, fun = mean)
  df <- as.data.frame(raster_small, xy = TRUE, na.rm = TRUE)
  value_col <- setdiff(names(df), c("x", "y"))[1]
  names(df)[names(df) == value_col] <- "value"
  return(df)
}


# ============================================================
# 6. TRUE COLOR AND FALSE COLOR VISUALIZATION
# ============================================================

p_tc_2018 <- im.ggplotRGB(tc_2018, title = "True Colour 2018")
p_tc_2020 <- im.ggplotRGB(tc_2020, title = "True Colour 2020")
p_tc_2022 <- im.ggplotRGB(tc_2022, title = "True Colour 2022")

combined_true_color <- (p_tc_2018 | p_tc_2020 | p_tc_2022) +
  plot_annotation(
    title = "True Colour Composites Pra River Basin Forest (2018, 2020, 2022)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  ) &
  theme(plot.margin = margin(5, 5, 5, 5))

print(combined_true_color)

ggsave(
  "maps/true_color_comparison.png",
  combined_true_color,
  width = 18,
  height = 6,
  dpi = 300,
  bg = "white"
)

p_fc_2018 <- im.ggplotRGB(fc_2018, title = "False Colour 2018")
p_fc_2020 <- im.ggplotRGB(fc_2020, title = "False Colour 2020")
p_fc_2022 <- im.ggplotRGB(fc_2022, title = "False Colour 2022")

combined_false_color <- (p_fc_2018 | p_fc_2020 | p_fc_2022) +
  plot_annotation(
    title = "False Colour Composites Pra River Basin Forest (2018, 2020, 2022)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  ) &
  theme(plot.margin = margin(5, 5, 5, 5))

print(combined_false_color)

ggsave(
  "maps/false_color_comparison.png",
  combined_false_color,
  width = 18,
  height = 6,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 7. NDVI CALCULATION
# ============================================================

ndvi_calc <- function(nir, red) {
  nd <- (nir - red) / (nir + red)
  names(nd) <- "NDVI"
  return(nd)
}

ndvi_2018 <- ndvi_calc(nir_2018, red_2018)
ndvi_2020 <- ndvi_calc(nir_2020, red_2020)
ndvi_2022 <- ndvi_calc(nir_2022, red_2022)


# ============================================================
# 8. NDVI-BASED LAND COVER CLASSIFICATION
# ============================================================

im.classify <- function(ndvi_current, ndvi_previous = NULL,
                        water_threshold = 0.05,
                        mining_threshold = 0.30,
                        healthy_threshold = 0.40,
                        disturbed_low = 0.25,
                        disturbed_high = 0.40) {
  
  class_raster <- ndvi_current * 0
  values(class_raster) <- NA
  
  water_mask <- ndvi_current < water_threshold
  class_raster[water_mask] <- 1
  cat("Water pixels:", global(water_mask, "sum", na.rm = TRUE)[1, 1], "\n")
  
  if (!is.null(ndvi_previous)) {
    mining_mask <- (ndvi_current < mining_threshold & ndvi_previous > 0.45) & (!water_mask)
    class_raster[mining_mask] <- 2
    cat("Active mining pixels:", global(mining_mask, "sum", na.rm = TRUE)[1, 1], "\n")
  } else {
    mining_mask <- ndvi_current * 0
    mining_mask[] <- FALSE
  }
  
  healthy_mask <- (ndvi_current >= healthy_threshold) & (!water_mask) & (!mining_mask)
  class_raster[healthy_mask] <- 3
  cat("Healthy vegetation pixels:", global(healthy_mask, "sum", na.rm = TRUE)[1, 1], "\n")
  
  disturbed_mask <- (ndvi_current >= disturbed_low & ndvi_current < disturbed_high) &
    (!water_mask) & (!mining_mask) & (!healthy_mask)
  class_raster[disturbed_mask] <- 4
  cat("Disturbed vegetation pixels:", global(disturbed_mask, "sum", na.rm = TRUE)[1, 1], "\n")
  
  bare_mask <- (ndvi_current >= water_threshold & ndvi_current < disturbed_low) &
    (!water_mask) & (!mining_mask) & (!healthy_mask) & (!disturbed_mask)
  class_raster[bare_mask] <- 5
  cat("Bare soil pixels:", global(bare_mask, "sum", na.rm = TRUE)[1, 1], "\n")
  
  unclassified <- is.na(values(class_raster))
  cat("Unclassified pixels:", sum(unclassified, na.rm = TRUE), "\n")
  
  names(class_raster) <- "classification"
  return(class_raster)
}

cat(">>> RUNNING CLASSIFICATION <<< \n")
vegetation_classes <- im.classify(ndvi_current = ndvi_2022, ndvi_previous = ndvi_2018)
cat(">>> CLASSIFICATION COMPLETE <<< \n")
print(vegetation_classes)


# ============================================================
# 9. SPECTRAL STACKS
# ============================================================

spec_stack_2018 <- c(blue_2018, green_2018, red_2018, nir_2018)
names(spec_stack_2018) <- c("Blue", "Green", "Red", "NIR")

spec_stack_2020 <- c(blue_2020, green_2020, red_2020, nir_2020)
names(spec_stack_2020) <- c("Blue", "Green", "Red", "NIR")

spec_stack_2022 <- c(blue_2022, green_2022, red_2022, nir_2022)
names(spec_stack_2022) <- c("Blue", "Green", "Red", "NIR")

spec_small_2018 <- terra::aggregate(spec_stack_2018, fact = 10)
spec_small_2020 <- terra::aggregate(spec_stack_2020, fact = 10)
spec_small_2022 <- terra::aggregate(spec_stack_2022, fact = 10)

print(sprintf("Resolution reduced to: %.1f meters", terra::res(spec_small_2018)[1]))


# ============================================================
# 10. FUZZY CLASSIFICATION FUNCTIONS
# ============================================================

im.fuzzy <- function(input_image, num_clusters = 3, seed = 42, m = 2, do_plot = FALSE) {
  
  if (!inherits(input_image, "SpatRaster")) stop("Input must be a SpatRaster object")
  
  vals <- terra::as.matrix(input_image)
  valid_idx <- complete.cases(vals)
  vals <- vals[valid_idx, , drop = FALSE]
  
  if (nrow(vals) == 0) stop("No valid pixels found")
  
  for (i in 1:ncol(vals)) {
    min_val <- min(vals[, i], na.rm = TRUE)
    max_val <- max(vals[, i], na.rm = TRUE)
    if (max_val > min_val) {
      vals[, i] <- 255 * (vals[, i] - min_val) / (max_val - min_val)
    } else {
      vals[, i] <- 0
    }
  }
  
  set.seed(seed)
  km_result <- kmeans(vals, centers = num_clusters)
  centers <- km_result$centers
  
  dist_matrix <- matrix(NA, nrow(vals), num_clusters)
  for (k in 1:num_clusters) {
    diff_matrix <- sweep(vals, 2, centers[k, ], "-")
    dist_matrix[, k] <- sqrt(rowSums(diff_matrix^2))
  }
  
  exponent <- 2 / (m - 1)
  membership_matrix <- matrix(NA, nrow(vals), num_clusters)
  
  for (i in 1:nrow(dist_matrix)) {
    distances <- dist_matrix[i, ]
    if (any(distances == 0)) {
      membership <- rep(0, num_clusters)
      exact_matches <- which(distances == 0)
      membership[exact_matches] <- 1 / length(exact_matches)
    } else {
      ratio_matrix <- outer(distances, distances, "/")
      membership <- 1 / rowSums(ratio_matrix^exponent)
    }
    membership_matrix[i, ] <- membership
  }
  
  template_raster <- input_image[[1]]
  distance_rasters <- list()
  membership_rasters <- list()
  
  for (k in 1:num_clusters) {
    dist_raster <- template_raster
    dist_values <- rep(NA, terra::ncell(template_raster))
    dist_values[valid_idx] <- dist_matrix[, k]
    terra::values(dist_raster) <- dist_values
    distance_rasters[[k]] <- dist_raster
    
    mem_raster <- template_raster
    mem_values <- rep(NA, terra::ncell(template_raster))
    mem_values[valid_idx] <- membership_matrix[, k]
    terra::values(mem_raster) <- mem_values
    membership_rasters[[k]] <- mem_raster
  }
  
  distance_stack <- terra::rast(distance_rasters)
  names(distance_stack) <- paste0("class_", 1:num_clusters, "_distance")
  
  membership_stack <- terra::rast(membership_rasters)
  names(membership_stack) <- paste0("class_", 1:num_clusters, "_membership")
  
  return(list(
    distances = distance_stack,
    memberships = membership_stack,
    centers = centers,
    raw_distances = dist_matrix,
    scaled_values = vals
  ))
}

im.fuzzy.fixed <- function(input_image, fixed_centers, m = 2, do_plot = FALSE) {
  
  vals <- terra::as.matrix(input_image)
  valid_idx <- complete.cases(vals)
  vals <- vals[valid_idx, , drop = FALSE]
  
  if (nrow(vals) == 0) stop("No valid pixels found")
  
  for (i in 1:ncol(vals)) {
    min_val <- min(vals[, i], na.rm = TRUE)
    max_val <- max(vals[, i], na.rm = TRUE)
    if (max_val > min_val) {
      vals[, i] <- 255 * (vals[, i] - min_val) / (max_val - min_val)
    } else {
      vals[, i] <- 0
    }
  }
  
  centers <- fixed_centers
  num_clusters <- nrow(centers)
  
  dist_matrix <- matrix(NA, nrow(vals), num_clusters)
  for (k in 1:num_clusters) {
    diff_matrix <- sweep(vals, 2, centers[k, ], "-")
    dist_matrix[, k] <- sqrt(rowSums(diff_matrix^2))
  }
  
  exponent <- 2 / (m - 1)
  membership_matrix <- matrix(NA, nrow(vals), num_clusters)
  
  for (i in 1:nrow(dist_matrix)) {
    distances <- dist_matrix[i, ]
    if (any(distances == 0)) {
      membership <- rep(0, num_clusters)
      exact_matches <- which(distances == 0)
      membership[exact_matches] <- 1 / length(exact_matches)
    } else {
      ratio_matrix <- outer(distances, distances, "/")
      membership <- 1 / rowSums(ratio_matrix^exponent)
    }
    membership_matrix[i, ] <- membership
  }
  
  template_raster <- input_image[[1]]
  distance_rasters <- list()
  membership_rasters <- list()
  
  for (k in 1:num_clusters) {
    dist_raster <- template_raster
    dist_values <- rep(NA, terra::ncell(template_raster))
    dist_values[valid_idx] <- dist_matrix[, k]
    terra::values(dist_raster) <- dist_values
    distance_rasters[[k]] <- dist_raster
    
    mem_raster <- template_raster
    mem_values <- rep(NA, terra::ncell(template_raster))
    mem_values[valid_idx] <- membership_matrix[, k]
    terra::values(mem_raster) <- mem_values
    membership_rasters[[k]] <- mem_raster
  }
  
  distance_stack <- terra::rast(distance_rasters)
  names(distance_stack) <- paste0("class_", 1:num_clusters, "_distance")
  
  membership_stack <- terra::rast(membership_rasters)
  names(membership_stack) <- paste0("class_", 1:num_clusters, "_membership")
  
  if (do_plot) {
    terra::plot(
      membership_stack,
      main = "Fuzzy Membership Maps (Fixed Centroids)",
      col = viridis::viridis(100)
    )
  }
  
  return(list(
    distances = distance_stack,
    memberships = membership_stack,
    centers = centers,
    raw_distances = dist_matrix,
    scaled_values = vals
  ))
}

print("Fuzzy classification")


# ============================================================
# 11. FUZZY CLASSIFICATION IMPLEMENTATION
# ============================================================

extract_distances <- function(fuzzy_result, year_label, class_idx, n_samples) {
  dists <- fuzzy_result$raw_distances[, class_idx]
  if (length(dists) > n_samples) {
    set.seed(42)
    dists <- sample(dists, n_samples)
  }
  data.frame(Year = year_label, Distance = dists)
}

print("Step 1: Computing 2018 baseline centroids...")
fuzzy_2018 <- im.fuzzy(spec_small_2018, num_clusters = 3, seed = 42, do_plot = FALSE)
centroids_2018 <- fuzzy_2018$centers

write.csv(centroids_2018, "centroids_2018.csv", row.names = FALSE)
print("2018 centroids saved to 'centroids_2018.csv'")

print("Step 2: Applying 2018 centroids to 2020 and 2022...")
fuzzy_2020 <- im.fuzzy.fixed(spec_small_2020, fixed_centers = centroids_2018, do_plot = FALSE)
fuzzy_2022 <- im.fuzzy.fixed(spec_small_2022, fixed_centers = centroids_2018, do_plot = FALSE)

print("Temporal analysis complete for all three years")


# ============================================================
# 12. MINING AND FOREST CLASS IDENTIFICATION
# ============================================================

red_nir_ratio <- centroids_2018[, 3] / centroids_2018[, 4]

centroid_analysis <- data.frame(
  Class = 1:3,
  Red = centroids_2018[, 3],
  NIR = centroids_2018[, 4]
)

centroid_analysis$NDVI <- (centroid_analysis$NIR - centroid_analysis$Red) /
  (centroid_analysis$NIR + centroid_analysis$Red)

mining_class <- which.max(red_nir_ratio)
forest_class <- which.max(centroid_analysis$NDVI)

centroid_df <- data.frame(
  Class = 1:3,
  Blue = round(centroids_2018[, 1], 1),
  Green = round(centroids_2018[, 2], 1),
  Red = round(centroids_2018[, 3], 1),
  NIR = round(centroids_2018[, 4], 1),
  Red_NIR_Ratio = round(red_nir_ratio, 3),
  Interpretation = "Undetermined"
)

for (i in 1:3) {
  if (i == mining_class) {
    centroid_df$Interpretation[i] <- "Mining/Bare Soil"
  } else if (i == forest_class) {
    centroid_df$Interpretation[i] <- "Healthy Vegetation"
  } else {
    centroid_df$Interpretation[i] <- "Disturbed Vegetation"
  }
}

print("Spectral Characteristics of Each Class:")
print(centroid_df)
print(paste("Mining class is Class", mining_class))
print(paste("Forest class is Class", forest_class))

write.csv(centroid_df, "centroid_class_interpretation.csv", row.names = FALSE)


# ============================================================
# 13. NDVI TIME SERIES
# ============================================================

p_ndvi_2018 <- plot_singleband_gg(ndvi_2018, title = "NDVI 2018")
p_ndvi_2020 <- plot_singleband_gg(ndvi_2020, title = "NDVI 2020")
p_ndvi_2022 <- plot_singleband_gg(ndvi_2022, title = "NDVI 2022")

combined_ndvi <- (p_ndvi_2018 | p_ndvi_2020 | p_ndvi_2022) +
  plot_annotation(
    title = "NDVI Time Series Pra River Basin Forest (2018, 2020, 2022)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  ) &
  theme(plot.margin = margin(5, 5, 5, 5), legend.position = "right")

print(combined_ndvi)

ggsave(
  "maps/ndvi_timeseries.png",
  combined_ndvi,
  width = 18,
  height = 8,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 14. NDVI CHANGE DETECTION AND BAR CHART
# ============================================================

ndvi_diff_18_22 <- ndvi_2022 - ndvi_2018

ndvi_change_18_22_class <- ndvi_change_class(ndvi_diff_18_22)
ndvi_change_discrete <- terra::aggregate(ndvi_change_18_22_class, fact = 8)

df_change <- as.data.frame(ndvi_change_discrete, xy = TRUE, na.rm = TRUE)
colnames(df_change) <- c("x", "y", "class")

df_change$class <- factor(
  df_change$class,
  levels = c(0, 1, 2),
  labels = c("No Change", "Vegetation Increase", "Vegetation Decrease")
)

p_ndvi_diff_18_22 <- plot_singleband_gg(ndvi_diff_18_22, title = "NDVI Difference (2022–2018)") +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "#F2F2F2",
    high = "#008B45",
    midpoint = 0,
    limits = c(-0.6, 0.4),
    name = expression(Delta * "NDVI")
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "#000000"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_line(color = "grey90", linewidth = 0.3),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

ndvi_legend <- ggplot() +
  annotate(
    "rect",
    xmin = 0, xmax = 1,
    ymin = 0.08, ymax = 0.92,
    fill = "white",
    color = "grey65",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = 0.08, y = 0.82,
    label = "NDVI Change Interpretation",
    hjust = 0,
    fontface = "bold",
    size = 4.2
  ) +
  annotate(
    "rect",
    xmin = 0.08, xmax = 0.16,
    ymin = 0.60, ymax = 0.68,
    fill = "#B2182B",
    color = "grey40"
  ) +
  annotate("text", x = 0.20, y = 0.64, label = "Vegetation decrease", hjust = 0, size = 3.7) +
  annotate(
    "rect",
    xmin = 0.08, xmax = 0.16,
    ymin = 0.42, ymax = 0.50,
    fill = "#F2F2F2",
    color = "grey40"
  ) +
  annotate("text", x = 0.20, y = 0.48, label = "Little or no change", hjust = 0, size = 3.7) +
  annotate(
    "rect",
    xmin = 0.08, xmax = 0.16,
    ymin = 0.24, ymax = 0.32,
    fill = "#008B45",
    color = "grey40"
  ) +
  annotate("text", x = 0.20, y = 0.28, label = "Vegetation increase", hjust = 0, size = 3.7) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void()

map_with_legend <- p_ndvi_diff_18_22 | ndvi_legend +
  plot_layout(widths = c(3.2, 1))

ndvi_change_clean <- df_change[!is.na(df_change$class), ]
class_counts <- table(ndvi_change_clean$class)

ndvi_change_summary <- data.frame(
  class = factor(
    names(class_counts),
    levels = c("Vegetation Decrease", "No Change", "Vegetation Increase")
  ),
  pixel_count = as.numeric(class_counts),
  percentage = as.numeric(class_counts) / sum(class_counts) * 100
)

ndvi_change_bar <- ggplot(
  ndvi_change_summary,
  aes(x = class, y = percentage, fill = class)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    vjust = -0.4,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Vegetation Decrease" = "#B2182B",
      "No Change" = "#C1CDC1",
      "Vegetation Increase" = "#008B45"
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "NDVI Change Distribution (2018–2022)",
    x = "NDVI Change Category",
    y = "Percentage of Study Area (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "black"),
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

combined_ndvi_change <- map_with_legend / ndvi_change_bar +
  plot_layout(heights = c(1.35, 1)) +
  plot_annotation(
    title = "NDVI Change Analysis in the Pra River Basin (2018–2022)",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = "black"))
  )

print(combined_ndvi_change)

ggsave(
  "maps/ndvi_change_map_and_bar_chart.png",
  combined_ndvi_change,
  width = 11,
  height = 12,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 15. LAND COVER CLASSIFICATION MAP
# ============================================================

class_small <- terra::aggregate(vegetation_classes, fact = 8, fun = modal, na.rm = TRUE)

df_class <- as.data.frame(class_small, xy = TRUE)
df_class <- na.omit(df_class)
colnames(df_class) <- c("x", "y", "class")

class_colors <- c(
  "1" = palette$water_bodies,
  "2" = palette$mining,
  "3" = palette$healthy_forest,
  "4" = palette$disturbed_forest,
  "5" = palette$bare_soil
)

im_class_map <- ggplot(df_class, aes(x = x, y = y, fill = factor(class))) +
  geom_raster() +
  scale_fill_manual(
    values = class_colors,
    name = "Land Cover Class",
    labels = c(
      "1" = "Water Bodies",
      "2" = "Active Mining",
      "3" = "Healthy Vegetation",
      "4" = "Disturbed Vegetation",
      "5" = "Bare Soil"
    )
  ) +
  coord_equal() +
  labs(title = "Land Cover Classification Showing Mining Areas (2022)")

print(im_class_map)

ggsave(
  "maps/vegetation_mining_2022.png",
  im_class_map,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 16. MINING DISTRIBUTION MAP
# ============================================================

class_small <- terra::aggregate(vegetation_classes, fact = 8, fun = modal, na.rm = TRUE)
ndvi_small <- terra::aggregate(ndvi_2022, fact = 8)

df_class <- as.data.frame(class_small, xy = TRUE, na.rm = TRUE)
colnames(df_class) <- c("x", "y", "class")

df_ndvi <- as.data.frame(ndvi_small, xy = TRUE, na.rm = TRUE)
colnames(df_ndvi) <- c("x", "y", "ndvi")

df_mining_points <- df_class[df_class$class == 2, ]

set.seed(42)
if (nrow(df_mining_points) > 4000) {
  df_mining_points <- df_mining_points[sample(nrow(df_mining_points), 4000), ]
}

p_mining <- ggplot() +
  geom_raster(data = df_ndvi, aes(x = x, y = y, fill = ndvi)) +
  scale_fill_viridis_c(option = "D", name = "NDVI") +
  geom_point(
    data = df_mining_points,
    aes(x = x, y = y),
    color = "firebrick",
    size = 0.5,
    alpha = 0.3
  ) +
  coord_equal() +
  labs(
    title = "Spatial Distribution of Active Mining Areas – Pra River Basin (2022)",
    subtitle = "Red dots indicate classified active mining pixels",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.4)
  )

print(p_mining)

ggsave(
  "maps/mining_distribution_dots_2022.png",
  p_mining,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 17. SPECTRAL ANALYSIS OF MINING AREAS
# ============================================================

dist_data <- rbind(
  extract_distances(fuzzy_2018, "2018", mining_class, 5000),
  extract_distances(fuzzy_2020, "2020", mining_class, 5000),
  extract_distances(fuzzy_2022, "2022", mining_class, 5000)
)

p_density <- ggplot(dist_data, aes(x = Distance, fill = Year)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("2018" = "#FEE8C8", "2020" = "#FDBB84", "2022" = "#E34A33")) +
  labs(
    title = "Spectral Distance to Mining Signature (2018-2022)",
    subtitle = "Using 2018 centroids as reference",
    x = "Distance to Mining Centroid (Lower = More Similar)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

df_mining_scatter <- data.frame(
  Red = fuzzy_2022$scaled_values[, 3],
  NIR = fuzzy_2022$scaled_values[, 4],
  Distance = fuzzy_2022$raw_distances[, mining_class]
)

set.seed(42)
if (nrow(df_mining_scatter) > 5000) {
  df_mining_scatter <- df_mining_scatter[sample(nrow(df_mining_scatter), 5000), ]
}

p_mining_scatter <- ggplot(df_mining_scatter, aes(x = Red, y = NIR, color = Distance)) +
  geom_point(alpha = 0.5, size = 0.8) +
  scale_color_viridis_c(option = "magma", name = "Distance\nto Mining") +
  geom_point(
    data = data.frame(
      x = fuzzy_2018$centers[mining_class, 3],
      y = fuzzy_2018$centers[mining_class, 4]
    ),
    aes(x = x, y = y),
    shape = 23,
    size = 6,
    fill = "red",
    color = "black",
    stroke = 1.5
  ) +
  labs(
    title = "Mining Spectral Distance (2022)",
    subtitle = "Red diamond = 2018 mining centroid",
    x = "Red Reflectance (0–255)",
    y = "NIR Reflectance (0–255)"
  ) +
  theme_minimal()

combined <- (p_mining_scatter | p_density) +
  plot_layout(widths = c(1.3, 1.1), heights = c(1)) +
  plot_annotation(
    title = "Mining Impact Analysis: Pra River Basin (2018-2022)",
    subtitle = "Fuzzy classification with fixed spectral centroids",
    caption = "Analysis: Sentinel-2 imagery | Distance measured in standardized spectral space"
  ) &
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 10),
    panel.spacing = unit(1.2, "lines")
  )

print(combined)

ggsave(
  "maps/mining_spectral_analysis.png",
  combined,
  width = 20,
  height = 7,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 18. FOREST SPECTRAL ANALYSIS
# ============================================================

centroid_analysis$NDVI <- (centroid_analysis$NIR - centroid_analysis$Red) /
  (centroid_analysis$NIR + centroid_analysis$Red)

df_forest_scatter <- data.frame(
  Red = fuzzy_2022$scaled_values[, 3],
  NIR = fuzzy_2022$scaled_values[, 4],
  Distance = fuzzy_2022$raw_distances[, forest_class]
)

set.seed(42)
if (nrow(df_forest_scatter) > 5000) {
  df_forest_scatter <- df_forest_scatter[sample(nrow(df_forest_scatter), 5000), ]
}

p_forest_scatter <- ggplot(df_forest_scatter, aes(x = Red, y = NIR, color = Distance)) +
  geom_point(alpha = 0.5, size = 0.8) +
  scale_color_viridis_c(option = "viridis", name = "Distance \nto Forest") +
  geom_point(
    data = data.frame(
      x = fuzzy_2018$centers[forest_class, 3],
      y = fuzzy_2018$centers[forest_class, 4]
    ),
    aes(x = x, y = y),
    shape = 23,
    size = 6,
    fill = "darkgreen",
    color = "black",
    stroke = 1.5
  ) +
  labs(
    title = "Forest Spectral Distance (2022)",
    subtitle = "Green diamond = 2018 forest centroid",
    x = "Red Reflectance (0–255)",
    y = "NIR Reflectance (0–255)"
  ) +
  theme_minimal()

dist_data <- rbind(
  extract_distances(fuzzy_2018, "2018", forest_class, 5000),
  extract_distances(fuzzy_2020, "2020", forest_class, 5000),
  extract_distances(fuzzy_2022, "2022", forest_class, 5000)
)

p_forest_density <- ggplot(dist_data, aes(x = Distance, fill = Year)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("2018" = "#00441B", "2020" = "#238B45", "2022" = "#66C2A4")) +
  labs(
    title = "Spectral Distance to Forest Signature (2018-2022)",
    subtitle = "Using 2018 centroids as reference",
    x = "Distance to Forest Centroid (Lower = More Similar)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

combined_forest <- (p_forest_scatter | p_forest_density) +
  plot_layout(widths = c(1.3, 1.1), heights = c(1)) +
  plot_annotation(
    title = "Forest Spectral Analysis (2022)",
    subtitle = "Scatter plot vs density distribution",
    caption = "Analysis: Sentinel-2 imagery | Distance measured in standardized spectral space"
  ) &
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 10),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "right"
  )

print(combined_forest)

ggsave(
  "maps/forest_spectral_analysis.png",
  combined_forest,
  width = 20,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("Analysis complete. All maps and outputs have been saved.\n")
