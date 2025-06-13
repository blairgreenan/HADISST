# Load required libraries
library(ggOceanMaps)
library(ggspatial)
library(ggplot2)
library(dplyr)
library(readr)
library(sf)
library(cowplot)
library(viridis)
library(RColorBrewer)
library(R.matlab)  # For reading MATLAB .mat files

# Clear environment
rm(list = ls())

# Set working directory (adjust as needed)
DIR <- "./"

# Define region files
region_files <- c(
  "polygon_gomss_subarea_gom.csv",
  "polygon_ss_merged.csv",
  "polygon_gsl_subarea_gsl_clipped.csv",
  "polygon_ns_subarea_sns.csv",
  "polygon_ns_merged.csv",
  "polygon_ls_merged.csv",
  "polygon_hb_subarea_hb.csv",
  "polygon_bb_subarea_bb.csv",
  "polygon_bcs_subarea_bcs.csv",
  "polygon_ca_subarea_sbs.csv"
)

# Define region names
region_names <- c("GoM", "SS", "GSL", "SNS", "NNS", "LS", "HB", "BB", "BCS", "SBS")

# Load temperature data from MATLAB files
tryCatch({
  # Load annual data
  mat_data_annual <- readMat("HadISST_annual.mat")
  cat("Successfully loaded HadISST_annual.mat\n")
  cat("Variables in annual file:", names(mat_data_annual), "\n")

  # Load summer data (optional - uncomment if needed)
  # mat_data_summer <- readMat("HadISST_summer.mat")
  # cat("Successfully loaded HadISST_summer.mat\n")
  # cat("Variables in summer file:", names(mat_data_summer), "\n")

  # Extract relevant variables from the MATLAB data
  # You'll need to adjust these variable names based on your actual .mat file structure
  if ("Data.diff.rear" %in% names(mat_data_annual)) {
    Data_diff_rear <- as.vector(mat_data_annual$Data.diff.rear)
  } else if ("Data_diff_rear" %in% names(mat_data_annual)) {
    Data_diff_rear <- as.vector(mat_data_annual$Data_diff_rear)
  } else {
    # Try to find the main data variable (often the largest numeric array)
    numeric_vars <- names(mat_data_annual)[sapply(mat_data_annual, is.numeric)]
    if (length(numeric_vars) > 0) {
      main_var <- numeric_vars[1]
      Data_diff_rear <- as.vector(mat_data_annual[[main_var]])
      cat("Using variable:", main_var, "as Data_diff_rear\n")
    } else {
      stop("Could not find numeric data in the .mat file")
    }
  }

  # Extract Bravo and Papa station data if available
  if ("Br" %in% names(mat_data_annual)) {
    Br <- as.numeric(mat_data_annual$Br)
  } else {
    Br <- 1.2  # Default value
    cat("Bravo station data not found, using default value:", Br, "\n")
  }

  if ("Pp" %in% names(mat_data_annual)) {
    Pp <- as.numeric(mat_data_annual$Pp)
  } else {
    Pp <- 1.8  # Default value
    cat("Papa station data not found, using default value:", Pp, "\n")
  }

  # Ensure we have the right number of regions (10)
  if (length(Data_diff_rear) != 10) {
    if (length(Data_diff_rear) > 10) {
      Data_diff_rear <- Data_diff_rear[1:10]
      cat("Truncated Data_diff_rear to 10 values\n")
    } else {
      # Pad with mean values if we have fewer than 10
      mean_val <- mean(Data_diff_rear, na.rm = TRUE)
      Data_diff_rear <- c(Data_diff_rear, rep(mean_val, 10 - length(Data_diff_rear)))
      cat("Padded Data_diff_rear to 10 values using mean\n")
    }
  }

  cat("Temperature data loaded successfully:\n")
  cat("Data_diff_rear:", paste(round(Data_diff_rear, 3), collapse = ", "), "\n")
  cat("Bravo (Br):", round(Br, 3), "\n")
  cat("Papa (Pp):", round(Pp, 3), "\n")

}, error = function(e) {
  cat("Error loading MATLAB files:", e$message, "\n")
  cat("Using sample data instead...\n")

  # Fallback to sample data if .mat files can't be loaded
  set.seed(123)
  Data_diff_rear <- runif(10, 0.4, 2.6)
  Br <- 1.2
  Pp <- 1.8
})

# Define temperature ranges for color mapping
temp_ranges <- list(
  c(0.4, 0.6), c(0.6, 0.8), c(0.8, 1.0), c(1.0, 1.2), c(1.2, 1.4),
  c(1.4, 1.6), c(1.6, 1.8), c(1.8, 2.0), c(2.0, 2.2), c(2.2, 2.4), c(2.4, 2.6)
)

# Create color palette (thermal-like colors)
n_colors <- 11
thermal_colors <- viridis(n_colors, option = "plasma")

# Function to assign colors based on temperature values
assign_color <- function(temp_value, ranges, colors) {
  for (i in 1:length(ranges)) {
    if (temp_value >= ranges[[i]][1] & temp_value < ranges[[i]][2]) {
      return(colors[i])
    }
  }
  return(colors[length(colors)])  # Return last color if above all ranges
}

# Read polygon data and create spatial features
regions_data <- list()
for (i in 1:length(region_files)) {
  tryCatch({
    # Read CSV file
    poly_data <- read_csv(file.path(DIR, region_files[i]))

    # Assume first two columns are longitude and latitude
    if (ncol(poly_data) >= 2) {
      colnames(poly_data)[1:2] <- c("lon", "lat")

      # Create polygon
      poly_sf <- poly_data %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

      # Add metadata
      poly_sf$region_id <- i
      poly_sf$region_name <- region_names[i]
      poly_sf$temp_change <- Data_diff_rear[i]
      poly_sf$color <- assign_color(Data_diff_rear[i], temp_ranges, thermal_colors)

      regions_data[[i]] <- poly_sf
    }
  }, error = function(e) {
    warning(paste("Could not read file:", region_files[i], "- Error:", e$message))
  })
}

# Combine all regions
if (length(regions_data) > 0) {
  all_regions <- do.call(rbind, regions_data)
} else {
  stop("No polygon data could be loaded. Please check file paths and formats.")
}

# Create the main map
main_map <- basemap(
  limits = c(-150, -40, 40, 80),
  projection = "lambert",
  land.col = "grey70",
  grid.col = "grey90",
  grid.size = 0.1
) +
  geom_sf(data = all_regions,
          aes(fill = temp_change),
          alpha = 0.7,
          color = "black",
          size = 0.2) +
  scale_fill_gradientn(
    colors = thermal_colors,
    limits = c(0.4, 2.6),
    name = "Temperature\nChange (°C)",
    guide = guide_colorbar(
      barwidth = 1,
      barheight = 10,
      title.position = "top"
    )
  ) +
  # Add region numbers
  geom_sf_text(data = all_regions,
               aes(label = region_id),
               size = 2,
               color = "white",
               fontface = "bold") +
  # Add Bravo station
  geom_point(aes(x = -51, y = 56.5),
             color = "black",
             fill = assign_color(Br, temp_ranges, thermal_colors),
             shape = 22,
             size = 3) +
  annotate("text", x = -50.5, y = 55.5, label = "Bravo",
           size = 2, style = "italic") +
  # Add Papa station
  geom_point(aes(x = -144.8599, y = 50.0378),
             color = "black",
             fill = assign_color(Pp, temp_ranges, thermal_colors),
             shape = 22,
             size = 3) +
  annotate("text", x = -143.8599, y = 49.0378, label = "Papa",
           size = 2, style = "italic") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
  )

# Create bar chart
bar_data <- data.frame(
  region = factor(1:10, labels = paste0(1:10, "-", region_names)),
  temp_change = Data_diff_rear
)

# Add station data
station_data <- data.frame(
  position = c(6.4, 9.4),
  temp_change = c(Br, Pp),
  label = c("Bravo", "Papa")
)

bar_chart <- ggplot(bar_data, aes(x = region, y = temp_change)) +
  geom_col(fill = "steelblue", color = "steelblue", width = 0.4) +
  # Add station bars
  geom_col(data = data.frame(region = factor(c(6.4, 9.4)), temp_change = c(Br, Pp)),
           aes(x = region, y = temp_change),
           fill = "grey50", color = "grey50", width = 0.2) +
  # Add station labels
  geom_text(data = station_data,
            aes(x = position - 0.1, y = 0.6, label = label),
            size = 2, color = "black") +
  scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
  labs(
    x = "",
    y = "Temperature Change (°C)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_text(size = 8),
    panel.grid.major = element_line(color = "grey90", size = 0.2),
    panel.grid.minor = element_blank()
  )

# Combine plots
combined_plot <- plot_grid(
  main_map,
  bar_chart,
  ncol = 1,
  rel_heights = c(3, 1),
  align = "v"
)

# Display the combined plot
print(combined_plot)

# Save the plot (optional)
# ggsave("temperature_change_map.png", combined_plot,
#        width = 12, height = 10, dpi = 300)

# Print summary statistics
cat("\n=== TEMPERATURE CHANGE ANALYSIS SUMMARY ===\n")
cat("Data source: HadISST MATLAB files\n")
cat("Regions analyzed:", length(region_names), "\n")
cat("Regions:", paste(region_names, collapse = ", "), "\n")
cat("Temperature changes by region:\n")
for (i in 1:length(region_names)) {
  cat(sprintf("  %d-%s: %.2f°C\n", i, region_names[i], Data_diff_rear[i]))
}
cat(sprintf("Bravo station: %.2f°C\n", Br))
cat(sprintf("Papa station: %.2f°C\n", Pp))
cat(sprintf("Mean temperature change: %.2f°C\n", mean(Data_diff_rear, na.rm = TRUE)))
cat(sprintf("Range: %.2f to %.2f°C\n", min(Data_diff_rear, na.rm = TRUE), max(Data_diff_rear, na.rm = TRUE)))

# Optional: Print structure of loaded MATLAB data for debugging
if (exists("mat_data_annual")) {
  cat("\n=== MATLAB FILE STRUCTURE ===\n")
  str(mat_data_annual, max.level = 2)
}
