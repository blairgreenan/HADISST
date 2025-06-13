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

# Load temperature data (you'll need to replace this with your actual data loading)
# For demonstration, creating sample data
# In practice, you would load your HadISST_annual.mat data here
# Example: load("HadISST_annual.RData") or similar

# Sample temperature difference data (replace with your actual data)
set.seed(123)  # for reproducible sample data
Data_diff_rear <- runif(10, 0.4, 2.6)  # Sample temperature changes for 10 regions
Br <- 1.2  # Bravo station temperature change
Pp <- 1.8  # Papa station temperature change

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
cat("Summary of Temperature Changes:\n")
cat("Regions:", paste(region_names, collapse = ", "), "\n")
cat("Temperature changes:", paste(round(Data_diff_rear, 2), collapse = ", "), "\n")
cat("Bravo station:", round(Br, 2), "°C\n")
cat("Papa station:", round(Pp, 2), "°C\n")