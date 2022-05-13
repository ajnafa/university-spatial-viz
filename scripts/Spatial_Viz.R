#----Spatial Visualization of U.S. Universities to Costco Wholesale Locations---
#-Author: A. Jordan Nafa---------------------------------Created: May 13, 2022-#
#-R Version: 4.1.3---------------------------------------Revised: May 13, 2022-#

# Set Project Options----
options(
  digits = 4, # Significant figures output
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"],
  mc.cores = parallel::detectCores()
)

# Load the necessary libraries----
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  "sf",
  "units",
  "tigris",
  install = FALSE
)

# Load the helper functions
source("scripts/Helper_Functions.R")

#------------------------------------------------------------------------------#
#------------Four-Year Universities' Proximity to Costco Locations--------------
#------------------------------------------------------------------------------#

# Read in the result of the data_prep script
df <- read_rds("data/US_Costco_Proximity.rds")

# Load the geometries for U.S. States
us_states <- states() %>% 
  # Keep only continental U.S. for simplicity
  filter(STUSPS %in% df$stabbr)

# Create the base plot object
base_plot <- ggplot() +
  # Add the U.S. state boundaries layer
  geom_sf(
    data = us_states, 
    aes(geometry = geometry), 
    color = "black",
    fill = NA
  ) +
  # Apply Custom Map Theme Settings
  map_theme(
    show.axis = FALSE, 
    title_size = 30,
    plot.margin = margin(3, 2, 5, 0.2, "mm")
  )
  
# Plot of the spatial proximity of universities to costco locations
proximity_by_inst_plot <- base_plot +   
  # Add the university locations for four year instutions
  geom_sf(
    data = df %>% 
      filter(prog_level == "University"), 
    aes(geometry = university_geometry, fill = distance_km),
    alpha = 0.75, shape = 21
    ) +
  # Facet wrap by institution type
  facet_wrap(~ inst_type, ncol = 2) +
  # Set the fill color
  viridis::scale_fill_viridis(option = "H") +
  # Setting the parameters for the plot legend
  guides(
    fill = guide_colorbar(
      title = "Distance (Km) to Nearest Costco",
      nbin = 1000,
      barheight = 2,
      barwidth = 30
    )) +
  # Place the legend at the bottom of the plot
  theme(
    legend.position = "bottom",
    plot.title = element_text(vjust = 2)
    ) +
  # Add plot labels
  labs(title = "Spatial Proximity to Costco Locations Among Four-Year Universities in the Continental U.S. by Institution Type")
  
# Save the generated plot object as a .jpeg file
ggsave(
  filename = "Costco_Proximity_by_Inst_Type.jpeg",
  plot = proximity_by_inst_plot,
  path = "figs/",
  device = "jpeg",
  width = 25,
  height = 8,
  units = "in",
  dpi = "retina",
  limitsize = F
)
  
# Plot of the spatial proximity of universities to costco locations
proximity_by_hbcu_plot <- base_plot +   
  # Add the university locations for four year institutions
  geom_sf(
    data = df %>% 
      filter(prog_level == "University"), 
    aes(geometry = university_geometry, fill = distance_km),
    alpha = 0.75, shape = 21
  ) +
  # Facet wrap by hbcu status
  facet_wrap(~ hbcu_status, ncol = 2) +
  # Set the fill color
  viridis::scale_fill_viridis(option = "H") +
  # Setting the parameters for the plot legend
  guides(
    fill = guide_colorbar(
      title = "Distance (Km) to Nearest Costco",
      nbin = 1000,
      barheight = 2,
      barwidth = 30
    )) +
  # Place the legend at the bottom of the plot
  theme(
    legend.position = "bottom",
    plot.title = element_text(vjust = 2)
  ) +
  # Add plot labels
  labs(title = "Spatial Proximity to Costco Locations Among Four-Year Universities in the Continental U.S. by HBCU Status")
  
# Save the generated plot object as a .jpeg file
ggsave(
  filename = "Costco_Proximity_by_HBCU_Status.jpeg",
  plot = proximity_by_hbcu_plot,
  path = "figs/",
  device = "jpeg",
  width = 25,
  height = 8,
  units = "in",
  dpi = "retina",
  limitsize = F
)
  
# Plot of the spatial proximity of universities to costco locations
proximity_all_plot <- base_plot +   
  # Add the university locations for four year instutions
  geom_sf(
    data = df, 
    aes(geometry = university_geometry, fill = distance_km),
    alpha = 0.75, shape = 21
  ) +
  # Facet wrap by institution type
  facet_wrap(~ inst_type, ncol = 2) +
  # Set the fill color
  viridis::scale_fill_viridis(option = "H") +
  # Setting the parameters for the plot legend
  guides(
    fill = guide_colorbar(
      title = "Distance (Km) to Nearest Costco",
      nbin = 1000,
      barheight = 2,
      barwidth = 30
    )) +
  # Place the legend at the bottom of the plot
  theme(
    legend.position = "bottom",
    plot.title = element_text(vjust = 2)
  ) +
  # Add plot labels
  labs(title = "Spatial Proximity to Costco Locations Among All Degree-Granting Colleges and Universities in the Continental U.S. by Institution Type")

# Save the generated plot object as a .jpeg file
ggsave(
  filename = "Costco_Proximity_by_Inst_All.jpeg",
  plot = proximity_all_plot,
  path = "figs/",
  device = "jpeg",
  width = 25,
  height = 8,
  units = "in",
  dpi = "retina",
  limitsize = F
)


# Throw in a Histogram for Good Measure
hist_all_dists <- ggplot(data = df) +
  # Facet by highest degree offered
  facet_wrap(~ degree_high, scales = "free") +
  # Add a Histogram geom
  geom_histogram(
    aes(x = distance_km, fill = degree_high), 
    bins = 50, color = "black"
    ) + 
  # Set the fill colors
  viridis::scale_fill_viridis(option = "H", discrete = TRUE) + 
  # Add labels to the plot
  labs(
    x = "Distance (Km) to Nearest Costco",
    y = "",
    title = "Distribution of Spatial Proximity to Costco Locations Among All Degree-Granting Colleges and Universities in the Continental U.S.",
    fill = "Highest Degree Offered"
  ) +
  # Apply theme settings
  plot_theme(
    title_size = 30,
    xaxis_size = 18,
    strip_size = 18,
    plot.margin = margin(3, 2, 5, 0.2, "mm")
  )

# Save the generated plot object as a .jpeg file
ggsave(
  filename = "Histogram_Costco_Proximity_All.jpeg",
  plot = hist_all_dists,
  path = "figs/",
  device = "jpeg",
  width = 25,
  height = 8,
  units = "in",
  dpi = "retina",
  limitsize = F
)
