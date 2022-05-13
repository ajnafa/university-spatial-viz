#------Spatial Proximity of U.S. Universities to Costco Wholesale Locations-----
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
  install = FALSE
)

#------------------------------------------------------------------------------#
#---------------------------University Location Data----------------------------
#------------------------------------------------------------------------------#

# Data on univeristy locations from the IPEDS database
universities <- read_csv("data/hd2020_data_stata.csv") %>% 
  # Convert the variable names to lowercase
  rename_with(.fn = str_to_lower) %>% 
  # Filter out united states territories
  filter(
    stabbr %in% unique(.$stabbr)[c(1, 3:15, 17:51)] & 
      (hloffer > 2 & cyactive == 1 & control < 3)
    ) %>% 
  # Transmute a subset of the variables and apply encodings
  transmute(
    # Unique identifier
    inst_id = unitid,
    # Name of the School
    inst_name = instnm,
    # Address of the School
    inst_address = addr,
    # Other geographic identifiers
    across(c(city:fips, longitud:latitude), ~ .x),
    # Program Level of the Institution
    prog_level = factor(
      iclevel,
      levels = 1:2,
      labels = c("University", "Junior College")
    ),
    # Type of Institution
    inst_type = factor(
      control,
      levels = 1:2,
      labels = c("Public", "Private Non-Profit")
    ),
    # Highest Degree Awarded
    degree_high = factor(
      case_when(
        hloffer %in% 3:4 ~ 1,
        hloffer %in% 5:6 ~ 2,
        hloffer %in% 7:8 ~ 3,
        hloffer == 9 ~ 4
      ),
      levels = 1:4,
      labels = c("Associate's", "Bachelor's", "Master's", "Doctorate")
    ),
    # Historically Balck College/University
    hbcu_status = factor(
      hbcu,
      levels = 1:2,
      labels = c("HBCU", "Non-HBCU")
    ),
    # Locale of the School
    locale = factor(
      case_when(
        locale %in% 11:13 ~ 1,
        locale %in% 21:23 ~ 2,
        locale %in% 31:33 ~ 3,
        locale %in% 41:43 ~ 4
      ),
      levels = 1:4,
      labels = c("City", "Suburb", "Town", "Rural")
    )
  ) %>% 
  # Convert the fiel to a spatial object
  st_as_sf(
    .,
    coords = c("longitud", "latitude"),
    crs = "+proj=longlat +datum=WGS84"
  )

#------------------------------------------------------------------------------#
#-----------------------------Costco Location Data------------------------------
#------------------------------------------------------------------------------#

# Read in and prepare the costco data from http://www.poi-factory.com/node/9856
costco <- read_csv("data/Costco_USA_Canada.csv") %>% 
  # Filter US locations
  filter(country == "USA" & state %in% universities$stabbr) %>% 
  # Rename for merge compatability
  rename_with(.cols = location:country, .fn = ~ str_c(.x, "costco", sep = "_")) %>% 
  # Convert the fiel to a spatial object
  st_as_sf(
    .,
    coords = c("longitude", "latitude"),
    crs = "+proj=longlat +datum=WGS84"
  )

# List object to store the distance calculations in
costco_distance <- list()

# Calculate the spatial proximity of each university/college to the nearest costco
for (i in seq_along(universities$inst_id)) {
  costco_distance[[i]] <- st_distance(universities[i,], costco, by_element = TRUE)
}

costco_proximity <- map_dfr(
  costco_distance, 
  ~ costco[which.min(.x),] %>% 
    mutate(dist_meters = .x[which.min(.x)])
  )

# Combine the costco proximity and universities data
univeristy_costcos <- universities %>% 
  # Bind the columns together
  bind_cols(costco_proximity) %>% 
  # Fix geometry names
  rename(
    university_geometry = geometry...13,
    costco_geometry = geometry...21,
  ) %>% 
  # Calculate distance in kilometers
  mutate(distance_km = as.numeric(dist_meters/1000))

# Write the full data to an rds file
write_rds(univeristy_costcos, "data/US_Costco_Proximity.rds")
