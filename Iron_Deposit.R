# Install necessary packages
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("viridis")
# install.packages("openxlsx")

# Load libraries
library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(viridis)

df <- read_excel("Drill_Holes_Iron.xlsx", sheet = "Dataset")
df <- df[ , 1:8]

dim(df)
names(df)

df_clean <- df %>%
  filter_all(all_vars(. != -99)) %>%
  rename(
    East = `East (X)`,
    North = `North (Y)`,
    Elevation = `Elevation (Z)`,
    Fe = `Fe (%)`,
    AL2O3 = `AL2O3 (%)`,
    Mn = `Mn (%)`,
    P = `P (%)`
  )

dim(df_clean)
names(df_clean)

# cor(df_clean$Fe, df_clean$AL2O3)

# Select columns of interest
columns_of_interest <- c("Fe", "AL2O3", "Mn", "P")

# Calculate correlation matrix
corr_matrix <- round(cor(df_clean[, columns_of_interest]), 5)
corr_matrix

# Create the spatial map
ggplot(df_clean, aes(x = East, y = North, color = Fe)) +
  geom_point(size = 1.5) + # Adjust the size of points if needed
  geom_hline(yintercept = min(df_clean$North), color = "black") +
  geom_vline(xintercept = min(df_clean$East), color = "black") +
  scale_color_viridis(name = "Fe (%)", option = "H", direction = -1) + # Use the viridis color scale
  labs(title = "Spatial Map of Fe (%)", x = "East (X)", y = "North (Y)") +
  theme_minimal() + # Use a minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(),                    # Remove major grid lines
    panel.grid.minor = element_blank()                     # Remove minor grid lines
  )

##########################################

# install.packages("revgeo")
# install.packages("terra")

library(revgeo)
library(terra)

# Function to convert UTM to lat/lon using terra
convert_utm_to_latlon <- function(easting, northing, zone = 33, hemisphere = "N") {
  # Create a SpatVector object with the UTM coordinates and UTM CRS
  crs_string <- paste0("+proj=utm +zone=", zone, ifelse(hemisphere == "N", " +north", " +south"), " +datum=WGS84")
  vect_utm <- vect(cbind(easting, northing), crs = crs_string)
  
  # Transform to lat/lon
  vect_latlon <- project(vect_utm, "+proj=longlat +datum=WGS84")
  
  # Extract the coordinates
  coords <- as.data.frame(geom(vect_latlon))
  return(coords)
}

# Apply the conversion function
latlon <- convert_utm_to_latlon(df_clean$East, df_clean$North)

# Add the lat/lon to the original data frame
df_clean$Latitude <- latlon$y
df_clean$Longitude <- latlon$x

# Function to get the country based on coordinates
get_country_revgeo <- function(lat, lon) {
  result <- revgeo(lat, lon, provider = "photon", output = "frame")
  return(result$country)
}

# Apply the function to each row of the data frame
df_clean$Country <- mapply(get_country_revgeo, df_clean$Latitude, df_clean$Longitude)

# Print the updated data frame with the country information
print(df_clean)

##########################################

# # Function to create plots with different viridis options
# create_plot <- function(option) {
#   ggplot(df_clean, aes(x = `East (X)`, y = `North (Y)`, color = Fe)) +
#     geom_point(size = 1.5) + # Adjust the size of points if needed
#     geom_hline(yintercept = min(df_clean$`North (Y)`), color = "black") +
#     geom_vline(xintercept = min(df_clean$`East (X)`), color = "black") +
#     scale_color_viridis(name = "Fe (%)", option = option) + # Use the viridis color scale
#     labs(title = "Spatial Map of Fe (%)", x = "East (X)", y = "North (Y)") +
#     theme_minimal() + # Use a minimal theme for a clean look
#     theme(
#       plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#       axis.title = element_text(size = 14, face = "bold"),
#       axis.text = element_text(size = 12),
#       legend.title = element_text(size = 14, face = "bold"),
#       legend.text = element_text(size = 12),
#       panel.grid.major = element_blank(),                    # Remove major grid lines
#       panel.grid.minor = element_blank()                     # Remove minor grid lines
#     )
# }
# 
# # List of options
# options <- c("A", "B", "C", "D", "E", "F", "G", "H")
# 
# # Create plots for each option
# plots <- lapply(options, create_plot)
# 
# # Print plots (this will display the plots one by one)
# for (plot in plots) {
#   print(plot)
# }

##########################################

# write.xlsx(df_clean, "Drill_Holes_Iron_Clean.xlsx", rowNames = FALSE)

##########################################

# install.packages("scatterplot3d")
# library(scatterplot3d)
# # Create 3D scatter plot
# scatterplot3d(df_clean$`East (X)`, df_clean$`North (Y)`, df_clean$`Elevation (Z)`, 
#               xlab = "East (X)", 
#               ylab = "North (Y)", 
#               zlab = "Elevation (Z)", 
#               main = "3D Scatter Plot of Spatial Data")