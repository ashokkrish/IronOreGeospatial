## Install necessary packages
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("viridis")
# install.packages("openxlsx")
# install.packages("tidyr")
# install.packages("sp")
# install.packages("sf")
# install.packages("ggspatial")
# install.packages("gstat")

## Clear all of the objects from your workspace to start with a clean environment
rm(list = ls())

# Load libraries
library(readxl)
library(dplyr)     # data manipulation
library(openxlsx)
library(ggplot2)   # plotting
library(viridis)
library(tidyr)
library(sp)
library(sf)
library(ggspatial)
library(gstat)     # geostatistics

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
    P = `P (%)`,
    Lithology = `Lithology`
  )

dim(df_clean)
names(df_clean)
class(df_clean)
head(df_clean)

# Define a mode function to get the most frequent value
mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Group by East and North, then calculate the average of the other columns
df_merged <- df_clean %>%
  group_by(East, North) %>%
  summarise(
    Elevation = round(mean(Elevation, na.rm = TRUE), 2),
    Fe = round(mean(Fe, na.rm = TRUE), 2),
    AL2O3 = round(mean(AL2O3, na.rm = TRUE), 2),
    Mn = round(mean(Mn, na.rm = TRUE), 2),
    P = round(mean(P, na.rm = TRUE), 2),
    Lithology = mode_function(Lithology),
    .groups = 'drop'
  )

dim(df_merged)
names(df_merged)
head(df_merged)

#----------------------------------------------#
# Export the clean data frame to an Excel file #
#----------------------------------------------#

write.xlsx(df_clean, "Drill_Holes_Iron_Clean.xlsx", rowNames = FALSE)
write.xlsx(df_merged, "Drill_Holes_Iron_Merged.xlsx", rowNames = FALSE)

#------------------------#
# Descriptive statistics #
#------------------------#

summary(df_clean$Fe)
table(df_clean$Lithology)
mode_function(df_clean$Lithology)
sd(df_clean$Fe)

summary(df_merged$Fe)
table(df_merged$Lithology)
mode_function(df_merged$Lithology)
sd(df_merged$Fe)

#-----------#
# Histogram #
#-----------#

hist(df_merged$Fe, xlab = "Fe (%)", main = "Histogram of Iron Deposit (%)")

#-----------------------------------------#
# Generating frequency tables using dplyr #
#-----------------------------------------#

# Define the bins for Fe values
bins <- c(0, 20, 40, 60, 80, 100)

# Create a new column 'Fe_bin' that categorizes Fe values into bins
df_merged <- df_merged %>%
  mutate(Fe_bin = cut(Fe, breaks = bins, right = FALSE))

# Create a frequency table for the bins
frequency_table <- df_merged %>%
  group_by(Fe_bin) %>%
  summarise(Frequency = n())

# Calculate the relative frequency
frequency_table <- frequency_table %>%
  mutate(Relative_Frequency = Frequency/sum(Frequency))

# Display the frequency table
print(frequency_table)

#-----------------------------#
# Cross Table: Lithology X Fe #
#-----------------------------#

# Create a cross table of Lithology and Fe_bin
cross_table <- df_merged %>%
  group_by(Lithology, Fe_bin) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Fe_bin, values_from = Frequency, values_fill = list(Frequency = 0)) %>%
  select(Lithology, `[20,40)`, `[40,60)`, `[60,80)`)

# Display the cross table
print(cross_table)

#-------------------------#
# Correlation Coefficient #
#-------------------------#

cor(df_merged$Fe, df_merged$AL2O3)

# Select columns of interest
columns_of_interest <- c("Fe", "AL2O3", "Mn", "P")

# Calculate correlation matrix
corr_matrix <- round(cor(df_merged[, columns_of_interest]), 5)
corr_matrix

#------------------------#
# Create the spatial map #
#------------------------#

ggplot(df_merged, aes(x = East, y = North, color = Fe)) +
  geom_point(size = 1.5) + # Adjust the size of points if needed
  geom_hline(yintercept = min(df_merged$North), color = "black") +
  geom_vline(xintercept = min(df_merged$East), color = "black") +
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

#--------------------------#
# Constructing a variogram #
#--------------------------#

coordinates(df_clean) = ~East+North
class(df_clean)

?variogram

# ?variogram gives as a default for argument width the value cutoff/15, 
# which causes the default of 15 points. If you make the value for 
# width smaller, you will see more points. 

methods(variogram)

vgm1 <- variogram(Fe~1, df_clean)
vgm1

summary(vgm1)

plot(vgm1, main = "Variogram: Iron Ore Deposit", pch = 19, xlab = "Distance", ylab = "Semivariance")

#?fit.variogram

model.1 <- fit.variogram(vgm1, vgm(psill = 2, model = "Sph", range = 50, nugget = 0.5))
model.1

summary(model.1)

plot(vgm1, model=model.1, pch = 19, xlab = "Distance", ylab = "Semivariance")






##########################################

## install.packages("sf")
# library(sf)
# 
# # Assuming the UTM Zone is 19S
# utm_crs <- st_crs("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs")
# wgs84_crs <- st_crs(4326)  # WGS84
# 
# # Create an sf object from the data frame
# df_sf <- st_as_sf(df_merged, coords = c("East", "North"), crs = utm_crs)
# 
# # Transform the coordinates to WGS84
# df_sf <- st_transform(df_sf, crs = wgs84_crs)
# 
# # Extract the Lat and Lon coordinates back into the data frame
# df_merged <- df_merged %>%
#   mutate(Latitude = st_coordinates(df_sf)[,2],
#          Longitude = st_coordinates(df_sf)[,1])
# 
# # Display the first few rows of the updated data frame
# head(df_merged)

##########################################

# # install.packages("revgeo")
# # install.packages("terra")
# 
# library(revgeo)
# library(terra)
# 
# # Function to convert UTM to lat/lon using terra
# convert_utm_to_latlon <- function(easting, northing, zone = 33, hemisphere = "N") {
#   # Create a SpatVector object with the UTM coordinates and UTM CRS
#   crs_string <- paste0("+proj=utm +zone=", zone, ifelse(hemisphere == "N", " +north", " +south"), " +datum=WGS84")
#   vect_utm <- vect(cbind(easting, northing), crs = crs_string)
#   
#   # Transform to lat/lon
#   vect_latlon <- project(vect_utm, "+proj=longlat +datum=WGS84")
#   
#   # Extract the coordinates
#   coords <- as.data.frame(geom(vect_latlon))
#   return(coords)
# }
# 
# # Apply the conversion function
# latlon <- convert_utm_to_latlon(df_merged$East, df_merged$North)
# 
# # Add the lat/lon to the original data frame
# df_merged$Latitude <- latlon$y
# df_merged$Longitude <- latlon$x
# 
# # Function to get the country based on coordinates
# get_country_revgeo <- function(lat, lon) {
#   result <- revgeo(lat, lon, provider = "photon", output = "frame")
#   return(result$country)
# }
# 
# # Apply the function to each row of the data frame
# df_merged$Country <- mapply(get_country_revgeo, df_merged$Latitude, df_merged$Longitude)
# 
# # Print the updated data frame with the country information
# print(df_merged)

##########################################

# # Function to create plots with different viridis options
# create_plot <- function(option) {
#   ggplot(df_merged, aes(x = `East (X)`, y = `North (Y)`, color = Fe)) +
#     geom_point(size = 1.5) + # Adjust the size of points if needed
#     geom_hline(yintercept = min(df_merged$`North (Y)`), color = "black") +
#     geom_vline(xintercept = min(df_merged$`East (X)`), color = "black") +
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

# install.packages("scatterplot3d")
# library(scatterplot3d)
# # Create 3D scatter plot
# scatterplot3d(df_merged$East, df_merged$North, df_merged$Elevation, 
#               xlab = "East (X)", 
#               ylab = "North (Y)", 
#               zlab = "Elevation (Z)", 
#               main = "3D Scatter Plot of Spatial Data")

##########################################