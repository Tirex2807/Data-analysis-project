library(geodata)
library(ggplot2)
library(raster)
library(gridExtra)
?worldclim_country

spatial_points <- SpatialPoints(coords = matrix_full_elev_eco[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))


##### TEMPERATURE
# Retrieve temperature data for France In summer
fr_clim_tavg <- worldclim_country("France", var = "tavg", path = tempdir())
fr_clim_tavg_br <- brick(fr_clim_tavg)

## Old version
#july_raster <- fr_clim_tavg$FRA_wc2.1_30s_tavg_7
#august_raster <- fr_clim_tavg$FRA_wc2.1_30s_tavg_8
# Extract temperature values at the species occurrences data
#temp_july <- raster::extract(july_raster, spatial_points, method = 'bilinear')
#temp_aug <- raster::extract(august_raster, spatial_points, method = 'bilinear')
# Create a dataframe with all the temperature data for all the summer
#temp <- data.frame(temp_june, temp_july, temp_aug)
# Create empty object to store the data and the name of the column
#So i created the column named temp_summer to meake the mean of the 3 columns
#temp$temp_summer <- rowMeans(temp[, c("temp_june", "temp_july", "temp_aug")])
#After that, i was doing this for precip and wind. Now i apply the loop method.

matrix_temp = NULL
vec_colnames1 = NULL

# Create a loop to have data in from May to September
for (i in 5:9)
{
raster_temp <- fr_clim_tavg_br[[i]] # extract the data
vec_colnames1 <- c(vec_colnames1, names(raster_temp)) # Call a null object colname and store it the name of the raster at each loop

temp <- raster::extract(raster_temp, spatial_points, method = 'bilinear')
matrix_temp <- cbind(matrix_temp, temp) #store the value in the empty object
}

# Add the names of the column in the matrix that are in the vectore names
colnames(matrix_temp) <- vec_colnames1

# Make the mean of these annual value and store it into the matrix
vec_mean_temp <- as.vector(rowMeans(matrix_temp))
matrix_temp <- data.frame(matrix_temp, vec_mean_temp)

# Control that there is the good data
View(matrix_temp)

# Plot density of temperature data
ggplot_E1 <- ggplot(matrix_temp, aes(x = vec_mean_temp)) +
  geom_density(color = "darkblue", fill = "lightblue", adjust = 3) +
  theme_bw()

print(ggplot_E1)

#### PRECIPITATION
# Retrieve precipitation data (same that for temperature before)
fr_clim_prec <- worldclim_country("France", var = "prec", path = tempdir())
fr_clim_prec_br <- brick(fr_clim_prec)

matrix_precip = NULL
vec_colnames2 = NULL

# Create a loop to have data in from May to September
for (i in 5:9)
{
raster_precip <- fr_clim_prec_br[[i]]
vec_colnames2 <- c(vec_colnames2, names(raster_precip))

precip <- raster::extract(raster_precip, spatial_points, method = 'bilinear')
matrix_precip <- cbind(matrix_precip, precip) 
}

# Add the names of the column in the matrix that are in the vectore names
colnames(matrix_precip) <- vec_colnames2

# Make the mean of these annual value and store it into the matrix
vec_mean_precip <- as.vector(rowMeans(matrix_precip))
matrix_precip <- data.frame(matrix_precip, vec_mean_precip)

# Plot density of precipitation data for species occurrences
ggplot_E2 <- ggplot(matrix_precip, aes(x = vec_mean_precip)) +
  geom_density(color = "black", fill = "darkgreen", adjust = 2) +
  theme_bw()

print(ggplot_E2)

#### WIND
# Retrieve Wind data (same as before)
fr_clim_wind <- worldclim_country("France", var = "wind", path = tempdir())
fr_clim_wind_br <- brick(fr_clim_wind)

matrix_wind = NULL
vec_colnames3 = NULL

# Create a loop to have data in from May to September
for (i in 5:9)
{
raster_wind <- fr_clim_wind_br[[i]]
vec_colnames3 <- c(vec_colnames3, names(raster_wind))

wind <- raster::extract(raster_wind, spatial_points, method = 'bilinear')
matrix_wind <- cbind(matrix_wind, wind) 
}

# Add the names of the column in the matrix that are in the vectore names
colnames(matrix_wind) <- vec_colnames3

# Make the mean of these annual value and store it into the matrix
vec_mean_wind <- as.vector(rowMeans(matrix_wind))
matrix_wind <- data.frame(matrix_wind, vec_mean_wind)

# Plot density of wind data for species occurrences
ggplot_E3 <- ggplot(matrix_wind, aes(x = vec_mean_wind)) +
  geom_density(color = "black", fill = "darkred", adjust = 2) +
  theme_bw()

print(ggplot_E3)

# Add climatic data to the matrix full
matrix_full_elev_eco_clim <- data.frame(matrix_full_elev_eco, matrix_temp, matrix_precip, matrix_wind)
