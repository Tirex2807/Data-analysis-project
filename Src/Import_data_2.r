# Here we load the package to access to certain function so I placed them at the top (same for the other code)
library(rgbif)
library(rnaturalearth)
library(ggplot2)
library(rinat)
library(sf)
library(sp)
library(raster)

# Create an object with the name of my species
Cocci1 <- c("Adalia bipunctata")
Cocci2 <- c("Coccinella septempunctata")
Cocci3 <- c("Propylea quatuordecimpunctata")
Cocci4 <- c("Psyllobora vigintiduopunctata")
Puceron <- c("Myzus persicae")

# download GBIF occurrence data
#####################################
#####################################
#####################################
gbif_data1 <- occ_data(scientificName = Cocci1, country = "FR", hasCoordinate = TRUE, limit = 1000)
gbif_data2 <- occ_data(scientificName = Cocci2, country = "FR", hasCoordinate = TRUE, limit = 1000)
gbif_data3 <- occ_data(scientificName = Cocci3, country = "FR", hasCoordinate = TRUE, limit = 1000)
gbif_data4 <- occ_data(scientificName = Cocci4, country = "FR", hasCoordinate = TRUE, limit = 1000)
gbif_data5 <- occ_data(scientificName = Puceron, country = "FR", hasCoordinate = TRUE, limit = 1000)


#Create an object specificly for the occurence data
occur1 <- gbif_data1$data
occur2 <- gbif_data2$data
occur3 <- gbif_data3$data
occur4 <- gbif_data4$data
occur5 <- gbif_data5$data

# Create an object with occurence only for the France 
gbif_data_france1 <- occur1[occur1$country == "France",]
gbif_data_france2 <- occur2[occur2$country == "France",]
gbif_data_france3 <- occur3[occur3$country == "France",]
gbif_data_france4 <- occur4[occur4$country == "France",]
gbif_data_france5 <- occur5[occur5$country == "France",]

# Here create an object with only the geographical informations of France. This will be used to display only the Map of France in the futur plots
France <- ne_countries(scale = "medium", returnclass = "sf", country ="France" )

# Plot GBIF occurrences on a map of France
ggplot(data = France) +
  geom_sf() +
  geom_point(data = occur1, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "darkred") + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))

ggplot(data = France) +
  geom_sf() +
  geom_point(data = occur2, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "darkred") + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))

ggplot(data = France) +
  geom_sf() +
  geom_point(data = occur3, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "darkred") + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))

ggplot(data = France) +
  geom_sf() +
  geom_point(data = occur4, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "darkred") + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))

ggplot(data = France) +
  geom_sf() +
  geom_point(data = occur5, aes(x = decimalLongitude, y = decimalLatitude), size = 4,
             shape = 23, fill = "darkred") + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))



# Download the occurence data from Inaturalist
#####################################
#####################################
#####################################
inat1 <- get_inat_obs(query = "Adalia bipunctata",place_id = "france")
inat2 <- get_inat_obs(query = "Coccinella septempunctata",place_id = "france") 
inat3 <- get_inat_obs(query = "Propylea quatuordecimpunctata",place_id = "france")
inat4 <- get_inat_obs(query = "Psyllobora vigintiduopunctata",place_id = "france") 
inat5 <- get_inat_obs(query = "Myzus persicae",place_id = "france")

# create  a data frame and make sure the informations are considered as in the numeric format
# Then create a vector with only the data of longitude and latitude
# Then we delete the NA and transforme the object / vector into a dataframe
dat_inat1 <- data.frame(as.numeric(inat1$longitude),as.numeric(inat1$latitude))
colnames(dat_inat1) <- c("longitude", "latitude")
dat_inat1 <- na.omit(dat_inat1)
dat_inat1 <- data.frame(dat_inat1)

dat_inat2 <- data.frame(as.numeric(inat2$longitude),as.numeric(inat2$latitude))
colnames(dat_inat2) <- c("longitude", "latitude")
dat_inat2 <- na.omit(dat_inat2)
dat_inat2 <- data.frame(dat_inat2)

dat_inat3 <- data.frame(as.numeric(inat3$longitude),as.numeric(inat3$latitude))
colnames(dat_inat3) <- c("longitude", "latitude")
dat_inat3 <- na.omit(dat_inat3)
dat_inat3 <- data.frame(dat_inat3)

dat_inat4 <- data.frame(as.numeric(inat4$longitude),as.numeric(inat4$latitude))
colnames(dat_inat4) <- c("longitude", "latitude")
dat_inat4 <- na.omit(dat_inat4)
dat_inat4 <- data.frame(dat_inat4)

dat_inat5 <- data.frame(as.numeric(inat5$longitude),as.numeric(inat5$latitude))
colnames(dat_inat5) <- c("longitude", "latitude")
dat_inat5 <- na.omit(dat_inat5)
dat_inat5 <- data.frame(dat_inat5)

# create an object with the data of the previous dataframe into a spatial point data 
spatial_coord1 <- SpatialPoints(dat_inat1)
spatial_coord2 <- SpatialPoints(dat_inat2)
spatial_coord3 <- SpatialPoints(dat_inat3)
spatial_coord4 <- SpatialPoints(dat_inat4)
spatial_coord5 <- SpatialPoints(dat_inat5)

# make a plot of INAT the occurence in France for each species
# the coord_sf function enable us to delimit the perimeter of France and not include the overseas territories
ggplot(data = France) +
  geom_sf()   +
  geom_point(data = inat1, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkgreen") + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))

ggplot(data = France) +
  geom_sf()   +
  geom_point(data = inat2, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkgreen") + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))

ggplot(data = France) +
  geom_sf()   +
  geom_point(data = inat3, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkgreen") + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))

ggplot(data = France) +
  geom_sf()   +
  geom_point(data = inat4, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkgreen") + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))

ggplot(data = France) +
  geom_sf()   +
  geom_point(data = inat5, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkgreen") + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))


############ MERGE DATA
# Create an object for following column stored in the Gbif data object : name of species, latitude, longitude and name of the source
# Create a data frame with only the interresting data we only selected / extracted from the raw dataframe
#####################################
#####################################
#####################################
species <- gbif_data_france1$species
latitude <- gbif_data_france1$decimalLatitude
longitude <- gbif_data_france1$decimalLongitude
source <- rep("gbif", length(species))
data_gbif1 <- data.frame(species, latitude, longitude, source)

species <- gbif_data_france2$species
latitude <- gbif_data_france2$decimalLatitude
longitude <- gbif_data_france2$decimalLongitude
source <- rep("gbif", length(species))
data_gbif2 <- data.frame(species, latitude, longitude, source)

species <- gbif_data_france3$species
latitude <- gbif_data_france3$decimalLatitude
longitude <- gbif_data_france3$decimalLongitude
source <- rep("gbif", length(species))
data_gbif3 <- data.frame(species, latitude, longitude, source)

species <- gbif_data_france4$species
latitude <- gbif_data_france4$decimalLatitude
longitude <- gbif_data_france4$decimalLongitude
source <- rep("gbif", length(species))
data_gbif4 <- data.frame(species, latitude, longitude, source)

species <- gbif_data_france5$species
latitude <- gbif_data_france5$decimalLatitude
longitude <- gbif_data_france5$decimalLongitude
source <- rep("gbif", length(species))
data_gbif5 <- data.frame(species, latitude, longitude, source)

# Same as before, we create an object for each interesting column. The informations for all the species are stored in the same object
# Then, create a data frame for these iNaturalist data
species <- inat1$scientific_name
latitude <- inat1$latitude
longitude <- inat1$longitude
source <- rep("inat", length(species))
data_inat1 <- data.frame(species, latitude, longitude, source)

species <- inat2$scientific_name
latitude <- inat2$latitude
longitude <- inat2$longitude
source <- rep("inat", length(species))
data_inat2 <- data.frame(species, latitude, longitude, source)

species <- inat3$scientific_name
latitude <- inat3$latitude
longitude <- inat3$longitude
source <- rep("inat", length(species))
data_inat3 <- data.frame(species, latitude, longitude, source)

species <- inat4$scientific_name
latitude <- inat4$latitude
longitude <- inat4$longitude
source <- rep("inat", length(species))
data_inat4 <- data.frame(species, latitude, longitude, source)

species <- inat5$scientific_name
latitude <- inat5$latitude
longitude <- inat5$longitude
source <- rep("inat", length(species))
data_inat5 <- data.frame(species, latitude, longitude, source)

# Combine GBIF and iNaturalist data frames with this function into a single unique matrix
matrix_full <- rbind(data_inat1, data_gbif1, data_inat2, data_gbif2, data_inat3, data_gbif3, data_inat4, data_gbif4, data_inat5, data_gbif5)

sf_use_s2(FALSE)

# Define the spatial extent for France and cut the background map for keeping only France 
extent(France) 
ext_France_cut <- as(raster::extent(-5, 10, 40, 55), "SpatialPolygons")

# Crop spatial data to the defined extent
France_crop <- st_crop(France, ext_France_cut)

# Convert data to sf object
data_sf <- st_as_sf(matrix_full, coords = c("longitude","latitude"), crs = 4326)

# Convert ext_France_cut to sf object
France_crop_sf <- st_as_sf(France_crop)

# transfrom the previous results into a matrix format
cur_data <- matrix_full[as.matrix(st_intersects(data_sf, France_crop_sf)),]

# create an object for latitude and another one for longitude data of France
latitude <- gbif_data_france1$decimalLatitude
longitude <- gbif_data_france1$decimalLongitude

latitude <- gbif_data_france2$decimalLatitude
longitude <- gbif_data_france2$decimalLongitude

latitude <- gbif_data_france3$decimalLatitude
longitude <- gbif_data_france3$decimalLongitude

latitude <- gbif_data_france4$decimalLatitude
longitude <- gbif_data_france4$decimalLongitude

latitude <- gbif_data_france5$decimalLatitude
longitude <- gbif_data_france5$decimalLongitude

# New data frame for GBIF data (this time in matrix format)
data_gbif_coord <- data.frame(latitude, longitude)

# Same for Inat data
latitude <- inat1$latitude
longitude <- inat1$longitude

latitude <- inat2$latitude
longitude <- inat2$longitude

latitude <- inat3$latitude
longitude <- inat3$longitude

latitude <- inat4$latitude
longitude <- inat4$longitude

latitude <- inat5$latitude
longitude <- inat5$longitude

data_inat_coord <- data.frame(latitude, longitude)

# Define the original CRS of your data (assuming it's WGS84, EPSG:4326)
original_crs <- CRS("+proj=longlat +datum=WGS84")

# Define the new CRS you want to project your data into
new_crs <- CRS("+proj=utm +zone=32 +datum=WGS84")

# Convert your iNaturalist and Gbif data to SpatialPointsDataFrame
gbif_points <- SpatialPointsDataFrame(coords = data_gbif_coord[, c("longitude", "latitude")], data = data_gbif_coord, proj4string = original_crs)
inat_points <- SpatialPointsDataFrame(coords = data_inat_coord[, c("longitude", "latitude")], data = data_inat_coord, proj4string = original_crs)

# Project your data to the new CRS
gbif_points_proj <- spTransform(gbif_points, new_crs)
inat_points_proj <- spTransform(inat_points, new_crs)
extent(gbif_points_proj)

#Display the following plot in a external window
windows()
# Make a plot of the occurence data for all the species from Inat and Gbif
ggplot <- ggplot(data = France_crop) +
  geom_sf() +
  geom_point(data = matrix_full, aes(x = longitude, y = latitude, fill = source), size = 2, 
             shape = 23) + theme_classic() + coord_sf(xlim = c(-5, 10), ylim = c(40, 55))

print(ggplot)






###########################
###########################
###########################
# This was to draw a polygon but i don't need it anymore
###########################
if(FALSE){
# Create a polygon where we keep the data
polygon <- st_polygon(list(cbind(c(-5 ,-5 ,10 , 10 , -5) ,c(40, 55 , 55 , 40 , 40))))

# this function transform the polygon into a "single features" according to internet
polygon <- st_sfc(polygon, crs=4326)

# Overlap the polygon and the data
crop_data_france <- st_intersection(data_sf,polygon)

# Make sure this works
ggplot(data = polygon) +
  geom_sf() +
  geom_point(data = matrix_full, aes(x = longitude, y = latitude, fill = species), size = 2, 
             shape = 23) + theme_classic()

# Put this in the matrix full
matrix_full_crop <- data.frame(crop_data_france)

# take the single feature object and put it into coordinate
coordi <- st_coordinates(crop_data_france)

# Convert this coordinate matrix into a data frame format
coordi <- as.data.frame(coordi)

# Set the column names
colnames(coordi) <- c ("longitude","latitude")

# Put this into the data frame matrix_full
matrix_full <- data.frame(coordi,matrix_full_crop)
}

#length(unique(paste(occur5$decimalLatitude,occur5$decimalLongitude)))
#matrix$dupli <- paste(occur5$decimalLatitude,occur5$decimalLongitude)
#matrix <- matrix[!duplicated(matrix$dupli),]