library(sf)
library(rnaturalearth)
library(elevatr)
library(raster)
library(png)
library(magick)
library(ambient)
library(rayshader)
library(conflicted)

#######Cut France without overseas territorries (same as in the previous script)
extent(France)
ext_France_cut <- as(raster::extent(-5, 10, 40, 55), "SpatialPolygons")
extent(ext_France_cut)
France_crop <- crop(as_Spatial(France), ext_France_cut)
France_crop <- st_as_sf(France_crop)

#Get the map of the france into a raster format
elevation_France_crop <- get_elev_raster(France_crop, z = 6)

#Convert this raster image stored in an object into a matrix format
elmat <- raster_to_matrix(elevation_France_crop)

#Put the previous matrix on the same extent than the one of the France cropped
attr(elmat, "extent") <- extent(elevation_France_crop)

# add texture with a background image
elevation.texture.map <- readPNG("D:/rissm/Documents/Master/Biodiversity Data Analysis/Project/data/image.png")

# Create an object for each latitude and longitude data from the matrix full so containing the informations for all the species
latitude <- matrix_full$latitude
longitude <- matrix_full$longitude

# Create a data frame for GBIF data containing juste the coordinate data
gbif_coord <- data.frame(longitude, latitude)

# Store the CRS in an object and Create an object containing the coordinate data from GBIF converted into spatial points
#Then create a dataframe containing only the elevation data
ll_prj <- "EPSG:4326"
points <- sp::SpatialPoints(gbif_coord,
                            proj4string = sp::CRS(SRS_string = ll_prj))
elevation_points <- terra::extract(elevation_France_crop, points, method = "bilinear")
elevation_df <- data.frame(elevation = elevation_points)

#integrate the elevation data into the matrix full
matrix_full_elev <- data.frame(matrix_full, elevation_points)

# Delete unwanted lines
matrix_full_elev <- matrix_full_elev [!grepl("Coccinellidae", matrix_full_elev$species), ]
matrix_full_elev <- matrix_full_elev [!grepl("Hesperomyces virescens", matrix_full_elev$species), ]
matrix_full_elev <- matrix_full_elev [!grepl("Propylea quatuordecimpunctata", matrix_full_elev$species), ]

# Just control plot to verify that the script run entirely.
plot(elevation_France_crop)