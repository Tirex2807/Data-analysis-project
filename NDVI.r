library(remotes)
library(rgeoboundaries)
library(sf)
library(raster)
library(here)
library(ggplot2)
library(viridis)
library(MODIStsp)

##run the following next ligne together
#Downloading the country boundary of France
#map_boundary <- geoboundaries("France")
#dir.create("./data/modis", recursive = TRUE)
#Defining filepath to save downloaded spatial file
#spatial_filepath <- "./data/modis/france.shp"
#Saving downloaded spatial file on to our computer
#st_write(map_boundary, paste0(spatial_filepath))

# check available data
MODIStsp_get_prodnames()

# Get the data
#MODIStsp_get_prodlayers("M*D13Q1")
#Download the data (set different parameters)
if(FALSE) {
MODIStsp(
  gui = FALSE,
  out_folder = "./data/modis",
  out_folder_mod = "./data/modis",
  selprod = "Vegetation Indexes_16Days_250m (M*D13Q1)",
  bandsel = "NDVI",
  user = "mstp_test",
  password = "MSTP_test_01",
  start_date = "2020.06.01",
  end_date = "2020.06.01",
  verbose = FALSE,
  spatmeth = "file",
  spafile = spatial_filepath,
  out_format = "GTiff"
)
}

# Downloading the boundaries of France
map_boundary <- geoboundaries("France")

# Reading in the downloaded NDVI raster data (store the image into an object, use stack to store all the layer without overwritting)
NDVI_raster <- raster("./data/MYD13Q1_NDVI_2020_153.tif")

# Transforming the data (set the raster data into the project CRS)
NDVI_raster <- projectRaster(NDVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Keep only boundaries of France and fit the raster image for france with it
# Then display the result (we see that there is only france now compare to before where the surrounding countries where also present)
NDVI_raster <- raster::mask(NDVI_raster, as_Spatial(map_boundary))

# Dividing values by 10000 to have NDVI values between -1 and 1 (Set the size of the pixels)
gain(NDVI_raster) <- 0.0001

# Extract values from the raster at the place where the spatial points data are
NDVI <- raster::extract(NDVI_raster, spatial_points)

# Add the satellite data to the matrix full
matrix_full_elev_eco_clim_sat <- data.frame(matrix_full_elev_eco_clim, NDVI )

# Control plot
plot(NDVI_raster)