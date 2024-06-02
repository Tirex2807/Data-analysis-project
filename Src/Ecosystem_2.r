library(raster)
library(elevatr)
library(geodata)

# Set the file path to your GeoTIFF raster 
file_path <- "D:/rissm/Documents/Master/Biodiversity Data Analysis/Project/data/WorldEcosystem.tif"

# Read the raster GeoTIFF and create an object for it
ecosystem_raster <- raster(file_path)

# Cut the ecosystem raster to keep only the Map / informations for France
r2 <- crop(ecosystem_raster, extent(France_crop))
ecosystem_france <- mask(r2, France_crop)

# Create an object containing spatial point informations (here coordinates) from the matrix full elev
spatial_points <- SpatialPoints(coords = matrix_full_elev[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))

# Extract values these spatial points and ecosystemic data and put it into an object 
eco_values <- raster::extract(ecosystem_france, spatial_points)

# Integrate these informations into the matrix full 
matrix_full_elev_eco <- data.frame(matrix_full_elev, eco_values)

# Set the file path to your Metadata
metadat_eco <- read.delim("D:/rissm/Documents/Master/Biodiversity Data Analysis/Project/data/WorldEcosystem.metadata.tsv")

# Integrate the metadata into the matrix full juste made before
matrix_full_elev_eco <- merge(matrix_full_elev_eco, metadat_eco, by.x="eco_values", by.y="Value", all.x =T)

#Delete the unwanted informations from metadata
#matrix_full_elev_eco[, c("Blue", "Red", "Green")] <- list(NULL)

# Control plot
plot(ecosystem_france)
plot(spatial_points,add=T,pch=16,cex=2)






# random code not useful
###########################
###########################
###########################
###########################
if(FALSE){
matrix_full_cocci <- matrix_full_elev_eco[!(matrix_full_elev_eco$species=="Myzus persicae"), ]
histo <- ggplot(matrix_full_cocci, aes (x= Landcover, fill = species)) + 
   geom_bar(position = "dodge") + labs(title = "Abundance of the species depending on the landcover", x = "landcover type", y = "abundance" )
   theme_minimal

print(histo)

matrix_full_cocci$species[matrix_full_cocci$species=="Coccinellidae"] <- "nom de l'espèce a remplacer"

#T apply : 
group_means <- tapply(matrix_full_elev_eco$longitude, matrix_full_elev_eco$species, mean, na.rm=TRUE) 
print(group_means)

#LOOPS : 
   for ( i in 1:5)

   {
      print(i)
   }
}