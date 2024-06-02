library(sf)
library(elevatr)
library(raster)
library(tidyverse)
library(RColorBrewer)
library(rayshader)
library(eks)
library(ggthemes)

sf_use_s2(FALSE)

#create a plot with the density of observations and so probability of presence (Kernel density)
sf_points <- data.frame(
    lat = matrix_full_elev_eco_clim$latitude,
    lon = matrix_full_elev_eco_clim$longitude
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
plot(sf_points)
skde <- st_kde(sf_points, gridsize = c(100, 100))
plot(skde)
dataxx = st_get_contour(skde, cont = c(seq(1, 99, 5)), disjoint = FALSE)

# Create a function to generate the color palette
color_palette <- colorRampPalette(c("darkolivegreen4","darkolivegreen3","darkseagreen1","yellow","orange","red","darkred"))

# Define the number of colors in the palette
num_colors <- 20  # Adjust as needed

# Generate the color palette
palette <- color_palette(num_colors)
elmat <- raster_to_matrix(elevation_France_crop)

# 3D plot of all the occurence and probability of presence in background (Kernel Density)
elmat %>%
 sphere_shade(texture = "bw") %>%
 add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.9)  %>%
 add_overlay(generate_polygon_overlay(dataxx, 
                        palette = palette, linewidth=0,
                        extent = extent(elevation_France_crop), heightmap = elmat),
                        alphalayer=0.7)  %>%
plot_3d(elmat, zscale = 150, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))
render_points(
  extent = extent(France_crop), size = 2,
  lat = gbif_coord$latitude, long = gbif_coord$longitude,
  altitude = elevation_points + 100, zscale = 150, color = "blue"
)

windows()

# Map of the occurence in france for each species
ggplot(data = France_crop) +
  geom_sf() +
  geom_point(data = matrix_full_elev_eco_clim, aes(x = longitude, y = latitude, fill = species), size = 1, 
             shape = 22) + 
             theme_solarized() + 
             theme(axis.text = element_text(color = "blue"), axis.title = element_text(color = "red", face = "bold"),
  legend.background = element_rect(fill = "lightblue"), legend.title = element_text(face = "italic"),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) + coord_sf(xlim = c(-5, 10), ylim = c(40, 55)) + 
  labs(title = "Distribution of Species in France", x = "Longitude", y = "Latitude")




