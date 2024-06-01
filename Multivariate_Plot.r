library(ggfortify)
library(vegan)
library(plotly)

# Assign the matrix to another object to work on it without modifying the original version
df <- matrix_full_elev_eco_clim_sat

# Here delete the NA for the PCA, if not made the PCA will not work
df <- na.omit(df, na.rm = TRUE)

# create a matrix with only the selected columns
df_continous <- df [,colnames(df) %in% c("vec_mean_temp","elevation_points","NDVI","vec_mean_precip", "vec_mean_wind")]
# Create a matrix with all the columns of matrix full, except the one selected
df_discrete <- df [,!(colnames(df) %in% c("vec_mean_temp","elevation_points","NDVI","vec_mean_precip", "vec_mean_wind"))]

# Convert the matrix into a numeric format
df_continous <- apply(df_continous,2,as.numeric)

# Now perfom the PCA
pca_res <- prcomp(df_continous, scale. = TRUE)
windows()
# Make a plot of the PCA with the matrix 
autoplot(pca_res, data =df_discrete, colour = 'species', 
          shape = "Landcover", 
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') + theme_classic()

ggplotly

# Creating a ID to each line of data (to give to each line a unique name)
row.names(df_continous) <- c(1:nrow(df_continous))

# Create a distance matrix (calculate distance between rows of the matrix full continous)
distance_matrix <- vegdist(df_continous, method = "euclidean")

# This function is used to calculate the coordinate of each point of the matrix in 3 dimension
D3_data_dist <- cmdscale(distance_matrix, k = 3)

# Convert the 3D distance matrix into a data frame format
D3_data_dist <- data.frame(D3_data_dist)

# Put the columns species of the matrix full into an object 
Species <- df_discrete$species

# Make a plot of the PCA from the 3D distance matrix
PCA <- ggplot(D3_data_dist, aes(x = X1, y = X2, color = Species)) +
  geom_point() + ggtitle("PCA for the 5 species") +
  theme_classic()
PCA

### interactive version of the PCA
intercative_pca <- ggplotly(PCA)
intercative_pca

# Perform PCA on this matrix
pca <- princomp(df_continous, scores=T, cor=T)

# Extract the score of each line in the 3 Dimension (store the coordinate in a matrix and then create an object for each dimension and assign the score for this dimension)
scores <- pca$scores
x <- scores[,1]
y <- scores[,2]
z <- scores[,3]

# Extract the loading of the PCA (correlation between the variable and the Principal Component explaining the more variances)
loads <- pca$loadings

# Extract the row names of the previous matrix 
load_names <- rownames(loads)

# Scale factor for loadings
scale.loads <- 5

# Make the 3D plot of the PCA
p <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="markers", color = df_discrete$species)

# Then add the lines representing the loadings to the plot
for (k in 1:nrow(loads)) {
   x <- c(0, loads[k,1])*scale.loads
   y <- c(0, loads[k,2])*scale.loads
   z <- c(0, loads[k,3])*scale.loads
   p <- p %>% add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="lines",
            line = list(width=8),
            opacity = 1,
            name = load_names[k])  # Adding names to the loadings
}

# Display the plot
print(p)
