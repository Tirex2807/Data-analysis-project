library(ggplot2)
library(ggcorrplot)
library(ggfortify)
library(corrplot)
library(pheatmap)
library(randomcoloR)
library(emmeans)
library(dplyr)

# Load and clean the dataset
df <- matrix_full_elev_eco_clim_sat
df <- na.omit(df)

# Separate continuous and discrete variables
df_continous <- df [,colnames(df) %in% c("vec_mean_temp","elevation_points","NDVI","vec_mean_precip", "vec_mean_wind")]
df_discrete <- df [,!(colnames(df) %in% c("vec_mean_temp","elevation_points","NDVI","vec_mean_precip", "vec_mean_wind"))]

##### Correlation Matrix and Corplot
# Compute the correlation matrix for continuous variables
mydata.cor <- cor(df_continous)

# Plot the correlation matrix with hierarchical clustering
my_corplot <- corrplot(mydata.cor, order = 'hclust', addrect = 3) #see correlation between variables

##### Annotated Heatmap 
### Simple heat map using only numeric values 
# Prepare data for heatmap
data <- df_continous
row.names(data) <- c(1:nrow(data))

# Generate a basic heatmap (See the relation between variables and the different area that we have)
heatmap(scale(data))

### Advanced heat map with annotation 
## Factor for annotation
my_group <- df_discrete[c("Landcover")]  # Only use "Landcover" for annotation
row.names(my_group) <- c(1:nrow(my_group))

# Generate an advanced heatmap with annotations
pheatmap(scale(data),
         annotation_row = my_group)

#### Customize the heatmap
# Define custom colors for the heatmap
data_col <- grDevices::colorRampPalette(c("black", "darkgreen", "white", "darkred"))

# Display the customized heatmap in a new window
# Here this is a custom version with other colors.
ht <- pheatmap(scale(data),
         annotation_row = my_group,
         cutree_rows = 2,
         cutree_cols = 2,
         cellwidth = 100,
         cellheight = 0.2,
         color = data_col(10))
ht

######################################################
#### Basic Statistics 
#### Correlation between continuous variables 
# Prepare data for statistical analysis
data_stat <- df_continous

# Create a scatter plot with a linear regression line
P <- ggplot(data = data_stat, mapping = aes(x = vec_mean_temp, y = vec_mean_precip))
P + geom_point(shape = 18) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic()

# Perform Pearson correlation test
cor.test(data_stat$vec_mean_temp, data_stat$vec_mean_precip)

# Fit a linear model
linear_model <- lm(vec_mean_temp ~ vec_mean_precip, data = data_stat)
summary(linear_model) # Here negatively correlated (estimate std of Vec_mean_precip) + Significant with P-value
anova(linear_model)

#### Factor Analysis
# Use the original dataset for factor analysis
data_stat <- matrix_full_elev_eco_clim_sat

# Create a boxplot for temperature by landcover type
P_fact <- ggplot(data = data_stat, mapping = aes(x = Landcover, y = vec_mean_temp, fill = Landcover))

P_fact <- P_fact + geom_boxplot(varwidth = TRUE, outlier.shape = NA) +  # Change boxplot width 
  geom_jitter(alpha = 0.2, size = 2, width = 0.1) +  # Add points and spread them
  stat_summary(fun = mean, shape = 13, size = 1, colour = "darkgreen") +  # Add mean 
  theme_classic()

print(P_fact)
# Interactive version !
require(ggplotly)
ggplotly(P_fact)
# Fit a linear model with landcover as a factor
linear_model <- lm(vec_mean_temp ~ Landcover, data = data_stat)

######################################################
### Post-hoc test
# Perform ANOVA on the linear model
anova(linear_model)

# Conduct post-hoc tests with Tukey adjustment
em <- emmeans(linear_model1, list(pairwise ~ Landcover), adjust = "tukey")
print(em)

######### Data Aggregation and Formatting for Plots
# Aggregate data by W_Ecosystm
aggregated_data <- aggregate(
  cbind(elevation_points, vec_mean_precip, vec_mean_temp, NDVI) ~ W_Ecosystm, 
  data = data_stat, 
  FUN = mean
)

# Print the aggregated data
print(aggregated_data)

############## Add factor value: 
# Extract species and W_Ecosystm columns
data_stat_discrete <- data_stat[c("species", "W_Ecosystm")]

# Merge aggregated data with discrete data by W_Ecosystm
aggregated_data_final_species <- merge(aggregated_data, data_stat_discrete, by = "W_Ecosystm")

# Ensure unique rows in the final aggregated data
aggregated_data_final_species <- aggregated_data_final_species %>% distinct()