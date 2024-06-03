library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggstatsplot)
library(ggcorrplot)
library(ggfortify)
library(corrplot)
library(pheatmap)
library(randomcoloR)
library(emmeans)
library(dplyr)
library(plotly)

#######################
# Statistical Analysis
#######################

# This graph shows the distribution of the altitude data for each species
plt <- ggbetweenstats(
  data = matrix_full_elev,
  x = species,
  y = elevation_points
)
plt <- plt + 
    labs(
    x = "Species",
    y = "Elevation points",
    title = "Boxplot showing the distribution of the altitude data for each species"
  ) + 
theme(
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    plot.title.position = "plot", 
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )
plt <- plt  +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

ggplotly(plt)

# Interpretation : 
# With this graph, we can see that Myzus persicae is underrepresented. 
# Comparison with other species can be done but interpretation need to be careful.
# However, we can notice that for the ladybug species, they are approximately around the same number of observation. 
# We can see that Propylaea quatuordecimpunctata seems to bee distributed at a sligtly higher elevations than the other. 
# Same for Adalia bipunctata that have the lower mean. 
# All these comparison still need to be tested, these are just observations.


# Abundance of each species in each Landcover type
matrix_full_elev_eco <- na.omit(matrix_full_elev_eco)
Landcover <- as.factor(matrix_full_elev_eco$Landcover)
class(Landcover)
levels(Landcover)

histo <- ggplot(matrix_full_elev_eco, aes (x= Landcover, fill = species)) + 
   geom_bar(position = "dodge") + labs(title = "Abundance of the species depending on the landcover", x = "landcover type", y = "abundance" )
   theme_minimal


histo2 <- ggplot(matrix_full_elev_eco, aes (x= Landforms, fill = species)) + 
   geom_bar(position = "dodge") + labs(title = "Abundance of the species depending on the landform", x = "landform type", y = "abundance" )
   theme_minimal

# display them together
grid.arrange(histo, histo2, ncol = 2, respect = TRUE)

# Interpretation : 
# First we can see that Adalia Bipunctata is the dominant species in Settlement. 
# Then, there is a little abudance of all species in grassland and shrubland. 
# However, this can be also due to a lower sampling effort in these areas, As we already saw a bit with the different maps, 
# the data seems to be over represented around Paris, something that could explain that settlement contain a lot of observation. 
# In cropland, the abundance is higher and all the species seems to be approximately equally there, and as previously, Myzus is lower because
# it as a lower amount of observations. 
# In addition, ladybugs and Myzus persicae are oftenly studied from an agricultural point of view, something that could explain these higher abundance 
# in cropland compared to the other areas.
# All the species seems to be more present in Hills and plains compare to tableland and moutains. 
# Globally they are often found in low elevation ecosystem and observation are made mainly in an agricultarl context.

# Here is plot showing the distribution of the climatic data for each species
ggplot(matrix_full_elev_eco_clim, aes(x= vec_mean_temp ,fill=factor(species))) +
  geom_density(adjust = 1,alpha=0.5)+s
  scale_fill_manual(values=c("darkgreen","darkred","darkblue", "yellow", "pink"))+
  geom_histogram(aes(y=..density..), alpha=0.3, position="identity")+
  theme_classic()

ggplot(matrix_full_elev_eco_clim, aes(x= vec_mean_precip ,fill=factor(species))) +
  geom_density(adjust = 1,alpha=0.5)+s
  scale_fill_manual(values=c("darkgreen","darkred","darkblue", "yellow", "pink"))+
  geom_histogram(aes(y=..density..), alpha=0.3, position="identity")+
  theme_classic()

ggplot(matrix_full_elev_eco_clim, aes(x= vec_mean_wind ,fill=factor(species))) +
  geom_density(adjust = 1,alpha=0.5)+s
  scale_fill_manual(values=c("darkgreen","darkred","darkblue", "yellow", "pink"))+
  geom_histogram(aes(y=..density..), alpha=0.3, position="identity")+
  theme_classic()

# Interpretation : 
# All the curves are overlapping, meaning that all the species live in habitat with the same conditions. 
# These habitats have the same wind speed, temperature and precipitation. 
# However, the overlapping is in my opinion a bit too perfect. There might are an error somewhere that cause this
# but i can't figure out where the problem is so this interpretation is not that useful.


# Statistical tests
# Load and clean the dataset
df <- matrix_full_elev_eco_clim_sat
df <- na.omit(df)

# Separate continuous and discrete variables
df_continous <- df [,colnames(df) %in% c("vec_mean_temp","elevation_points","NDVI","vec_mean_precip", "vec_mean_wind")]
df_discrete <- df [,!(colnames(df) %in% c("vec_mean_temp","elevation_points","NDVI","vec_mean_precip", "vec_mean_wind"))]

# Compute the correlation matrix for continuous variables
mydata.cor <- cor(df_continous)

# Plot the correlation matrix with hierarchical clustering
my_corplot <- corrplot(mydata.cor, order = 'hclust', addrect = 3) #see correlation between variables

# Interpretation : 
# In this plot we can see that the biggest correlation are between wind and precipitation (negatively) 
# and precip and elevation (positively)
# This means that, for example, when the elevation increase, the amount of precipitation increase too and the wind speed decrease.

# Give another name more clear
data_stat <- data_stat <- matrix_full_elev_eco_clim_sat

# Create a scatter plot with a linear regression line
P1 <- ggplot(data = data_stat, mapping = aes(x = vec_mean_temp, y = vec_mean_precip))
P1 + geom_point(shape = 18) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic()

# Interpretation : 
# here we have the confirmation that other parameter are slighly correlated, i have tested for the other variables and they are all a bit correlated
# but i don't display the plot to not saturate the code
# In fact this is natural because everything is a bit correlated in an ecosystem

# Test to assess all the correlation :
cor.test(data_stat$vec_mean_temp, data_stat$vec_mean_precip)
# Temperature and precipitation are negatively correlated (see P-val below 0.05 and sample estimate : cor that is negative)

cor.test(data_stat$vec_mean_temp, data_stat$vec_mean_wind)
# Temperature and wind speed are positively correlated

cor.test(data_stat$vec_mean_wind, data_stat$vec_mean_precip)
# Wind and precipitation are negatively correlated
# This are individual test but let's assess it with a linear model

# Linear model
linear_model1 <- lm(vec_mean_temp ~ vec_mean_precip + vec_mean_wind + NDVI + elevation_points, data = data_stat)
summary(linear_model1) # Here all parameter are negatively correlated (estimate std) + Significant with P-value for each one
anova(linear_model1) # Confirmed

linear_model2 <- lm(vec_mean_precip ~ vec_mean_temp + vec_mean_wind + NDVI + elevation_points, data = data_stat)
summary(linear_model2) # Here all parameter are negatively correlated except NDVI and elevation + Significant with P-value for all except NDVI
anova(linear_model2) # Confirmed that all are significant even NDVI

linear_model3 <- lm(vec_mean_wind ~ vec_mean_temp + vec_mean_precip + elevation_points + NDVI, data = data_stat)
summary(linear_model3) # Here all parameter are negatively correlated except elevation + Significant with P-value for each one
anova(linear_model3) # Confirmed

linear_model4 <- lm(NDVI ~ vec_mean_temp + vec_mean_precip + vec_mean_wind + elevation_points, data = data_stat)
summary(linear_model4) # Here all parameter are negatively correlated except elevation and precip + Significant with P-value for each one except precip
anova(linear_model4) # Confirmed even for precip

linear_model5 <- lm(elevation_points ~ vec_mean_temp + vec_mean_precip + vec_mean_wind + NDVI, data = data_stat)
summary(linear_model5) # Here all parameter are negatively correlated except NDVI, precip and wind + Significant with P-value for each one
anova(linear_model5) # Confirmed

# With all these models, we can have a detailled view of the different correlation. 
# In fact, all the variables are correlated, something that seems to be normal in an ecosystem.

lm1 <- aov(vec_mean_temp ~ Landcover, data = data_stat)
lm2 <- aov(vec_mean_precip ~ Landcover, data = data_stat)
lm3 <- aov(vec_mean_wind ~ Landcover, data = data_stat)
lm4 <- aov(elevation_points ~ Landcover, data = data_stat)
lm5 <- aov(NDVI ~ Landcover, data = data_stat)

### Post-hoc test
# Perform ANOVA on the linear model
anova(lm1)
anova(lm2)
anova(lm3)
anova(lm4)
anova(lm5)

# Conduct post-hoc tests with Tukey adjustment
em1 <- emmeans(lm1, list(pairwise ~ Landcover), adjust = "tukey")
print(em1)
# No difference of Temperature between Sparsely/non vegetated grassland and forest 

em2 <- emmeans(lm2, list(pairwise ~ Landcover), adjust = "tukey")
print(em2)
# No difference of Precipitation between Sparsely/non and all other landcover except grassland

em3 <- emmeans(lm3, list(pairwise ~ Landcover), adjust = "tukey")
print(em3)
# No difference of wind between cropland and settlment, sparsely/non vegetated and cropland, grassland, settlement, shrubland 
# and between forest and grassland

em4 <- emmeans(lm4, list(pairwise ~ Landcover), adjust = "tukey")
print(em4)
# No difference of elevation between shrubland and cropland and settlement

em5 <- emmeans(lm5, list(pairwise ~ Landcover), adjust = "tukey")
print(em5)
# No difference of vegetation density (NDVI) between shrubland and cropland and grassland and between settlement and sparsely/non vegetated


# Do the same but with the Landforms
lm6 <- aov(vec_mean_temp ~ Landforms, data = data_stat)
anova(lm6)

lm7 <- aov(vec_mean_precip ~ Landforms, data = data_stat)
anova(lm7)

lm8 <- aov(vec_mean_wind ~ Landforms, data = data_stat)
anova(lm8)

lm9 <- aov(elevation_points ~ Landforms, data = data_stat)
anova(lm9)

lm10 <- aov(NDVI ~ Landforms, data = data_stat)
anova(lm10)

# Tukey verification
em6 <- emmeans(lm6, list(pairwise ~ Landforms), adjust = "tukey")
print(em6)
# Difference of temperature between all the Landforms

em7 <- emmeans(lm7, list(pairwise ~ Landforms), adjust = "tukey")
print(em7)
# Same for precipitation

em8 <- emmeans(lm8, list(pairwise ~ Landforms), adjust = "tukey")
print(em8)
# No difference of wind speed between "Plateau" and hills and plains

em9 <- emmeans(lm9, list(pairwise ~ Landforms), adjust = "tukey")
print(em9)
# Difference of altitudes between all the Landformes

em10 <- emmeans(lm10, list(pairwise ~ Landforms), adjust = "tukey")
print(em10)
# No difference of density of vegetation between Hills and plains

### PCA
df_continous <- apply(df_continous,2,as.numeric)

# Now perfom the PCA
pca_res <- prcomp(df_continous, scale. = TRUE)
windows()
# Make a plot of the PCA with the matrix 
autoplot(pca_res, data =df_discrete, colour = 'species', 
          shape = "Landcover", 
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') + theme_classic()

autoplot(pca_res, data =df_discrete, colour = 'species',  
        shape = "Landforms",
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') + theme_classic()

# Interpretation : 
# In both PCA the ellipses are all overlapping nearly entirely. 
# This is coherent with the previous results. 
# All the parameter in Landforms and Landcover are rougly correlated execpt few ones. 
# With this, we can't distinguishe clear trend in the ecological niches of the species. 
# They seems to lives all in the sames ecosystems. 

#####################################################################
######################   FINAL CONCLUSION     #######################
#####################################################################
if(FALSE) {

As a final conclusion, it is difficult to identify clear trends. 
The initial questions was to know if the number of points on the ladybugs would represents their "skills" 
to predate the Aphids "Myzus persicae". 
With all the plots and tests, we can see that both in Landocover and in Landforms types, the variables are nearly all correlated.
This means that we cannot know what is influencing the distribution of the species. 
With the PCA and the geom_density plots we can see that the curves and ellipses are overlapping. 
These species seems to be present in the same environmental conditions with very near ecological niches. 
Potential hypotheses would be that the aphids would be predated by only one of the ladybugs species and the others would predate other species of aphids.
species of aphid, or the ladybugs would all predate the aphids and be in competition with each other but this is, in my opinion, less possible.


Finally, looking at the last map in the "Maps" script, we can see that the obeservations are mainly around the Paris region. 
This is creeating an overrepresentation of the species at this place compared to the other region in France. 
However, we can see that all the species are widely distributed at the same places. 
This confirms the previous plots and the PCA showing that they live in the same places. 


To come back to the initial hypothesis, with our results we can not give a clear answer. 
Any of our results can confirm of reject the hypothesis. 
As always, further analysis would be needed, with a more complexe and detailled data set. 
Sampling biais would be also something to take into account as well as the biology of each species.
}