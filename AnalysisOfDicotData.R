# The effect of environmental conditions on dicot plant biodiversity -----------


# Experimental overview --------------------------------------------------------
#
# This study investigated the effects of an environmental gradient on the
# biodiversity of dicot plant species. Morphological keys were
# used to estimate percentage cover of target herb species along a 12m gradient
# of known temperature and moisture levels. PCR, Sanger Sequencing and BLAST
# were used to confirm identification of species.


# Description of data ----------------------------------------------------------
#
# All data is stored in the data-raw folder.
#
# The numerical data is stored in the class_data.xlsx file. This is an excel
# file where columns represent the position of sample , temperature , moisture
# and  species names. Rows represent unique samples taken. Species values are
# percent  cover, moisture is percent , temperature is celcius and position is
# meters.



# Analysis overview ------------------------------------------------------------
#
# This script imports, explores and analyses the relationship and significance
# between dicot plant species and moisture/temperature. Shannon index with cooks
# distance followed by spearmans rank was used to test significance. Further
# analysis tested specific species and a coor plot was used to compare species.
# Figures are created for these results, some of which are in the report.

# Packages required ------------------------------------------------------------
# ------------------------------------------------------------------------------

# for import, manipulation and plotting
library("tidyverse")

# for reading excel files
library("readxl")

# for shannon index
library("vegan")

# for coor plot
library("ggcorrplot")

# Data import ------------------------------------------------------------------
# ------------------------------------------------------------------------------

dicot <- read_excel("data-raw/class_data.xlsx")

# Data exploration -------------------------------------------------------------
# ------------------------------------------------------------------------------

# Checking if samples were taken evenly
ggplot(data = dicot, aes(x = Position)) +
  geom_histogram(bins=10) +
  xlab("Elevation Along the Hill (m)")

# Interpretation : unevenly distributed with 4m lacking replicates so
# conculusions at 4km are less reliable 

# Checking relationship between temp, elevation and moisture
ggplot(data = dicot, aes(x = Position, y = temperature)) +
  geom_point() +
  xlab("Elevation Along the Hill (m)") +
  ylab("Temperature")

# Interpretation : temperature increases steadily as elevation increases,
# meaning the two variables are correlated and will have similar effects
# on diversity

ggplot(data = dicot, aes(x = Position, y = moisture)) +
  geom_point() +
  xlab("Elevation Along the Hill (m)") +
  ylab("Moisture")

# Interpretation : moisture decreases as elevation increases, again correlated
# with position meaning all three environmental variables change together
# along the gradient

ggplot(data = dicot, aes(x = temperature, y = moisture)) +
  geom_point() +
  xlab("Temperature") +
  ylab("Moisture")

# Interpretation : moisture decreases as temperature increases, confirming
# that temperature and moisture are not independent along this gradient,
# this makes it difficult to separate their individual effects on diversity

# Calculating shannon index ----------------------------------------------------
# Plotting shannon diversity against each environmental variable to check
# for visible patterns before running formal statistics

speciesData <- dicot[,6:16]
plantDiversity <- diversity(speciesData, index = "shannon")

# Plot of shannon vs elevation
df <- data.frame("position" = dicot$Position,
                 "diversity" = plantDiversity)

ggplot(data = df, aes(x = position, y = diversity)) +
  geom_point() +
  xlab("Position (m)") +
  ylab("Shannon Index")

# Interpretation : Shannon index tends to increase with increased elevation

# Plot of shannon vs temperature
ggplot(data = dicot, aes(x = temperature, y = plantDiversity)) +
  geom_point() +
  xlab("Temperature (°C)") +
  ylab("Shannon Index")

# Interpretation : Shannon index tends to increase with increased temperature

# Plot of shannon vs moisture
ggplot(data = dicot, aes(x = moisture, y = plantDiversity)) +
  geom_point() +
  xlab("Moisture") +
  ylab("Shannon Index")

# Interpretation : Shannon index tends to decrease with increased moisture


# Removing outliers and performing linear regression for position --------------
# ------------------------------------------------------------------------------

# Make initial model
out = lm(plantDiversity~dicot$Position)
summary(out)

# Call:
# lm(formula = plantDiversity ~ dicot$Position)

# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.92309 -0.07204  0.02786  0.17474  0.44261

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)    1.157822   0.053852  21.500  < 2e-16 ***
#   dicot$Position 0.046058   0.007912   5.821  1.1e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2656 on 82 degrees of freedom
# Multiple R-squared:  0.2924,	Adjusted R-squared:  0.2838
# F-statistic: 33.88 on 1 and 82 DF,  p-value: 1.097e-07

# Interpretation : for every 1m increase in position, plant diversity increases
# by ~0.046. This is very significant (***)



# Calculate Cook's distance
cooksD <- cooks.distance(out)
outlierValues <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
outlierValues

dicotTableMinusOutlier <- dicot[-as.numeric(names(outlierValues)),]

plantDiversityMinusOutliers <- diversity(
  dicotTableMinusOutlier[, 6:16],
  index = "shannon")

df <- data.frame("position" = dicotTableMinusOutlier$Position,
                 "diversity" = plantDiversityMinusOutliers)

ggplot(data = df, aes(x = position, y = diversity)) +
  geom_point() +
  xlab("Position (m)") +
  ylab("Shannon Index")

# Spearmans Correlation
cor.test(dicotTableMinusOutlier$Position, plantDiversityMinusOutliers,
         method = "spearman")

# data: dicotTableMinusOutlier$Position and plantDiversityMinusOutliers
# S = 32726, p-value = 1.147e-09
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho = 0.6164378

# Interpretation : moderately strong correlation (0.616) between position and
# plant diversity which is highly significant (1.147e-09).


# Fitting a linear regression
out <- lm(diversity ~ position, data = df)
summary(out)

position_shannon <- ggplot(dat = df, aes(x = position, y = diversity)) +
  geom_point(alpha = 0.7) +
  theme_classic() +
  xlab("Position (m)") +
  ylab("Shannon Index") +
  stat_smooth(method = "lm",
              colour = "red")

position_shannon

# Save it to file
ggsave("./figures/position_shannon.tif", 
       plot = position_shannon, 
       device = "tiff",
       width = 6, 
       height = 6,
       units = "in",
       dpi = 300)

# Removing outliers and performing linear regression for temperature -----------
# ------------------------------------------------------------------------------

# Make initial model
out = lm(plantDiversity~dicot$temperature)
summary(out)

# Call:
#   lm(formula = plantDiversity ~ dicot$temperature)
#
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.97746 -0.06969  0.03283  0.18746  0.47689 
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.73897    0.13501   5.474 4.70e-07 ***
#   dicot$temperature  0.08975    0.01730   5.188 1.51e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.274 on 82 degrees of freedom
# Multiple R-squared:  0.2471,	Adjusted R-squared:  0.238 
# F-statistic: 26.92 on 1 and 82 DF,  p-value: 1.507e-06

# Interpretation : for every 1 degree increase in temperature, plant diversity
# increases by ~0.09.

# Calculate Cook's distance
cooksD <- cooks.distance(out)
outlierValues <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
outlierValues

dicotTableMinusOutlier <- dicot[-as.numeric(names(outlierValues)),]

plantDiversityMinusOutliers <- diversity(
  dicotTableMinusOutlier[, 6:16],
  index = "shannon")

df <- data.frame("temperature" = dicotTableMinusOutlier$temperature,
                 "diversity" = plantDiversityMinusOutliers)

ggplot(data = df, aes(x = temperature, y = diversity)) +
  geom_point() +
  xlab("Temperature (°C)") +
  ylab("Shannon Index")

# Spearmans Correlation
cor.test(dicotTableMinusOutlier$temperature, plantDiversityMinusOutliers,
         method = "spearman")

# data: dicotTableMinusOutlier$temperature and plantDiversityMinusOutliers
# S = 32726, p-value = 1.147e-09
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho = 0.6164378

# Interpretation: a strong positive monotonic relationship between temperature
# and diversity, mirroring the position result because temperature and position
# are tightly correlated along this gradient.

# Fitting a linear regression
out <- lm(diversity ~ temperature, data = df)
summary(out)

temperature_shannon <- ggplot(dat = df, aes(x = temperature, y = diversity)) +
  geom_point() +
  theme_classic() +
  xlab("Temperature (°C)") +
  ylab("Shannon Index") +
  stat_smooth(method = "lm")

temperature_shannon

# Save it to file
ggsave("./figures/temperature_shannon.tif", 
       plot = temperature_shannon, 
       device = "tiff",
       width = 6, 
       height = 6,
       units = "in",
       dpi = 300)

# Removing outliers and performing linear regression for moisture --------------
# Results are expected to be the mirror of the previous two regressions as
# moisture is inversely coorelated with position and temperature

# Make initial model
out = lm(plantDiversity~dicot$moisture)
summary(out)

# Call:
# lm(formula = plantDiversity ~ dicot$moisture)

# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.92309 -0.07204  0.02786  0.17474  0.44261

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# #(Intercept)     1.829726   0.075795  24.140  < 2e-16 ***
#   dicot$moisture -0.029169   0.005011  -5.821  1.1e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2656 on 82 degrees of freedom
# Multiple R-squared:  0.2924,	Adjusted R-squared:  0.2838
# F-statistic: 33.88 on 1 and 82 DF,  p-value: 1.097e-07

# Interpretation : for every unit increase in moisture, plant diversity
# decreases by ~0.029

# Calculate Cook's distance
cooksD <- cooks.distance(out)
outlierValues <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
outlierValues

dicotTableMinusOutlier <- dicot[-as.numeric(names(outlierValues)),]

plantDiversityMinusOutliers <- diversity(
  dicotTableMinusOutlier[, 6:16],
  index = "shannon")

df <- data.frame("moisture" = dicotTableMinusOutlier$moisture,
                 "diversity" = plantDiversityMinusOutliers)

ggplot(data = df, aes(x = moisture, y = diversity)) +
  geom_point() +
  xlab("Moisture") +
  ylab("Shannon Index")

# Spearmans Correlation
cor.test(dicotTableMinusOutlier$moisture, plantDiversityMinusOutliers,
         method = "spearman")

# data: dicotTableMinusOutlier$moisture and plantDiversityMinusOutliers
# S = 137914, p-value = 1.147e-09
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho = -0.6164378

# Interpretation : moderately strong negative correlation (rho = -0.616)
# between moisture and plant diversity which is highly significant
# (p = 1.147e-09) , higher moisture is associated with lower diversity


# Fitting a linear regression
out <- lm(diversity ~ moisture, data = df)
summary(out)

moisture_shannon <- ggplot(dat = df, aes(x = moisture, y = diversity)) +
  geom_point() +
  theme_classic() +
  xlab("Moisture") +
  ylab("Shannon Index") +
  stat_smooth(method = "lm")

moisture_shannon

# Save it to file
ggsave("./figures/moisture_shannon.tif", 
       plot = moisture_shannon, 
       device = "tiff",
       width = 6, 
       height = 6,
       units = "in",
       dpi = 300)

# Specific species analysis excluding outliers ---------------------------------
# ------------------------------------------------------------------------------

pvals <- apply(dicotTableMinusOutlier[, 6:16], 2, function(species){ # looping

  # create a data frame with all required data for the linear regression
  df = data.frame(abundance = species,
                  positions = dicotTableMinusOutlier$Position)

  # perform a linear regression, with precipitation and temperature as input
  # variables and class abundance as the output variable
  out = lm(abundance ~ positions, data = df)

  # extract the p-values of these fits
  sumres = summary(out)
  pf(sumres$fstatistic[1L],
     sumres$fstatistic[2L],
     sumres$fstatistic[3L],
     lower.tail = FALSE)

})

# Adjust p-values for multiple comparisons
p.adjust(pvals)

# Extracting species name with significant associations with elevation
names(which(p.adjust(pvals)<0.05))

# [1] "Centaurea_nigra" p = 0.00597

# Interpretation: of the 11 species tested, only Centaurea nigra shows a
# significant association with position.


# Scatterplot of species J -----------------------------------------------------
# A linear regression of abundance on position is fitted for each species and
# the resulting p-values are adjusted for multiple comparisons.

c_nigra <- ggplot(data = dicot, aes(x = Position, y = Centaurea_nigra)) +
  geom_point(alpha = 0.7) +
  theme_classic() +
  stat_smooth(method = "lm" ,
              colour = "red") +
  xlab("Position (m)") +
  ylab("Abundance (Centaurea nigra)")

c_nigra

# Save it to file
ggsave("./figures/c_nigra.tif", 
       plot = c_nigra, 
       device = "tiff",
       width = 6, 
       height = 6,
       units = "in",
       dpi = 300)

# Running a spearmans rank on all species
spearman_pvals <- apply(speciesData, 2, function(species) { # looping
  cor.test(dicot$Position, species, method = "spearman")$p.value
})

p.adjust(spearman_pvals)
names(which(p.adjust(spearman_pvals) < 0.05))

# [1] "Centaurea nigra"

# Interperation: the spearmans test agrees with the linear regression, only 
# Centaurea nigra is significant 


# Species correlation plot -----------------------------------------------------
# Examines whether some species co-occur or exclude eachother. Only significant
# correlations p < 0.05 are shown.

colnames(speciesData) <- gsub("_", " ", colnames(speciesData))
corr <- round(cor(speciesData), 1)
p.mat <- cor_pmat(speciesData)

correlation <- ggcorrplot(corr,
                hc.order = TRUE,
                type = "upper",
                outline.col = "white",
                method = "circle",
                p.mat = p.mat,
                sig.level = 0.05,
                insig = "blank",
                colors = c("#74ADD1", "white", "#F46D43"),
                ggtheme = theme_classic(),
                lab = TRUE,
                lab_size = 3) +

  scale_fill_gradient2(
    name = "Correlation",
    low = "#74ADD1",
    mid = "white",
    high = "#F46D43",
    limits = c(-1, 1)
  ) +

  theme(
    axis.text.x = element_text(face = "italic",
                               angle = 45,
                               hjust = 1),
    axis.text.y = element_text(face = "italic")
  )

correlation

# Save it to file
ggsave("./figures/correlation.tif", 
       plot = correlation, 
       device = "tiff",
       width = 6, 
       height = 6,
       units = "in",
       dpi = 300)

# Interpretation : Some species are positively correlated with eachother, no 
# species exclude eachother. 

# Citations --------------------------------------------------------------------

# R Core Team (2024). R: A Language and Environment for Statistical Computing.
# R Foundation for Statistical Computing, Vienna, Austria.
# https://www.R-project.org/

# Wickham H, et al. (2019). Welcome to the tidyverse. Journal of Open Source
# Software, 4(43), 1686. https://doi.org/10.21105/joss.01686

# Wickham H, Bryan J (2023). readxl: Read Excel Files. R package version 1.4.3.
# https://CRAN.R-project.org/package=readxl

# Oksanen J, et al. (2025). vegan: Community Ecology Package. R package
# version 2.7-2. https://CRAN.R-project.org/package=vegan

# Kassambara A (2023). ggcorrplot: Visualization of a Correlation Matrix using
# 'ggplot2'. R package version 0.1.4.1.
# https://CRAN.R-project.org/package=ggcorrplot
