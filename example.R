### Example codes to use 3D metric functions


# set the working directory to where the "coral_reef_3Dmodels" folder is located

library(tidyverse)
library(raster)
library(sf)
library(exactextractr)


### obtain a summary of 3D habitat metrics using 'habitat_metrics_matrix.R' and 'get_metrics_summary.R'

source("coral_reef_3Dmodels/habitat_metrics_matrix.R")
source("coral_reef_3Dmodels/get_metrics_summary.R")

# create a vector of file names under the wd for processing, it can be a single file
files <- c('coral_reef_3Dmodels/dems/WH_2022_S03_090_DEM_1cm.tif', 
           'coral_reef_3Dmodels/dems/WH_2022_S04_244_DEM_1cm.tif', 
           'coral_reef_3Dmodels/dems/WH_2022_S10_003_DEM_1cm.tif')

# get a summary of habitat metrics
habitat_metrics <- get_metrics_summary(files, resolution = 0.01)

# access surface complexity in the first file (example)
habitat_metrics[[1]]$surf_complex

# create a data frame of habitat metrics (example)
metrics_df <- data.frame(fac = numeric(0), 
                         mean_slope = numeric(0), 
                         sd_slope = numeric(0), 
                         mean_slope2 = numeric(0), 
                         sd_slope2 = numeric(0), 
                         mean_vrm = numeric(0), 
                         sd_vrm = numeric(0), 
                         mean_curvature = numeric(0), 
                         sd_curvature = numeric(0),
                         mean_profile_curvature = numeric(0), 
                         sd_profile_curvature = numeric(0), 
                         mean_plan_curvature = numeric(0), 
                         sd_plan_curvature = numeric(0), 
                         mean_profile_curvature = numeric(0), 
                         sd_proile_curv = numeric(0), 
                         surface_complexity = numeric(0), 
                         file = character(0))
for (i in 1:length(habitat_metrics)) {
  temp_df <- habitat_metrics[[i]]$metrics
  temp_df$surface_complexity <- habitat_metrics[[i]]$surf_complex
  temp_df$file <- names(habitat_metrics[i])
  metrics_df <- rbind(metrics_df, temp_df)
}

# resulting data frame of summary structural metrics at the original resolution (fac = 1),
# 2x resolution (fac = 4, if original resolution is 1 cm, then 2 cm), 
# 4x resolution (fac = 4, if original resolution is 1 cm, then 4 cm).
metrics_df

# save as a csv file
write.csv(metrics_df, file = "coral_reef_3Dmodels/habitat_metric_summary.csv")

# clear the environment
rm(list = ls())

###############################################################################

### obtain matrices of 3d habitat metrics using 'habitat_metrics_matrix.R' and 'get_metrics.R'
### this script works with a single DEM file and allow for detailed analysis of structural complexity, 
### looking at the whole distribution of habitat metrics from each cell of the DEM.

source("coral_reef_3Dmodels/habitat_metrics_matrix.R")
source("coral_reef_3Dmodels/get_metrics.R")

# read a DEM file
file <- c("coral_reef_3Dmodels/dems/WH_2022_S04_244_DEM_1cm.tif")
# get habitat metrics as matrices
habitat_metrics <- get_metrics(file, resolution = 0.01)

# extract summary metrics (examples)
habitat_metrics$surf_complex  # surface complexity
mean(habitat_metrics$slope, na.rm = TRUE) # mean slope
sd(habitat_metrics$slope, na.rm = TRUE) # sd slope

# obtain a vector of vrm_1x values (example)
vrm_1x <- as.vector(habitat_metrics$vrm_1x)
vrm_1x <- vrm_1x[!is.na(vrm_1x)]

# obtain a vector of vrm_2x values (example)
vrm_2x <- as.vector(habitat_metrics$vrm_2x)
vrm_2x <- vrm_2x[!is.na(vrm_2x)]

# visualize the distributions of vrm_1x and vrm_2x obtained from each cell of the plot
hist(vrm_1x)  # vrm_1x
hist(vrm_2x)  # vrm_2x

# visualize habitat metrics of the plot
# slope (example)
plot(raster(habitat_metrics$slope, 
            xmx = dim(habitat_metrics$slope)[2], 
            ymx = dim(habitat_metrics$slope)[1]))
# vrm 1x (example)
plot(raster(habitat_metrics$vrm_1x, 
            xmx = dim(habitat_metrics$vrm_1x)[2], 
            ymx = dim(habitat_metrics$vrm_1x)[1]))
# vrm 2x (example)
plot(raster(habitat_metrics$vrm_2x, 
            xmx = dim(habitat_metrics$vrm_2x)[2]*2, 
            ymx = dim(habitat_metrics$vrm_2x)[1]*2))
# vrm 4x (example)
plot(raster(habitat_metrics$vrm_4x, 
            xmx = dim(habitat_metrics$vrm_4x)[2]*4, 
            ymx = dim(habitat_metrics$vrm_4x)[1]*4))
# profile curvature
plot(raster(habitat_metrics$profile_curvature, 
            xmx = dim(habitat_metrics$profile_curvature)[2], 
            ymx = dim(habitat_metrics$profile_curvature)[1]))

# to save the structural data for future use
save(habitat_metrics, file = "coral_reef_3Dmodels/metrics_WH_2022_S04_244.rda")

# to load the structural data
load("coral_reef_3Dmodels/metrics_WH_2022_S04_244.rda")

# clear the environment
rm(list = ls())

###############################################################################

### obtain fractal dimension using 'fractal_dimension.R'

source("coral_reef_3Dmodels/fractal_dimension.R")

# create a vector of file names under the wd for processing
files <- c('coral_reef_3Dmodels/dems/WH_2022_S03_090_DEM_1cm.tif', 
           'coral_reef_3Dmodels/dems/WH_2022_S04_244_DEM_1cm.tif', 
           'coral_reef_3Dmodels/dems/WH_2022_S10_003_DEM_1cm.tif')

# get fractal dimension using 1x, 2x, 4x, 8x, 16x aggregation
fd16 <- get_fractal(files, resolution = 0.01, max_agg_8 = FALSE)

# get fractal dimension using 1x, 2x, 4x, 8x aggregation
fd8 <- get_fractal(files, resolution = 0.01, max_agg_8 = TRUE)

# access fractal dimension of the first file (example)
fd16[[1]]$fd
fd8[[1]]$fd

# make a plot of area information of the first file (example)
plot(log(s_area) ~ log(fac * 0.01), data = fd16[[1]]$area_information)
plot(log(s_area) ~ log(fac * 0.01), data = fd8[[1]]$area_information)

# create a data frame of fractal dimension (example)
fractal_df <- data.frame(file = character(0), 
                         fd16 = numeric(0), 
                         df8 = numeric(0))
for (i in 1:length(fd16)) {
  temp_df <- data.frame(file = names(fd16[i]), 
                        fd16 = fd16[[i]]$fd, 
                        fd8 = fd8[[i]]$fd)
  fractal_df <- rbind(fractal_df, temp_df)
}

fractal_df  # can be merged with metrics_df by 'file' column

# save as a csv file
write.csv(fractal_df, file = "coral_reef_3Dmodels/fractal.csv")

rm(list = ls())


###############################################################################

### obtain surface areas of polygons using 'surface_area.R'

source("coral_reef_3Dmodels/surface_area.R")

# read a DEM file
ras <- raster('coral_reef_3Dmodels/files/PHR_080619PM_DEM_1cm.tif')

# generate raster with 3D surface area
surf_area_ras <- surface_area_raster(ras, res = 0.01)

# read a shapefile
features <- st_zm(st_read('coral_reef_3Dmodels/files/PHR_080619PM_shapefiles/PHR_080619PM_digitized.shp'))
# CRS should be 'NA'

# visualize
plot(ras)
plot(features, add = TRUE)

# add planar surface area to polygons
features$area_2d <- as.numeric(st_area(features))
# remove if there are empty polygons
features <- features[features$area_2d != 0, ]
# add 3D surface area to polygons
features$area_3d <- exact_extract(surf_area_ras, features, fun='sum')

features

# get summary of benthic cover
benthic <- features %>% 
  group_by(species) %>% 
  summarise(area_2d = sum(area_2d), area_3d = sum(area_3d)) %>% 
  st_drop_geometry()

benthic

# save as a csv file
write.csv(benthic, file = "coral_reef_3Dmodels/benthic.csv")

rm(list = ls())

###############################################################################

### estimate volume of plot using 'volume_estimate.R'
### this function gives rough estimates, so use it with some caution.
### it gives better estimates when the resolution of DEM is higher.
### consider using the highest resolution possible given the accuracy of the 3D model
### if estimating something small like the volume of coral colonies

source('coral_reef_3Dmodels/volume_estimate.R')

ras <- raster('coral_reef_3Dmodels/files/PHR_080619PM_DEM_1cm.tif')

# generate raster with 3D surface area
volume_ras <- volume_estimate_raster(ras, res = 0.01)

# read a shapefile
features <- st_zm(st_read('coral_reef_3Dmodels/files/PHR_080619PM_shapefiles/PHR_080619PM_digitized.shp'))
# CRS should be 'NA'

# add planar surface area to polygons
features$area_2d <- as.numeric(st_area(features))
# remove if there are empty polygons
features <- features[features$area_2d != 0, ]
# add volume to polygons
features$volume <- exact_extract(volume_ras, features, fun='sum')

features

# get summary of volume per feature
volume <- features %>% 
  group_by(species) %>% 
  summarise(area_2d = sum(area_2d), volume = sum(volume)) %>% 
  st_drop_geometry()

volume

# save as a csv file
write.csv(volume, file = "coral_reef_3Dmodelsvolume.csv")

rm(list = ls())


