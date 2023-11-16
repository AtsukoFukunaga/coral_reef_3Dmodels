### Example codes to use 3D metric functions


# set the working directory to where the .R files and "files" folder are using setwd()

library(raster)
library(sf)
library(exactextractr)


### obtain matrices of 3d habitat metrics using 'habitat_metrics_matrix.R' and 'get_metrics.R'

source("habitat_metrics_matrix.R")
source("get_metrics.R")

# read a DEM file
file <- c("files/PHR_080619PM_DEM_1cm.tif")
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

rm(list = ls())


###############################################################################

### obtain a summary of 3D habitat metrics using 'habitat_metrics_matrix.R' and 'get_metrics_summary.R'

source("habitat_metrics_matrix.R")
source("get_metrics_summary.R")

# create a vector of file names under the wd for processing
files <- c("files/PHR_080619PM_DEM_1cm.tif")
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

metrics_df

rm(list = ls())


###############################################################################

### obtain fractal dimension using 'fractal_dimension.R'

source("fractal_dimension.R")

# create a vector of file names under the wd for processing
files <- c("files/PHR_080619PM_DEM_1cm.tif")

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

rm(list = ls())


###############################################################################

### obtain surface areas of polygons using 'surface_area.R'

source("surface_area.R")

# read a DEM file
ras <- raster('files/PHR_080619PM_DEM_1cm.tif')

# generate raster with 3D surface area
surf_area_ras <- surface_area_raster(ras, res = 0.01)

# read a shapefile
features <- st_zm(st_read('files/PHR_080619PM_shapefiles/PHR_080619PM_digitized.shp'))
# CRS should be 'NA'

# add planar surface area to polygons
features$area_2d <- as.numeric(st_area(features))
# remove if there are empty polygons
features <- features[features$area_2d != 0, ]
# add 3D surface area to polygons
features$area_3d <- exact_extract(surf_area_ras, features, fun='sum')

features

rm(list = ls())
