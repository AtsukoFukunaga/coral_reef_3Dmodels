### function to calculate fractal dimension
### files = a vector of DEM raster files
### resolution = DEM cell resolution in appropriate unit, default 0.01 m
### max_agg_8 = set maximum aggregation to x8, default FALSE set it to x16

get_fractal <- function(files, resolution = 0.01, max_agg_8 = FALSE) {
  
  require(raster)
  
  fractal <- list()
  
  if (max_agg_8) {
    fac <- 8
  } else {
    fac <- 16
  }
  
  for (k in 1:length(files)) {
    
    ras <- raster(files[k])
    extent <- aggregate(ras, fac = fac, fun = mean, expand = FALSE, na.rm = FALSE)
    extent_uniform <- extent > -Inf
    extent_polygon <- rasterToPolygons(extent_uniform, dissolve = TRUE)
    ras_clip <- mask(crop(ras, extent(extent_polygon)), extent_polygon)
    
    if (max_agg_8) {
      dat <- data.frame(fac = c(1, 2, 4, 8), 
                        area = NA, s_area = NA)
    } else {
      dat <- data.frame(fac = c(1, 2, 4, 8, 16), 
                        area = NA, s_area = NA)
    }
    
    for (i in 1:nrow(dat)) {
      
      if (dat$fac[i] == 1) {
        reef <- ras_clip
      } else {
        reef <- aggregate(ras_clip, fac = dat$fac[i], fun = mean, expand = FALSE, na.rm = FALSE)
      }
      reef_g <- as(reef, "SpatialGridDataFrame")
      reef_g@data[, 1][is.nan(reef_g@data[, 1])] <- NA
      dat$area[i] <- sum(!is.na(reef_g@data)) * (dat$fac[i] * resolution) ^ 2
      dat$s_area[i] <- surfaceArea(reef_g)
    }

    ### fractal dimension
    d <- lm(log(s_area/area) ~ log(fac * resolution), data = dat)
    s_coef <- coef(d)[[2]]
    fd <- 2 - s_coef
    
    fractal[[files[k]]] <- list(fd = fd, area_information = dat)
  
  }
  
  return(fractal)
  
}
