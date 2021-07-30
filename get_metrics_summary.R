### function to get a summary of habitat metrics
### files = a vector of DEM raster files, resolution = DEM cell resolution in appropriate unit, default 0.01 m

get_metrics_summary <- function(files, resolution = 0.01) {
  
  require(raster)
  
  hab_data <- list()
  
  # process raster files
  for (k in 1:length(files)) {
    
    ras <- raster(files[k])
    ras_g <- as(ras, "SpatialGridDataFrame")
    orig_area <- sum(!is.na(ras_g@data)) * resolution ^ 2
    surf_area <- surfaceArea(ras_g)
    surf_complex <- surf_area / orig_area
    
    # aggregate for vrm @ x2 resolution
    ras_2x <- aggregate(ras, fac = 2, fun = mean, expand = FALSE, na.rm = FALSE)
    # aggregate for vrm @ x4 resolution
    ras_4x <- aggregate(ras, fac = 4, fun = mean, expand = FALSE, na.rm = FALSE)
    
    # get metrics at 1x resolution
    terrain_res_1x <- terrain_fun(ras, cell_size = resolution)
    # get metrics at 2x resolution
    terrain_res_2x <- terrain_fun(ras_2x, cell_size = resolution * 2)
    # get metrics at 4x resolution
    terrain_res_4x <- terrain_fun(ras_4x, cell_size = resolution * 4)
    
    dat <- data.frame(fac = c(1, 2, 4), 
                      mean_slope = c(mean(terrain_res_1x$slope, na.rm = TRUE), 
                                     mean(terrain_res_2x$slope, na.rm = TRUE), 
                                     mean(terrain_res_4x$slope, na.rm = TRUE)), 
                      sd_slope = c(sd(terrain_res_1x$slope, na.rm = TRUE), 
                                   sd(terrain_res_2x$slope, na.rm = TRUE), 
                                   sd(terrain_res_4x$slope, na.rm = TRUE)), 
                      mean_slope2 = c(mean(terrain_res_1x$slope2, na.rm = TRUE), 
                                      mean(terrain_res_2x$slope2, na.rm = TRUE), 
                                      mean(terrain_res_4x$slope2, na.rm = TRUE)), 
                      sd_slope2 = c(sd(terrain_res_1x$slope2, na.rm = TRUE), 
                                    sd(terrain_res_2x$slope2, na.rm = TRUE), 
                                    sd(terrain_res_4x$slope2, na.rm = TRUE)), 
                      mean_vrm = c(mean(terrain_res_1x$vrm, na.rm = TRUE), 
                                   mean(terrain_res_2x$vrm, na.rm = TRUE), 
                                   mean(terrain_res_4x$vrm, na.rm = TRUE)), 
                      sd_vrm = c(sd(terrain_res_1x$vrm, na.rm = TRUE), 
                                 sd(terrain_res_2x$vrm, na.rm = TRUE), 
                                 sd(terrain_res_4x$vrm, na.rm = TRUE)), 
                      mean_curvature = c(mean(terrain_res_1x$curvature, na.rm = TRUE), 
                                         mean(terrain_res_2x$curvature, na.rm = TRUE), 
                                         mean(terrain_res_4x$curvature, na.rm = TRUE)),
                      sd_curvature = c(sd(terrain_res_1x$curvature, na.rm = TRUE), 
                                       sd(terrain_res_2x$curvature, na.rm = TRUE), 
                                       sd(terrain_res_4x$curvature, na.rm = TRUE)), 
                      mean_profile_curvature = c(mean(terrain_res_1x$profile_curvature, na.rm = TRUE), 
                                                 mean(terrain_res_2x$profile_curvature, na.rm = TRUE), 
                                                 mean(terrain_res_4x$profile_curvature, na.rm = TRUE)), 
                      sd_profile_curvature = c(sd(terrain_res_1x$profile_curvature, na.rm = TRUE), 
                                               sd(terrain_res_2x$profile_curvature, na.rm = TRUE), 
                                               sd(terrain_res_4x$profile_curvature, na.rm = TRUE)), 
                      mean_plan_curvature = c(mean(terrain_res_1x$plan_curvature, na.rm = TRUE), 
                                              mean(terrain_res_2x$plan_curvature, na.rm = TRUE), 
                                              mean(terrain_res_4x$plan_curvature, na.rm = TRUE)), 
                      sd_plan_curvature = c(sd(terrain_res_1x$plan_curvature, na.rm = TRUE), 
                                            sd(terrain_res_2x$plan_curvature, na.rm = TRUE), 
                                            sd(terrain_res_4x$plan_curvature, na.rm = TRUE)))
    
    hab_data[[files[k]]] <- list(orig_area = orig_area, 
                                 surface_area = surf_area, 
                                 min_height = min(ras_g@data[[1]], na.rm = TRUE),
                                 max_height = max(ras_g@data[[1]], na.rm = TRUE),
                                 surf_complex = surf_complex, 
                                 metrics = dat)
    
  }
  
  return(hab_data)
  
}
