### function to get matrices of habitat metrics
### file = DEM raster, resolution = DEM cell resolution in appropriate unit, default 0.01 m

get_metrics <- function(file, resolution = 0.01) {
  
  require(raster)
  
  ras <- raster(file)
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
    
  hab_data <- list(surf_complex = surf_complex, 
                   slope = terrain_res_1x$slope, 
                   slope2 = terrain_res_1x$slope2, 
                   vrm_1x = terrain_res_1x$vrm, 
                   vrm_2x = terrain_res_2x$vrm, 
                   vrm_4x = terrain_res_4x$vrm, 
                   curvature = terrain_res_1x$curvature, 
                   profile_curvature = terrain_res_1x$profile_curvature, 
                   plan_curvature = terrain_res_1x$plan_curvature)
  
  return(hab_data)
  
}
