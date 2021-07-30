### terrain function returns slope, vrm, curvature
### data = DEM raster, cell_size = DEM cell resolution in appropriate unit, default 0.01 m

terrain_fun <- function(data, cell_size = 0.01) {
  
  nbrcell <- function(d0) {
    
    da <-  cbind(matrix(NA, nrow = nrow(d0), ncol = 1), 
                 rbind(matrix(NA, nrow = 1, ncol = ncol(d0) - 1), 
                       matrix(d0[1 : (nrow(d0) - 1), 1 : (ncol(d0) - 1)], 
                              nrow = nrow(d0) - 1, ncol = ncol(d0) - 1)))
    db <- rbind(matrix(NA, nrow = 1, ncol = ncol(d0)), 
                matrix(d0[1 : (nrow(d0) - 1), ], 
                       nrow = nrow(d0) - 1, ncol = ncol(d0)))
    dc <- cbind(rbind(matrix(NA, nrow = 1, ncol = ncol(d0) - 1), 
                      matrix(d0[1 : (nrow(d0) - 1), 2 : ncol(d0)], 
                             nrow = nrow(d0) - 1, ncol = ncol(d0) - 1)),
                matrix(NA, nrow = nrow(d0), ncol = 1))
    dd <- cbind(matrix(NA, nrow = nrow(d0), ncol = 1), 
                matrix(d0[, 1 : (ncol(d0) - 1)], nrow = nrow(d0), ncol = ncol(d0) -1))
    df <- cbind(matrix(d0[, 2 : ncol(d0)], nrow = nrow(d0), ncol = ncol(d0) - 1), 
                matrix(NA, nrow = nrow(d0), ncol = 1))
    dg <- cbind(matrix(NA, nrow = nrow(d0), ncol = 1), 
                rbind(matrix(d0[2 : nrow(d0), 1 : (ncol(d0) - 1)], 
                             nrow = nrow(d0) - 1, ncol = ncol(d0) - 1), 
                      matrix(NA, nrow = 1, ncol = ncol(d0) - 1)))
    dh <- rbind(matrix(d0[2 : nrow(d0), ], nrow = nrow(d0) - 1, ncol = ncol(d0)), 
                matrix(NA, nrow = 1, ncol = ncol(d0)))
    di <- cbind(rbind(matrix(d0[2 : nrow(d0), 2 : ncol(d0)], 
                             nrow = nrow(d0) - 1, ncol = ncol(d0) - 1), 
                      matrix(NA, nrow = 1, ncol = (ncol(d0) - 1))), 
                matrix(NA, nrow = nrow(d0), ncol = 1))
    
    return(list(a = da, b = db, c = dc, d = dd, f = df, g = dg, h = dh, i = di))
    
  }
  
  d0 <- as.matrix(data)
  if (ncol(d0) == 1 | nrow(d0) == 1) {
    terrain_list <- list(matrix(NA), matrix(NA), matrix(NA), matrix(NA), matrix(NA), matrix(NA))
    names(terrain_list) <- c("slope", "slope2", "vrm",  
                             "curvature", "profile_curvature", "plan_curvature")
  } else {
    nbr <- nbrcell(d0)
    da <- nbr$a
    db <- nbr$b
    dc <- nbr$c
    dd <- nbr$d
    df <- nbr$f
    dg <- nbr$g
    dh <- nbr$h
    di <- nbr$i
    
    ## slope (based on Horn 1981, Walbridge et al 2018)
    
    x_rate <- ((dc + (2 * df) + di) - (da + (2 * dd) + dg)) / (8 * cell_size)
    y_rate <- ((dg + (2 * dh) + di) - (da + (2 * db) + dc)) / (8 * cell_size)
    
    slope_rad <- atan(sqrt(x_rate ^ 2 + y_rate ^ 2))
    slope_deg <- slope_rad * 57.29578
    
    ## aspect (see ArcMap Spatial Analyst reference)
    
    x_rate_aspect <- ((dc + (2 * df) + di) - (da + (2 * dd) + dg)) / 8
    y_rate_aspect <- ((dg + (2 * dh) + di) - (da + (2 * db) + dc)) / 8
    
    aspect_rad <- atan2(y_rate_aspect, (-1 * x_rate_aspect))
    
    ## VRM (based on Sappington et al 2007)
    
    xy_raster_calc <- sin(slope_rad)
    z_raster_calc <- cos(slope_rad)
    x_raster_calc <- sin(aspect_rad) * xy_raster_calc
    y_raster_calc <- cos(aspect_rad) * xy_raster_calc
    
    x_nbr <- nbrcell(x_raster_calc)
    y_nbr <- nbrcell(y_raster_calc)
    z_nbr <- nbrcell(z_raster_calc)
    
    x_sum_calc <- x_nbr$a + x_nbr$b + x_nbr$c + x_nbr$d + x_raster_calc + x_nbr$f + x_nbr$g + x_nbr$h + x_nbr$i
    y_sum_calc <- y_nbr$a + y_nbr$b + y_nbr$c + y_nbr$d + y_raster_calc + y_nbr$f + y_nbr$g + y_nbr$h + y_nbr$i
    z_sum_calc <- z_nbr$a + z_nbr$b + z_nbr$c + z_nbr$d + z_raster_calc + z_nbr$f + z_nbr$g + z_nbr$h + z_nbr$i
    
    result_vect <- sqrt(x_sum_calc ^ 2 + y_sum_calc ^ 2 + z_sum_calc ^ 2)
    
    vrm <- 1 - (result_vect / 9)

    ## slope 2 (based on Zevenbergen & Thorne 1987)
    
    coefD <- (((dd + df) / 2) - d0) / (cell_size ^ 2)
    coefE <- (((db + dh) / 2) - d0) / (cell_size ^ 2)
    coefF <- (dc + dg -da - di) / (4 * (cell_size ^ 2))
    coefG <- (df - dd) / (2 * cell_size)
    coefH <- (db - dh) / (2 * cell_size)
    
    slope2 <- sqrt(coefG ^ 2 + coefH ^ 2)
    
    ## curvature (based on Zevenbergen & Thorne 1987, Moore et al 1991)
    
    curvature <- 2 * (coefD + coefE)
    prof_curv <- -2 * ((coefD * coefG ^ 2 + coefE * coefH ^ 2 + coefF * coefG * coefH) / 
                         (coefG ^ 2 + coefH ^ 2))
    plan_curv <- 2 * ((coefD * coefH ^ 2 + coefE * coefG ^ 2 - coefF * coefG * coefH) / 
                        (coefG ^ 2 + coefH ^ 2))

    ## output list
    
    terrain_list <- list(slope_deg, slope2, vrm, 
                         curvature, prof_curv, plan_curv)
    names(terrain_list) <- c("slope", "slope2", "vrm",  
                             "curvature", "profile_curvature", "plan_curvature")
    
  }
  
  return(terrain_list)
  
}
