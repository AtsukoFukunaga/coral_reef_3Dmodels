### function to return a raster layer with volume estimate
### ras = DEM raster, res = DEM cell resolution in appropriate unit

volume_estimate_raster <- function(ras, res) {
  
  md0 <- as(ras, "matrix")
  md0_adj <- md0 - min(md0, na.rm = TRUE)
  vol <- md0_adj * res * res
  
  vol_ras <- setValues(ras, vol)
  
  return(vol_ras)
  
}
