### function to return a raster layer with surface area
### ras = DEM raster, res = DEM cell resolution in appropriate unit, default 0.01 m

surface_area_raster <- function(ras, res = 0.01) {

  md0 <- as(ras, "matrix")
  
  mda <-  cbind(matrix(NA, nrow = nrow(md0), ncol = 1), 
                rbind(matrix(NA, nrow = 1, ncol = ncol(md0) - 1), 
                      matrix(md0[1 : (nrow(md0) - 1), 1 : (ncol(md0) - 1)], nrow = nrow(md0) - 1, ncol = ncol(md0) - 1)))
  mdb <- rbind(matrix(NA, nrow = 1, ncol = ncol(md0)), 
               matrix(md0[1 : (nrow(md0) - 1), ], nrow = nrow(md0) - 1, ncol = ncol(md0)))
  mdc <- cbind(rbind(matrix(NA, nrow = 1, ncol = ncol(md0) - 1), 
                     matrix(md0[1 : (nrow(md0) - 1), 2 : ncol(md0)], nrow = nrow(md0) - 1, ncol = ncol(md0) - 1)),
               matrix(NA, nrow = nrow(md0), ncol = 1))
  mdd <- cbind(matrix(NA, nrow = nrow(md0), ncol = 1), 
               matrix(md0[, 1 : (ncol(md0) - 1)], nrow = nrow(md0), ncol = ncol(md0) -1))
  mdf <- cbind(matrix(md0[, 2 : ncol(md0)], nrow = nrow(md0), ncol = ncol(md0) - 1), 
               matrix(NA, nrow = nrow(md0), ncol = 1))
  mdg <- cbind(matrix(NA, nrow = nrow(md0), ncol = 1), 
               rbind(matrix(md0[2 : nrow(md0), 1 : (ncol(md0) - 1)], nrow = nrow(md0) - 1, ncol = ncol(md0) - 1), 
                     matrix(NA, nrow = 1, ncol = ncol(md0) - 1)))
  mdh <- rbind(matrix(md0[2 : nrow(md0), ], nrow = nrow(md0) - 1, ncol = ncol(md0)), 
               matrix(NA, nrow = 1, ncol = ncol(md0)))
  mdi <- cbind(rbind(matrix(md0[2 : nrow(md0), 2 : ncol(md0)], nrow = nrow(md0) - 1, ncol = ncol(md0) - 1), 
                     matrix(NA, nrow = 1, ncol = (ncol(md0) - 1))), 
               matrix(NA, nrow = nrow(md0), ncol = 1))
  
  cell_side <- res
  cell_diag <- sqrt(cell_side ^ 2 + cell_side ^ 2)
  
  # calculate height differences
  ab <- abs(mda - mdb)
  bc <- abs(mdb - mdc)
  de <- abs(mdd - md0)
  ef <- abs(md0 - mdf)
  gh <- abs(mdg - mdh)
  hi <- abs(mdh - mdi)
  ad <- abs(mda - mdd)
  be <- abs(mdb - md0)
  cf <- abs(mdc - mdf)
  dg <- abs(mdd - mdg)
  eh <- abs(md0 - mdh)
  fi <- abs(mdf - mdi)
  ea <- abs(md0 - mda)
  ec <- abs(md0 - mdc)
  eg <- abs(md0 - mdg)
  ei <- abs(md0 - mdi)
  
  # calculate lengths
  l_ab <- sqrt(cell_side ^ 2 + ab ^ 2) / 2
  l_bc <- sqrt(cell_side ^ 2 + bc ^ 2) / 2
  l_de <- sqrt(cell_side ^ 2 + de ^ 2) / 2
  l_ef <- sqrt(cell_side ^ 2 + ef ^ 2) / 2
  l_gh <- sqrt(cell_side ^ 2 + gh ^ 2) / 2
  l_hi <- sqrt(cell_side ^ 2 + hi ^ 2) / 2
  l_ad <- sqrt(cell_side ^ 2 + ad ^ 2) / 2
  l_be <- sqrt(cell_side ^ 2 + be ^ 2) / 2
  l_cf <- sqrt(cell_side ^ 2 + cf ^ 2) / 2
  l_dg <- sqrt(cell_side ^ 2 + dg ^ 2) / 2
  l_eh <- sqrt(cell_side ^ 2 + eh ^ 2) / 2
  l_fi <- sqrt(cell_side ^ 2 + fi ^ 2) / 2
  l_ea <- sqrt(cell_diag ^ 2 + ea ^ 2) / 2
  l_ec <- sqrt(cell_diag ^ 2 + ec ^ 2) / 2
  l_eg <- sqrt(cell_diag ^ 2 + eg ^ 2) / 2
  l_ei <- sqrt(cell_diag ^ 2 + ei ^ 2) / 2
  
  # calculate the area of triangles
  t1_s <- (l_ea + l_ab + l_be) / 2; t1 <- sqrt(t1_s * (t1_s - l_ea) * (t1_s - l_ab) * (t1_s - l_be))
  t2_s <- (l_ec + l_bc + l_be) / 2; t2 <- sqrt(t2_s * (t2_s - l_ec) * (t2_s - l_bc) * (t2_s - l_be))
  t3_s <- (l_ea + l_de + l_ad) / 2; t3 <- sqrt(t3_s * (t3_s - l_ea) * (t3_s - l_de) * (t3_s - l_ad))
  t4_s <- (l_ec + l_ef + l_cf) / 2; t4 <- sqrt(t4_s * (t4_s - l_ec) * (t4_s - l_ef) * (t4_s - l_cf))
  t5_s <- (l_eg + l_de + l_dg) / 2; t5 <- sqrt(t5_s * (t5_s - l_eg) * (t5_s - l_de) * (t5_s - l_dg))
  t6_s <- (l_ei + l_ef + l_fi) / 2; t6 <- sqrt(t6_s * (t6_s - l_ei) * (t6_s - l_ef) * (t6_s - l_fi))
  t7_s <- (l_eg + l_gh + l_eh) / 2; t7 <- sqrt(t7_s * (t7_s - l_eg) * (t7_s - l_gh) * (t7_s - l_eh))
  t8_s <- (l_ei + l_hi + l_eh) / 2; t8 <- sqrt(t8_s * (t8_s - l_ei) * (t8_s - l_hi) * (t8_s - l_eh))
  
  # surface area per cell
  surf_area <- t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8
  
  # generate a raster file for surface area
  sur_area_ras <- setValues(ras, surf_area)
  
  return(sur_area_ras)
  
}
