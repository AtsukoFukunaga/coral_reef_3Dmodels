# coral_reef_3Dmodels
## Process DEMs from coral-reef 3D models created using SfM photogrammetry

<br>
These codes process DEMs to obtain 3D surface area and 3D metrics of structural complexity.<br>
DEMs should use local coordinates (e.g., meter) and have no CRS assigned.<br>
<br>
See <b>example.R</b> for how to use these functions.<br>
<br>
<li>Use habitat_metrics_matrix.R and get_metrics.R to obtain matrices of habitat metrics to take a close look at the distributions of metrics per DEM.</li>
<li>Use habitat_metrics_matrix.R and get_metrics_summary.R to process a series of DEM files to obtain mean values and standard deviations of habitat metrics per DEM.</li>
<li>Use fractal_dimension.R to process a series of DEM files to calculate fractal dimension.</li>
<li>Use surface_area.R to add 3D surface area to digitized polygons based on a DEM file.</li>
<br>
<h3>habitat_metrics_matrix.R</h3>
This file contains a function that returns a list containing matrices of slope, VRM, curvature, profile curvature and planform curvature.<br>
It processes a single DEM raster file at a time. The function takes 2 arguments: data = DEM raster file and cell_size = DEM cell resolution, with default resolution of  0.01 (m). Depending on the size of each DEM, the list can be extremely large. There are 2 types of slope metrics, slope based on Horn, 1981 and Walbridge et al., 2018 and slope2 based on Zevenbergen & Thorne, 1987. VRM is based on Sappington et al., 2007.  Curvature metrics are based on Zevenbergen & Thorne, 1987 and Moore et al., 1991.<br>
<h3>get_metrics.R</h3>
This file contains a function to get matrices of habitat metrics using habitat_metrics_matrix.R.<br>
It processes a single DEM raster file at a time. The function takes 2 arguments: file = DEM raster file and resolution = DEM cell resolution, with default resolution of 0.01 (m). The output list can be extremely large.<br>
<h3>get_metrics_summary.R</h3>
This file contains a function to get a summary (mean and standard deviation) of habitat metrics using habitat_metrics_matrix.R.<br>
It can process a series of DEM raster files. The function takes 2 arguments: files = a vector of DEM raster files and resolution = DEM cell resolution, with default resolution of 0.01 (m).<br>
<h3>fractal_dimension.R</h3>
This file contains a function to calculate fractal dimension.<br>
It can process a series of DEM raster files. The function takes 3 arguments: files = a vector of DEM raster files, resolution = DEM cell resolution, with default resolution of 0.01 (m) and max_agg_8 that sets the maximum aggregation to x8. If set to FALSE (default) the maximum aggregation is set to x16.<br>
<h3>surface_area.R</h3>
This file contains a function that takes a DEM and converts it into raster with each cell containing a value of 3D surface area.<br>
The function takes 2 arguments: ras = DEM raster file and res = DEM cell resolution, with default resolution of 0.01 (m). It can be used to calculate 3D surface area of each coral colony after digitizing the corresponding orthophotomosaic that aligns with the DEM.  See Jenness, 2004 for how 3D surface area is calculated.<br>
<h3>volume_estimate.R</h3>
This file contains a function that takes a DEM and converts it into raster with each cell containing a value of volume based on the (2d cell area) x (DEM elevation value of the cell - the minimum elevation value of the DEM).<br>
The function takes 2 arguments: ras = DEM raster file and res = DEM cell resolution, with default resolution of 0.01 (m). It can be used to calculate the volume of each coral colony after digitizing the corresponding orthophotomosaic that aligns with the DEM.<br>
<br>
<h3>References</h3>
<li>Horn, B.K.P. 1981. Hill shading and the reflectance map. Proceedings of the IEEE 69(1):14-47</li>
<li>Jenness, J.S. 2004. Calculating landscape surface area from digital elevation models. Wildlife Society Bulletin 32(3):829–839</li>
<li>Moore, I.D.,Grayson, R.B. and Ladson, A.R. Digital terrain modelling: a review of hydrological, geomorphological, and biological applications. Hydrological Processes 5(1):3-30</li>
<li>Sappington, J.M., Longshore, K.M. and Thompson, D.B. Quantifying landscape ruggedness for animal habitat analysis: a case study using bighorn sheep in the Mojave Desert. The Journal of Wildlife Management 71(5):1419-1426</li>
<li>Walbridge, S., Slocum, N., Pobuda, M. and Wright, D.J. 2018. Unified geomorphological analysis workflows with Benthic Terrain Modeler. Geosciences 8(3):94</li>
<li>Zevenbergen, L.W. and Thorne, C.R. 1987. Quantitative analysis of land surface topography. Earth Surface Processes and Landforms 12(1):47-56</li>
