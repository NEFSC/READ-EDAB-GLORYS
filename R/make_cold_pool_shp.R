#' Creates the shape file for the maximum cold pool extent
#'
#' descriptions
#'
#' @param bathy.file Full input file directory for bathymetry data
#' @param shp.file Full input file directory for the shape file to crop
#' @param output.file Full output file directory for the cold pool area shape file
#' 
#' @return .shp with cold pool detection area
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 

make_cold_pool_shp = function(bathy.file, shp.file, output.file){
  
  #GLORYS Bathymetry for NEUS
  bathy =  terra::rast(bathy.file,subds = 'deptho')
  bathy.crs = terra::crs(bathy)
  
  #Create Cold Pool area from the Southern New England Yellowtail stock area
  cp.strata = c(1690,1730,1740,1010,1020,1050,1060,1090,1100,1700,1650,1610,1660,1620,1030,1070,1110)
  cp.shp =terra::vect(shp.file)
  cp.shp = cp.shp[which(cp.shp$STRATA %in% cp.strata)] %>% 
    terra::aggregate()%>%
    terra::project(bathy.crs)
  cp.shp = terra::clamp(bathy,lower = 20, upper=200,values = F) %>%
    terra::mask(cp.shp)%>%
    terra::crop(cp.shp)
  #convert cp.shp to a spatVector
  cp.shp = terra::as.polygons(cp.shp,dissolve = T)%>%terra::aggregate()
  #write cp.shp as a shape file
  terra::writeVector(cp.shp,output.file,overwrite = T)

}

# make_cold_pool_shp(bathy.file = here::here('data-raw','GLORYS','GLORYS_bathymetry_east_coast_crop.nc'),
#                    shp.file = here::here('data-raw','geometry','NES_BOTTOM_TRAWL_STRATA.shp'),
#                    output.file = here::here('data-raw','geometry','cold_pool_area_GLORYS.shp'))