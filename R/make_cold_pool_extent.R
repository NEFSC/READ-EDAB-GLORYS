#' Creates the maximum extent of the cold pool over the input data
#'
#' descriptions
#'
#' @param input.data data.frame formatted by make_cold_pool_data
#' @param output.dir Full output file directory
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param grid.file Full input file directory for the cold pool grid file
#' 
#' @return RDS file with the processed data for GLORYS and ROMS
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 

#change to full input files instead of glorys/roms


make_cold_pool_extent = function(input.data,output.dir,shp.file,grid.file){
  
  cp.months = 6:9
  
  bt_temp_time_series_month <- input.data%>%
    dplyr::mutate(month = as.numeric(month))
  
  year.min = min(bt_temp_time_series_month$year)
  year.max = max(bt_temp_time_series_month$year)
  
  #Find maximum cold pool extent over all years. Whenever a cell in footprint is <10C from June-September
  cp_max_extent<- bt_temp_time_series_month %>%
    dplyr::filter(year%in%(year.min:year.max) & month%in%cp.months) %>%
    dplyr::group_by(cell)  %>%
    dplyr::summarise(avg_bottom_t=mean(value)) %>%
    as.data.frame() %>%
    dplyr::filter(avg_bottom_t<10) 
  
  cp.grid = terra::rast(grid.file)
  
  cp_max_extent =cbind(cp_max_extent,terra::xyFromCell(cp.grid,cell = cp_max_extent$cell))
  
  saveRDS(cp_max_extent,paste0(output.dir,'cold_pool_maximum_extent_',year.min,'_',year.max,'.rds'))
  # ggplot(cp_max_extent,aes(x=x,y=y,fill= avg_bottom_t))+
  #   geom_tile()
  
  #Calculate annual cold pool extent
  cp_annual_extent = bt_temp_time_series_month %>%
    dplyr::filter(year%in%(year.min:year.max) & month%in%cp.months) %>%
    dplyr::group_by(year,cell)  %>%
    dplyr::summarise(avg_bottom_t=mean(value)) %>%
    as.data.frame() %>%
    dplyr::filter(avg_bottom_t<10) 
  
  cp_annual_extent =cbind(cp_annual_extent,terra::xyFromCell(cp.grid,cell = cp_annual_extent$cell))
  
  saveRDS(cp_max_extent,paste0(output.dir,'cold_pool_annual_extent_',year.min,'_',year.max,'.rds'))
  # ggplot(cp_annual_extent,aes(x=x,y=y,fill= avg_bottom_t))+
  #   geom_tile()+
  #   facet_wrap(~year)

  
}


