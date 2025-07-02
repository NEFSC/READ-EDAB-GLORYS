#' Creates the cold pool extent shape file and metrics
#'
#' descriptions
#'
#' @param input.files Full input file directory for bottom temp data returned from make_cold_pool_data
#' @param cp.extent.file Full input file directory for cold pool extent data returned from make_cold_pool_extent
#' @param output.dir Full output file directory
#' 
#' @return csv with cold pool indices
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 

make_cold_pool_extent_shp = function(input.files, output.dir){

  cp.months = 6:9
  #needs to retrun .shp for each year with max annual cold pool extent
  #plots for monthly cold pool extent?
  #return area of cold pool by month/year
  cp.area.ls = list()
  
  for(i in 1:length(input.files)){
    
    input.file = input.files[i]
    
    #Read in input file
    data.rast = terra::rast(input.file)
    
    data.time = terra::time(data.rast)
    data.year = format(data.time, '%Y')
    data.month = as.numeric(format(data.time, '%m'))
    
    which.years = sort(unique(data.year))
    
    cp.area = data.frame(year = which.years,area = NA)
    for(y in 1:length(which.years)){
      
      which.dates =which(which.years[y] == data.year & data.month %in% cp.months)
      this.year = terra::subset(data.rast,which.dates)
      this.year.date = terra::time(this.year)
      
      this.year.mean = terra::mean(this.year,na.rm=T)
      this.year.cp = terra::clamp(this.year.mean, lower = -Inf, upper = 10, values = F)
      # terra::plot(this.year.cp)
      this.year.cp.shp = terra::as.polygons(this.year.cp, dissolve = T) %>% terra::aggregate()
      # terra::plot(this.year.cp.shp)
      
      # this.year.month = terra::tapp(this.year,fun = 'mean',index = 'months')
      # this.year.cp = terra::clamp(this.year.month,lower = -Inf,upper = 10,values =F)
      # plot(this.year.cp)
      # this.year.vect = lapply(1:terra::nlyr(this.year.cp), function(x) terra::as.polygons(terra::subset(this.year.cp,x), dissolve = T))
      # for(i in 1:length(this.year.vect)) {plot(this.year.vect[[i]], col = 'black')}
      
      terra::writeVector(this.year.cp.shp,
                         filename = paste0(output.dir,'cold_pool_extent_',which.years[y],'.shp'),
                        filetype = 'ESRI Shapefile',
                        overwrite = TRUE)
      
      cp.area$area[y] = terra::expanse(this.year.cp.shp,unit = 'km') #area in km^2
    }

    cp.area.ls[[i]] = cp.area 
  }
  
  cp.area.df = dplyr::bind_rows(cp.area.ls) %>%
    dplyr::mutate(source = ifelse(grepl('GLORYS',input.files[1]),'GLORYS','ROMS')) %>%
    dplyr::select(year,source,area)
  
  saveRDS(cp.area.df,paste0(output.dir,'cold_pool_extent_.rds'))
}

make_cold_pool_extent_shp(input.files = c(here::here('data','cold_pool','ROMS','roms_debiased_cold_pool_1959_1992.nc'),
                                          list.files(here::here('data','cold_pool','GLORYS'),'*.nc',full.names = TRUE)),
                          output.dir = here::here('data','cold_pool','extent','/'))

                