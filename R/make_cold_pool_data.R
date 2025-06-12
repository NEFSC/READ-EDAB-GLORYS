#' Createst the thermal_habitat_gridded inidcator for the State of the Ecosystem Report
#'
#' descriptions
#'
#' @param input.files Full input file directory for bottom temp data
#' @param output.dir Full output file directory
#' @param output.prefix string. Prefix for the output files
#' @param cp.shp.file Full input file directory for the cold pool area shape file
#' 
#' @return RDS file with the processed data for GLORYS and ROMS
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 

make_cold_pool_data = function(input.files, output.dir,output.prefix,cp.shp.file){

  #Read cold pool detection area shape file
  cp.shp  = terra::vect(cp.shp.file)
  
  i=1
  data.ls = list()
  data.df.ls = list()
  file.years = numeric()
  for(i in 1:length(input.files)){
    
    input.file = paste0(input.files[i])
    
    #Read in input file
    data.rast = terra::rast(input.file)
    
    file.years[i] = format(terra::time(data.rast),'%Y')[1]
    #Crop input data to cp.shp
    data.crop = data.rast %>%
      terra::crop(cp.shp) %>%
      terra::mask(cp.shp) 
    
    names(data.crop) = terra::time(data.crop)
      
    data.df.ls[[i]] = terra::as.data.frame(data.crop,cell = T)%>%
      tidyr::gather(date,value,-cell)%>%
      dplyr::mutate(date = as.Date(date),
                    month = format(date, "%m"),
                    year = format(date,'%Y'))%>%
      dplyr::group_by(year,month,cell)%>%
      dplyr::summarise(value = mean(value,na.rm=T))
    
    terra::writeCDF(data.crop,varname = 'BottomT',filename =paste0(output.dir,output.prefix,file.years[i],'.nc'),overwrite =T)
  
  }
  
  glorys.df = dplyr::bind_rows(data.df.ls)
  saveRDS(glorys.df,paste0(output.dir,output.prefix,'_monthly_',file.years[1],'_',file.years[length(file.years)],'.rds'))
  
  
}

