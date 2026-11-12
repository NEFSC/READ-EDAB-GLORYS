#' Formats data for cold pool analysis
#'
#' descriptions
#'
#' @param input.files Full input file directory for bottom temp data
#' @param output.dir Full output file directory
#' @param output.prefix string. Prefix for the output files
#' @param file.time character. Either 'daily' or 'annual' layers in file
#' @param cp.shp.file Full input file directory for the cold pool area shape file
#' @param write.out if TRUE writes to file
#' 
#' @return RDS file with the processed data for GLORYS and ROMS
#' 
#' @export
#' 
make_cold_pool_data = function(input.files, file.time = 'daily',output.dir,output.prefix,cp.shp.file,redo =F,write.out = F){

  #Read cold pool detection area shape file
  cp.shp  = terra::vect(cp.shp.file)
  
  # data.crop = EDABUtilities::mask_nc_2d(data.in = input.files,shp.file = cp.shp,var.name = 'bottomT',write.out = F,area.names = NA,min.value = -Inf, max.value = Inf,binary =F)
  
  data.daily = EDABUtilities::make_2d_summary_gridded(data.in = input.files,
                                                      write.out = F,
                                                      output.files = NA,
                                                      shp.file = cp.shp,
                                                      file.time = 'daily',
                                                      var.name =  'bottomT',
                                                      agg.time = 'days',
                                                      statistics = 'mean',
                                                      area.names = NA)
  data.rast = terra::rast(data.daily[[1]])
  
  file.years = format(terra::time(data.rast),format = "%Y")[1]
    
  names(data.rast) = terra::time(data.rast)
      
  data.df = terra::as.data.frame(data.rast,cell = T)|>
      tidyr::gather(date,value,-cell)|>
      dplyr::mutate(date = as.Date(date),
                    month = format(date, "%m"),
                    year = format(date,'%Y'))|>
      dplyr::group_by(year,month,cell)|>
      dplyr::summarise(value = mean(value,na.rm=T))
    
  if(write.out){
    terra::writeCDF(data.rast,varname = 'bottomT',filename =paste0(output.dir,output.prefix,file.years,'.nc'),overwrite =T)  
    saveRDS(data.df,paste0(output.dir,output.prefix,'monthly_',file.years,'.rds'))  
  }else{
    out.ls = list(data.frame = data.df,
                  data.nc = data.rast
                  )
    return(out.ls)
  }
    
}

