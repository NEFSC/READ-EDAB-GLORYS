#' Createst the bottom_temp_model_gridded (gridded seasonal means) inidcator for the State of the Ecosystem Report
#'
#' descriptions
#'
#' @param input.file Either a character vector of full input file names for a list of spatrasters
#' @param output.file character vector of full output file names corresponding to each input file
#' @param file.year numeric. Year of the input file
#' @param write.out logical. If TRUE, the function will write out the gridded data to a netcdf file
#' 
#' @return a dataframe (Time, Latitude, Longitude, var, value) or csv file of the gridded data
#' 

#' 
#' @export
#' 
#' 
#' 

make_bottom_temp_daily_NEUS = function(input.file,
                                         output.dir, 
                                         output.prefix = NA,
                                         shp.file,
                                         write.out = T){
  #Define Season Names  
  file.dates =  sub(".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", input.file)
  output.files = paste0(output.dir,output.prefix, file.dates,'.nc')
  EDABUtilities::mask_nc_2d(data.in = input.file,
                              write.out = write.out,
                              output.files = output.files,
                            shp.file = shp.file,
                            min.value = -Inf,
                            max.value = Inf,
                            var.name = 'BottomT',
                            area.names = NA)  
  
}
