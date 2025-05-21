#' Createst the bottom_temp_model_gridded (gridded seasonal means) inidcator for the State of the Ecosystem Report
#'
#' descriptions
#'
#' @param input.file Either a character vector of full input file names for a list of spatrasters
#' @param output.file character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param file.year numeric. Year of the input file
#' @param write.out logical. If TRUE, the function will write out the gridded data to a netcdf file
#' 
#' @return a dataframe (Time, Latitude, Longitude, var, value) or csv file of the gridded data
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 
#' 

make_bottom_temp_model_gridded = function(input.file,
                                  output.file = NA,
                                  file.year,
                                  shp.file,
                                  write.out = F
                                  ){
  #Define Season Names
  season.match = data.frame(season.id = 1:4, season.name = c('Winter','Spring','Summer','Fall'))

  #Create seasonal-annual means

  glorys.season = EDABUtilities::make_2d_summary_gridded(data.in = input.file,
                                         write.out = F,
                                         output.files = output.file,
                                         shp.file = shp.file,
                                         var.name = 'BottomT',
                                         agg.time = 'season',
                                         statistic = 'mean',
                                         area.names = c('MAB','GB','GOM','SS')
  )[[1]]
  
  glorys.season.mean =terra::as.data.frame(terra::rast(glorys.season),xy =T)
  colnames(glorys.season.mean) = c('Longitude','Latitude','winter','spring','summer','fall')[1:ncol(glorys.season.mean)]
  glorys.season.mean = glorys.season.mean %>%
      tidyr::gather(Var,Value,-Longitude,-Latitude)%>%
      dplyr::mutate(Time = file.year )%>%
      dplyr::select(Time,Latitude,Longitude,Var,Value)
  
  
  if(write.out){
    write.csv(glorys.season.mean,output.file,row.names = F)
  }else{
    return(glorys.season.mean)
  }
}

# make_bottom_temp_model_gridded(input.file = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/GLORYS_daily_BottomTemp_2024.nc',
#                                output.file = here::here('data','GLORYS','bottom_temp_test.nc'),
#                                shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
#                                file.year = 2024)
