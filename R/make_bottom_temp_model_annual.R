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
#' 


make_bottom_temp_model_annual = function(input.file,
                                        output.file = NA,
                                        file.year,
                                        shp.file,
                                        climatology.file,
                                        write.out = F){
    #Define Season Names  
  glorys.epu.day = EDABUtilities::make_2d_summary_ts(data.in = input.file,
                                                     write.out =F,
                                                     shp.file = shp.file,
                                                     var.name = 'bottomT',
                                                     agg.time = 'days',
                                                     statistic = 'mean',
                                                     touches =F,
                                                     file.time = 'annual',
                                                     area.names = c('MAB','GB','GOM','SS')
  )
  
  data.all.mean = glorys.epu.day[[1]] %>%
    dplyr::mutate(year = format(as.Date(time),format = '%Y'))%>%
    dplyr::group_by(year,area)%>%
    dplyr::summarise(Value = mean(value,na.rm=T))%>%
    dplyr::mutate(source = 'GLORYS') %>%
    dplyr::rename(Time = 'year',
           EPU = 'area',
           Source = 'source')%>%
    dplyr::mutate(Var = 'Annual_Bottom Temp',
           Units = 'degree C',
           Time = as.numeric(Time))%>%
    dplyr::select(Time,Value,EPU,Source,Var,Units)
  
  if(write.out){
    write.csv(data.all.mean, output.file,row.names =F)  
  }else{
    return(data.all.mean)
  }
}
  
# make_bottom_temp_annual(input.file = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/GLORYS_daily_BottomTemp_2024.nc',
#                         output.file = here::here('data','GLORYS','bottom_temp_test.nc'),
#                         shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
#                         file.year = 2024,
#                         climatology.file =  here::here('data','GLORYS','GLORYS_bottom_temp_clim_1990_2020.csv')
# )
