#' Createst the bottom_temp_model_gridded (gridded seasonal means) inidcator for the State of the Ecosystem Report
#'
#' descriptions
#'
#' @param input.file Either a character vector of full input file names for a list of spatrasters
#' @param climatology.file string. File name for climatology file created by make_soe_climatology()
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

make_bottom_temp_model_anom = function(input.file,
                                       output.file = NA,
                                       file.year,
                                       shp.file,
                                       climatology.file,
                                       write.out = F){
  
  #Define Season Names
  season.match = data.frame(season.id = 1:4, season.name = c('Winter','Spring','Summer','Fall'))
  
  # Create Seasonal Bottom Temp Anomalies
  ##GLORYS
  glorys.season.epu = EDABUtilities::make_2d_summary_ts(data.in = input.file,
                                                        write.out =F,
                                                        shp.file = shp.file,
                                                        var.name = 'bottomT',
                                                        agg.time = 'season',
                                                        statistic = 'mean',
                                                        touches =F,
                                                        file.time = 'annual',
                                                        area.names = c('MAB','GB','GOM','SS')
  )
  
  glorys.season.epu = mutate(glorys.season.epu[[1]], year = file.year, Var = 'GLORYS')
  
  glorys.anom.clim = read.csv(climatology.file)

  data.all.anom = glorys.season.epu %>%
    left_join(glorys.anom.clim)%>%
    rename(Source = 'Var')%>%
    mutate(value.anom = value-value.clim)%>%
    left_join(season.match,by = c('time'= 'season.id')) %>%
    mutate(dum = '_Bottom Temp Anomaly')%>%
    tidyr::unite(Var,c('season.name','dum'),sep='')%>%
    rename(Time = 'year',
           EPU = 'area',
           Value = 'value.anom')%>%
    mutate(Units = 'degree C')%>%
    select(Time, Value, EPU, Source, Var, Units)
  
  if(write.out){
    write.csv(data.all.anom,output.file,row.names = F)
  }else{
    return(data.all.anom)
  }
}

# make_bottom_temp_model_anom(input.file = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/GLORYS_daily_BottomTemp_2024.nc',
#                             output.file = here::here('data','GLORYS','bottom_temp_test.nc'),
#                             shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
#                             file.year = 2024,
#                             climatology.file =  here::here('data','GLORYS','GLORYS_bottom_temp_clim_1990_2020.csv')
# )