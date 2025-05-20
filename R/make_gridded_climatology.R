#' Creates a climatology 
#'
#' descriptions
#'
#' @param input.dir Either a character vector of full input file names for a list of spatrasters
#' @param input.prefix string. Prefix for the input file names
#' @param output.file character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param ref.year.start numeric. Year that climatological period starts
#' @param ref.year.end numeric. Year that climatological period ends
#' @param agg.time string. Aggregation level. Options are 'season' or 'annual'
#' @param var.name string. Name of the variable in the input file
#' @param write.out logical. If TRUE, the function will write out the gridded data to a netcdf file
#' 
#' @return a dataframe (Time, Latitude, Longitude, var, value) or csv file of the gridded data
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 
#' 



make_gridded_climatology = function(input.dir,
                                input.prefix,
                                output.file = NA,
                                shp.file,
                                ref.year.start,
                                ref.year.end,
                                agg.time,
                                var.name,
                                write.out = F
){
  
  data.files = list.files(input.dir,
                          pattern = paste0(input.prefix,'*'),
                          full.names = T)
  
  #Retreive years from file names
  glorys.years = as.numeric(gsub(".*(\\d{4}).*", '\\1', data.files))
  
  #Seasonal means by epu for all reference years
  glorys.season.epu = EDABUtilities::make_2d_summary_ts(data.in = data.files,
                                                        write.out =F,
                                                        file.time = 'annual',
                                                        shp.file =shp.file,
                                                        var.name = var.name,
                                                        agg.time = agg.time,
                                                        statistic = 'mean',
                                                        touches =F,
                                                        area.names = c('MAB','GB','GOM','SS')
  )
  
  for(i in 1:length(glorys.years)){glorys.season.epu[[i]] = dplyr::mutate(glorys.season.epu[[i]], year = glorys.years[i], Var = 'GLORYS')}
  
  data.all.anom.clim = dplyr::bind_rows(glorys.season.epu) %>%
    dplyr::filter(year %in% seq(ref.year.start,ref.year.end)) %>%
    dplyr::group_by(time,area)%>%
    dplyr::summarise(value.clim = mean(value,na.rm=T))
  
  if(write.out){
    write.csv(data.all.anom.clim,output.file,row.names = F)
  }else{
    return(data.all.anom.clim)
  }
}

# make_gridded_climatology(input.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/',
#                      input.prefix = 'GLORYS_daily_BottomTemp_',
#                      output.file = here::here('data-raw','GLORYS_bottom_temp_clim_1990_2020.csv'),
#                      shp.file = here::here('data-raw','geometry','EPU_NOESTUARIES.shp'),
#                      write.out = T,
#                      ref.year.start = 1990,
#                      ref.year.end = 2020,
#                      agg.time = 'season',
#                      var.name = 'bottomT')
