#' Make gridded daily files split by EPU for heatwave index
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


make_bottom_temp_daily_epu = function(input.file, output.file, shp.file, file.year, write.out =T){
  #Define Season Names  
  
  epu.names =  c('MAB','GB','GOM','SS')
  out.ls = list()
  for(i in 1:length(epu.names)){
    this.data.rast = EDABUtilities::make_2d_summary_gridded(data.in = input.file,
                                                            write.out =F,
                                                            shp.file = shp.file,
                                                            var.name = 'bottomT',
                                                            agg.time = 'days',
                                                            statistic = 'mean',
                                                            touches =F,
                                                            file.time = 'daily',
                                                            area.names = epu.names[i]
    )
    out.epu.ls = list()
    for(j in 1:length(this.data.rast)){
      out.epu.ls[[j]] = terra::as.data.frame(terra::rast(this.data.rast[j][[1]]),xy =T,time = T,wide =F) %>% 
        dplyr::select(-layer) %>% 
        dplyr::group_by(time) %>% 
        dplyr::summarise(BottomT.mean = mean(values,na.rm=T),
                         BottomT.sd = sd(values,na.rm=T)) %>%
        dplyr::rename(date = 'time') %>% 
        dplyr::mutate(EPU = epu.names[i],
                      source = 'GLORYS')
    }
    out.ls[[i]] = dplyr::bind_rows(out.epu.ls)
  }
  
  data.all = dplyr::bind_rows(out.ls)
  
  if(write.out){
    write.csv(data.all, output.file,row.names =F)  
  }else{
    return(data.all)
  }
}