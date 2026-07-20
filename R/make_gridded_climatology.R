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
#' @importFrom magrittr "|>"
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
  data.files.sub = data.files[which(glorys.years>=ref.year.start & glorys.years<=ref.year.end)]
  
  #Seasonal means by epu for all reference years
  # unique.years = ref.year.start:ref.year.end
  # for(i in 1:length(unique.years)){
  #   this.year.files =  data.files[which(glorys.years == unique.years[i])]
  #   if(length(this.year.files)==0){
  #     next()
  #   }
  #   tic()
  glorys.season.epu = EDABUtilities::make_2d_summary_ts(data.in = data.files.sub,
                                                          write.out =F,
                                                          file.time = 'daily',
                                                          shp.file =shp.file,
                                                          var.name = var.name,
                                                          agg.time = agg.time,
                                                          statistic = 'mean',
                                                          touches =F,
                                                          area.names = c('MAB','GB','GOM','SS')
    )
  #   toc()
  #   print(unique.years[i])
  # }

  glorys.season.epu.all = dplyr::bind_rows(glorys.season.epu) |> 
    dplyr::rename(year = 'ls.id') |> 
    dplyr::mutate(Var = 'GLORYS')
  # for(i in 1:length(glorys.years)){glorys.season.epu[[i]] = dplyr::mutate(glorys.season.epu[[i]], year = glorys.years[i], Var = 'GLORYS')}
  
  data.all.anom.clim =glorys.season.epu.all |>
    dplyr::filter(year %in% seq(ref.year.start,ref.year.end)) |>
    dplyr::group_by(time,area)|>
    dplyr::summarise(value.clim = mean(value,na.rm=T))
  
  if(write.out){
    write.csv(data.all.anom.clim,output.file,row.names = F)
  }else{
    return(data.all.anom.clim)
  }
}


