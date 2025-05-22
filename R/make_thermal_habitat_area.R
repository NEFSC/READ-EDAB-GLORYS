#' Createst the thermal_habitat_gridded inidcator for the State of the Ecosystem Report
#'
#' descriptions
#'
#' @param input.file Either a character vector of full input file names for a list of spatrasters
#' @param output.file.area character with full file name for thermal_habitat_area output csv
#' @param output.file.gridded character with full file name for thermal_habitat_gridded output csv
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param file.year numeric. Year of the input file
#' @param write.area logical. If TRUE, the function will write out the area csv file
#' @param write.gridded logical. If TRUE, the function will write out the gridded csv file
#' @param t.max.seq numeric. A vector of temperature thresholds to calculate the number of days above
#' 
#' @return a dataframe (Time, Latitude, Longitude, var, value) or csv file of the gridded data
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 

make_thermal_habitat_area = function(input.file, output.file.area = NA, output.file.gridded =NA, shp.file, file.year,t.max.seq, write.area = F, write.gridded = F){

  EPU.names = c('MAB','GB','GOM','SS','all')
  depth.df = data.frame(depth.min = c(0,25,100, 0),
                        depth.max = c(25,100,300, 2000),
                        depth.name = c('0-25m','25-100m','100-300m', 'AllDepths'))
  
  combs = expand.grid(year = file.year,t.max = t.max.seq,depth.name = depth.df$depth.name,EPU = EPU.names,stringsAsFactors = F)%>%
    dplyr::left_join(depth.df)
  
  bathy.shp = terra::rast(paste0(supp.dir,'GLORYS/GLORYS_bathymetry_east_coast_crop.nc'),subds = 'deptho')
  
  out.area.ls = list()
  out.gridded.ls = list()
  for(i in 1:nrow(combs)){
    
    if(combs$EPU[i] == 'all'){
      area.names = c('MAB','GB','GOM','SS')
    }else{
      area.names = combs$EPU[i]
    }
    
    this.file = input.file
    
    if(i ==1){
      neus.shp = terra::crop(bathy.shp,terra::rast(this.file))
    }
    
    depth.rast = terra::clamp(neus.shp, lower = combs$depth.min[i], upper = combs$depth.max[i],values =F)
    EPU.vect = terra::vect(shp.file)
    area.vect = EPU.vect[which(EPU.vect$EPU %in% area.names)]
    
    area.mask = terra::mask(depth.rast,area.vect)
    
    #Mask of area over t.max
    area.i = EDABUtilities::mask_nc_2d(data.in = this.file,
                                       write.out =F,
                                       shp.file = area.mask,
                                       var.name = 'BottomT',
                                       min.value =  combs$t.max[i],
                                       max.value = Inf,
                                       binary = F,
                                       area.names =NA
    )
    
    this.rast = terra::subset(terra::rast(this.file),1)
    
    nd.i = EDABUtilities::make_2d_deg_day_gridded_nc(data.in = area.i,
                                                     shp.file = area.mask,
                                                     var.name = 'BottomT',
                                                     statistic = 'nd',
                                                     type = 'above',
                                                     ref.value = combs$t.max[i],
                                                     area.names = NA
    )
    
    shp.area = terra::expanse(area.mask)$area
    
    area.df = terra::expanse(area.i[[1]]) %>%
      as.data.frame()%>%
      dplyr::mutate(Time = terra::time(area.i[[1]]),
             EPU = combs$EPU[i],
             Depth = combs$depth.name[i],
             Var = paste0('>',combs$t.max[i],'\u00B0C'),
             Value = area/ shp.area,
             Source = 'GLORYS',
             year = combs$year[i],
             temp.threshold = combs$t.max[i],
             Units = 'Proportion'
      )
    
    out.area.ls[[i]] = area.df
    
    out.gridded.ls[[i]] = as.data.frame(nd.i[[1]],cells =T, xy = T) %>%
      dplyr::mutate(Time = combs$year[i], EPU = combs$EPU[i], Depth = combs$depth.name[i], Var = combs$t.max[i], Source = 'GLORYS',Units = 'Number of Days')%>%
      dplyr::rename(Latitude = 'y', Longitude = 'x', Value = 'sum')%>%
      dplyr::select(Time,EPU, Depth, Var,Value,Latitude,Longitude,Source,Units)
    
    print(signif(i/nrow(combs)*100,2))
  }
  
  out.area.df = dplyr::bind_rows(out.area.ls)%>%
    dplyr::select(Time, EPU, Depth, Var, Value, Source, year, temp.threshold, Units)%>%
    dplyr::mutate(Year = format(as.Date(Time),format = '%Y'))%>%
    dplyr::group_by(Year,EPU, Depth, Var, temp.threshold, Units,Source)%>%
    dplyr::summarise(Value = mean(Value))%>%
    dplyr::rename(Time = Year)
  
    out.gridded.df = dplyr::bind_rows(out.gridded.ls)
  
  if(write.area == T){
    write.csv(out.area.df, output.file.area,row.names = F)
  }
  if(write.gridded == T){
    write.csv(out.gridded.df,output.file.gridded,row.names = F)
  }
  if(write.area == F |write.gridded == F){
    return(list(thermal.area = out.area.df,thermal.gridded = out.gridded.df))
  }
  
}

  