#' Createst the thermal_habitat_gridded inidcator for the State of the Ecosystem Report
#'
#' descriptions
#'
#' @param input.file Either a character vector of full input file names for a list of spatrasters
#' @param output.file character vector of full output file names corresponding to each input file
#' @param supp.dir character vector of the directory where the input files are located
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param file.year numeric. Year of the input file
#' @param write.out logical. If TRUE, the function will write out the gridded data to a netcdf file
#' @param t.max.seq numeric. A vector of temperature thresholds to calculate the number of days above
#' 
#' @return a dataframe (Time, Latitude, Longitude, var, value) or csv file of the gridded data
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 

make_thermal_habitat_gridded = function(input.file, output.file,supp.dir, shp.file, file.year,t.max.seq, write.out =F){
  
  EPU.names = c('MAB','GB','GOM','SS')
  depth.df = data.frame(id = 1:4,
                        depth.min = c(0,25,100,300),
                        depth.max = c(25,100,300,Inf),
                        depth.name = c('0-25m','25-100m','100-300m','300+'))
  
    combs = expand.grid(depth.name = depth.df$depth.name,EPU = EPU.names,stringsAsFactors = F)%>%
    dplyr::left_join(depth.df) 
 
  bathy.shp = terra::rast(paste0(supp.dir,'GLORYS/GLORYS_bathymetry_east_coast_crop.nc'),subds = 'deptho')

  #create EPU mask layer
  EPU.vect = terra::vect(shp.file)
  neus.shp = terra::crop(bathy.shp,bathy.shp)

  epu.ls = lapply(1:length(EPU.names), function(x){
    e.vect = EPU.vect[which(EPU.vect$EPU %in% EPU.names[x])]
    e = terra::mask(neus.shp,e.vect)
    terra::values(e)[!is.na(terra::values(e))] = x
    return(e)
  })
  epu.mask = terra::merge(terra::sprc(epu.ls))
  # epu.mask = as.factor(epu.mask)
  levels(epu.mask) = data.frame(1:4, EPU.names)
  epu.mask.binary = epu.mask
  terra::values(epu.mask.binary)[!is.na(terra::values(epu.mask.binary))] = 1
  
  i=1
  y=32
  
  t=i=1
  out.ind =1
  out.var.names = character()
  year.out.ls = list()
  out.df.ls = list()
  out.ls = list()
  depth.rast.ls = list()
  epu.rast.ls = list()
  
  for(t in 1:length(t.max.seq)){
    
    #Mask of area over t.max
    area.i = EDABUtilities::mask_nc_2d(data.in = input.file,
                                       write.out =F,
                                       shp.file = EPU.vect,
                                       var.name = 'BottomT',
                                       min.value =  t.max.seq[t],
                                       max.value = Inf,
                                       binary = F,
                                       area.names =NA
    )
    
    out.ls[[t]] = EDABUtilities::make_2d_deg_day_gridded_nc(data.in = area.i,
                                                            shp.file = EPU.vect,
                                                            var.name = 'BottomT',
                                                            statistic = 'nd',
                                                            type = 'above',
                                                            ref.value = t.max.seq[t],
                                                            area.names = NA
    )[[1]]
    
    out.ls[[t]] = out.ls[[t]] *terra::crop(epu.mask.binary, out.ls[[t]])
    
    year.time = as.POSIXct(paste0(file.year, '-01-01 00:00:00UTC'),origin = '1970-01-01 00:00:00',tz = 'UTC')
    terra::time(out.ls[[t]]) = as.numeric(year.time)
    
    print(signif(t/length(t.max.seq) ,2))
    
    out.var.names[t] = paste0('Number of Days per Year Above ',t.max.seq[t],' degrees C')
    
  }
  
  out.sds =terra::sds(out.ls)
  
  out.var = out.sds
  
  #concatenate names
  out.names = c(paste0('nday_',t.max.seq))
  out.names = gsub('\\.','_',out.names)
  names(out.var) = out.names
  terra::longnames(out.var) = out.var.names
  terra::units(out.var) = c(rep('n days',length(t.max.seq)))
  
  terra::writeCDF(out.var,filename = output.file,overwrite =T,missval = 0)
  
  #Format netcdf
  var.atts = read.csv(paste0(supp.dir,'GLORYS/thermal_habitat_gridded_variable_attributes.csv')) %>%
    filter(!is.na(Value) & Attribute.Name != '_FillValue')
  global.atts = read.csv(paste0(supp.dir,'GLORYS/thermal_habitat_gridded_global_attributes.csv'))%>%
    filter(!is.na(Value))
  
  file.nc = ncdf4::nc_open(output.file,write =T)
  file.var = names(file.nc$var)
  
  for(g in 1:nrow(global.atts)){
    
    ncdf4::ncatt_put(file.nc,0,global.atts$Attribute.Name[g],global.atts$Value[g])
    
  }
  ncdf4::ncatt_put(file.nc,0,'time_coverage_start',paste0(file.year,'-01-01'))
  ncdf4::ncatt_put(file.nc,0,'time_coverage_end',paste0(file.year,'-12-31'))
  ncdf4::ncatt_put(file.nc,'time','units','seconds since 1970-01-01T00:00:00Z')
  ncdf4::ncatt_put(file.nc,'latitude','standard_name','latitude')
  ncdf4::ncatt_put(file.nc,'latitude','coverage_content_type','coordinates')
  
  ncdf4::ncatt_put(file.nc,'longitude','standard_name','longitude')
  ncdf4::ncatt_put(file.nc,'longitude','coverage_content_type','coordinates')
  
  for(v in 1:length(file.var)){
    
    
    ncdf4::ncatt_put(file.nc,file.var[v],'standard_name','number_of_days_with_bottom_temperature_above_threshold',prec ='text' )
    
    for(va in 1:nrow(var.atts)){
      
      ncdf4::ncatt_put(file.nc,file.var[v],var.atts$Attribute.Name[va],var.atts$Value[va],prec ='text' )
      
    }
    
  }
  
  ncdf4::nc_close(file.nc)
  
  ##Write into test
  # file.names = list.files(here::here('data','SOE','thermal_habitat_gridded_V2'),'thermal_', full.names = T)
  # test.nc = ncdf4::nc_open(file.names[32])
  # # test.nc = ncdf4::nc_open('C:/Users/joseph.caracappa/Downloads/thermal_habitat_gridded_1993_SOE2025.nc')
  # ncdf4::ncatt_get(test.nc,'nday_10')
  # ncdf4::nc_close(test.nc)
  # x = terra::rast(file.names[1])
  # plot(x)
  # test.sds = terra::sds(file.names[1],1)
  # writeCDF(test.sds,here::here('data','SOE','thermal_habitat_checker.nc'))
  
}
