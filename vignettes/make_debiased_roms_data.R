#Check if daily EPU ROMS file exists

input.roms = '/home/jcaracappa/EDAB_Datasets/ROMS_NWA/bottom_temp_debiased_roms_reg112_1959_2004.nc'             
output.roms = '/home/jcaracappa/EDAB_Dev/jcaracappa/ROMS_NWA/debiased_ROMS_1959_1992.csv'
shp.file = '/home/jcaracappa/EDAB_Resources/workflow_resources/shapefiles/EPU_NOESTUARIES.shp'
roms.years = 1959:1992
if(!dir.exists(dirname(output.roms))){dir.create(dirname(output.roms),recursive =T)}
if(file.exists(output.roms)){
 print(paste0('File already exists: ',output.roms))
}else{
  
  shp.vect = terra::vect(shp.file)
  roms.rast = terra::rast(input.roms)
  roms.crop = terra::mask(terra::crop(roms.rast, shp.vect),shp.vect)
  roms.names = terra::names(roms.crop)
  
  roms.day = as.numeric(sub('^[^0-9]+([0-9]+).*', '\\1', roms.names))
  roms.year = as.numeric(sub('.*=([0-9]{4})$', '\\1', roms.names))
  roms.date = as.Date(paste(roms.year, roms.day), format = "%Y %j")
  
  date.keep = which(!is.na(roms.date))
  roms.sub = terra::subset(roms.crop,date.keep)
  terra::time(roms.sub) = roms.date[date.keep]
  roms.day = roms.day[date.keep]
  roms.year = roms.year[date.keep]
  roms.date = roms.date[date.keep]
  
  y=i=j = 1
  data.all.ls = list()
  for(y in 1:length(roms.years)){
    which.vars = which(roms.year == roms.years[y])
    
    roms.this.y = terra::subset(roms.sub,which.vars)
    terra::time(roms.this.y) = roms.date[which.vars]
    
    epu.names =  c('MAB','GB','GOM','SS')
    out.ls = list()
    for(i in 1:length(epu.names)){
      this.data.rast = EDABUtilities::make_2d_summary_gridded(data.in = roms.this.y,
                                                              write.out =F,
                                                              shp.file = shp.file,
                                                              var.name = 'bottomT',
                                                              agg.time = 'days',
                                                              statistic = 'mean',
                                                              touches =F,
                                                              file.time = 'annual',
                                                              area.names = epu.names[i]
      )
      out.epu.ls = list()
      for(j in 1:length(this.data.rast)){
        out.epu.ls[[j]] = terra::as.data.frame(terra::rast(this.data.rast[j][[1]]),xy =T,time = T,wide =F) |> 
          dplyr::select(-layer) |> 
          dplyr::group_by(time) |> 
          dplyr::summarise(BottomT.mean = mean(values,na.rm=T),
                           BottomT.sd = sd(values,na.rm=T)) |> 
          dplyr::mutate(EPU = epu.names[i])
      }
      out.ls[[i]] = dplyr::bind_rows(out.epu.ls)
    }
    
    data.all.ls[[y]] = dplyr::bind_rows(out.ls)
    
    
  }
  
  data.all = bind_rows(data.all.ls) |> 
    dplyr::mutate(Source = 'ROMS') |> 
    dplyr::rename(date = 'time')
  sort(unique(format(as.Date(data.all$date),format = '%Y')))
  write.csv(data.all, '/home/jcaracappa/EDAB_Dev/jcaracappa/ROMS_NWA/ROMS_daily_epu_1959_1992.csv', row.names =F)

}
