
#Gets arguments from command line
args = commandArgs(trailingOnly = TRUE)
if(length(args)>0){
  print(args)
  input.dir= args[1]
  supp.dir = args[2]
  output.dir = args[3]
  print('Using command line arguments')
}else{
  # input.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_dailAy/'
  input.dir = '/home/jcaracappa/EDAB_Datasets/GLORYS/glorys_bottomT/cmems_mod_glo_phy_my_0.083deg_P1D-m/bottomT_NEUS/'
  # output.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/READ_EDAB_GLORYS/'
  output.dir = '/home/jcaracappa/EDAB_Dev/jcaracappa/glorys_my_soe/'
  # supp.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/READ_EDAB_GLORYS/data-raw/'
  supp.dir = '/home/jcaracappa/EDAB_Resources/workflow_resources/'
  
  print('Using default arguments')
}
shp.file = paste0(supp.dir,'shapefiles/EPU_NOESTUARIES.shp')

#Get year range
year.start = 1993
year.end = format(Sys.time(), '%Y')
run.years = year.start:year.end
y=1
input.prefix = 'GLORYS_REANALYSIS_DAILY_cmems_mod_glo_phy_my_0.083deg_P1D-m_bottomT_NEUS_'
input.files = list.files(input.dir,input.prefix)
input.files.year = as.numeric(gsub(".*(\\d{4}).*", '\\1', input.files))
no.threshold =F

check.dir = function(file){
  if(!dir.exists(dirname(file))){dir.create(dirname(file),recursive =T)}
}
#Make seasonal climatology
output.file1 =  '/home/jcaracappa/EDAB_Dev/jcaracappa/glorys_soe/data/climatology/GLORYS_bottom_temp_seasonal_clim_1990_2020.csv'
check.dir(output.file1)
if(!file.exists(output.file1)){
  EDAB.GLORYS::make_gridded_climatology(input.dir = input.dir,
                                        input.prefix = input.prefix,
                                        output.file = output.file1,
                                        shp.file = shp.file,
                                        write.out = T,
                                        ref.year.start = 1990,
                                        ref.year.end = 2020,
                                        agg.time = 'season',
                                        var.name = 'bottomT')
  
}else{
  print('Seasonal climatology already exists, skipping creation')
}

#Make Annual Climatology
output.file2 = '/home/jcaracappa/EDAB_Dev/jcaracappa/glorys_soe/data/climatology/GLORYS_bottom_temp_annual_clim_1990_2020.csv'
check.dir(output.file2)
if(!file.exists(output.file2)){
  EDAB.GLORYS::make_gridded_climatology(input.dir =input.dir,
                                        input.prefix = input.prefix,
                                        output.file = output.file2,
                                        shp.file = shp.file,
                                        write.out = T,
                                        ref.year.start = 1990,
                                        ref.year.end = 2020,
                                        agg.time = 'years',
                                        var.name = 'bottomT')
}else{
  print('Annual climatology already exists, skipping creation')
}

#Produce each year's indicies
for(y in 1:length(run.years)){
  
  which.files.year = which(input.files.year == run.years[y])
  if(length(which.files.year) == 0){
    next()
  }
  
  this.year.files = paste0(input.dir,input.files[which.files.year])
  if(any(file.size(this.year.files)==0)){
    next()
  }
  
  if(all(!file.exists(this.year.files))){
    print(paste0('File does not exist: ',this.year.file))
    next()
  }
  
  year.dates = seq.Date(as.Date(paste0(run.years[y],'-01-01')), as.Date(paste0(run.years[y],'-12-31')), by = '1 day')
  file.dates <- sub(".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", this.year.files)
  if(!all(year.dates %in% file.dates)){
    print(paste0('Missing Dates in ',run.years[y],': ', paste0(year.dates[which(!(year.dates %in% file.dates))],collapse = ', ')))
    next()
  }
  
  #Make bottom_temp_model_gridded
  print(paste0('Starting bottom_temp_model_gridded for year: ',run.years[y], ' at ', Sys.time()))
  
  output.file3 = paste0(output.dir,'data/bottom_temp_model_gridded/GLORYS_bottom_temp_model_gridded_',run.years[y],'.csv')
  check.dir(output.file3)
  if(file.exists(output.file3)){
    print(paste0('File already exists: ',output.file3))
  }else{
    EDAB.GLORYS::make_bottom_temp_model_gridded(input.file = this.year.files,
                                                output.file = output.file3,
                                                shp.file = shp.file,
                                                file.year = run.years[y],
                                                write.out = T)
  }
  
  
  #Make bottom_temp_model_anom
  print(paste0('Starting bottom_temp_model_anom for year: ',run.years[y], ' at ', Sys.time()))
  
  output.file4 = paste0(output.dir,'data/bottom_temp_model_anom/GLORYS_bottom_temp_model_anom_',run.years[y],'.csv')
  check.dir(output.file4)
  if(file.exists(output.file4)){
    print(paste0('File already exists: ',output.file4))
  }else{
    EDAB.GLORYS::make_bottom_temp_model_anom(input.file = this.year.files,
                                             output.file = output.file4,
                                             shp.file =shp.file,
                                             file.year =  run.years[y],
                                             climatology.file = output.file1,
                                             write.out =T)
  }
  
  #Make bottom_temp_model_annual
  print(paste0('Starting bottom_temp_model_annual for year: ',run.years[y], ' at ', Sys.time()))
  
  output.file5 = paste0(output.dir,'data/bottom_temp_model_annual/GLORYS_bottom_temp_model_annual_',run.years[y],'.csv')
  check.dir(output.file5)
  if(file.exists(output.file5)){
    print(paste0('File already exists: ',output.file5))
  }else{
    EDAB.GLORYS::make_bottom_temp_model_annual(input.file = this.year.files,
                                               output.file = output.file5,
                                               shp.file = shp.file,
                                               file.year = run.years[y],
                                               climatology.file =  output.file2,
                                               write.out =T
    )
                                               
  }
  
  print(paste0('Starting bottom_temp_daily_epu for year: ',run.years[y], ' at ', Sys.time()))
  
  output.file5b = paste0(output.dir,'data/bottom_temp_daily_epu/GLORYS_bottom_temp_daily_epu_',run.years[y],'.csv')
  check.dir(output.file5b)
  if(file.exists(output.file5b)){
    print(paste0('File already exists: ',output.file5b))
  }else{
    EDAB.GLORYS::make_bottom_temp_daily_epu(input.file = this.year.files,
                                               output.file = output.file5b,
                                               shp.file = shp.file,
                                               file.year = run.years[y],
                                               write.out =T
    )
    
  }
  

  #Make thermal_habitat_gridded
  print(paste0('Starting thermal_habitat_gridded for year: ',run.years[y], ' at ', Sys.time()))
        
  output.file6 = paste0(output.dir,'data/thermal_habitat_gridded/GLORYS_thermal_habitat_gridded_',run.years[y],'.nc')
  check.dir(output.file6)
  if(file.exists(output.file6) | no.threshold){
    print(paste0('File already exists: ',output.file6))
  }else{
    EDAB.GLORYS::make_thermal_habitat_gridded(input.file = this.year.files,
                                              output.file =  output.file6,
                                              supp.dir = supp.dir,
                                              shp.file = shp.file,
                                              file.year = run.years[y],
                                              write.out =T,
                                              t.max.seq = seq(0,30,1)
    )
  }
  
  #Make thermal_habitat_area
  print(paste0('Starting thermal_habitat_area for year: ',run.years[y], ' at ', Sys.time()))
        
  output.file7a = paste0(output.dir,'data/thermal_habitat_area/GLORYS_thermal_habitat_area_',run.years[y],'.csv')
  output.file7b = paste0(output.dir,'data/thermal_habitat_gridded/GLORYS_thermal_habitat_gridded_',run.years[y],'.csv')
  check.dir(output.file7a)
  
  if(file.exists(output.file7a) & file.exists(output.file7b) | no.threshold){
    print(paste0('File already exists: ',output.file7a, ' and ', output.file7b))
  }else{

    if(any(file.size(this.year.files)==0))
    EDAB.GLORYS::make_thermal_habitat_area(input.file = this.year.files,
                                           output.file.area =  output.file7a,
                                           output.file.gridded = output.file7b  ,
                                           shp.file = shp.file,
                                           file.year = run.years[y],
                                           write.area =T,
                                           write.gridded =T,
                                           t.max.seq = seq(0,30,1)
    )
  }
  

}

glorys.heatwave.files = c(list.files(paste0(output.dir,'data/bottom_temp_daily_epu/'),full.names = T),'/home/jcaracappa/EDAB_Dev/jcaracappa/ROMS_NWA/ROMS_daily_epu_1959_1992.csv' )
glorys.heatwave.data = lapply(glorys.heatwave.files, read.csv) |>
  dplyr::bind_rows() |>
  dplyr::mutate(source_m = dplyr::coalesce(Source, source)) |>
  dplyr::select(-Source,-source) |>
  dplyr::rename(source = 'source_m')
#check all dates
all(1959:2025 %in% sort(unique(format(as.Date(glorys.heatwave.data$date),format = '%Y')) ))
glorys.heatwaves.out = write.csv(glorys.heatwave.data,paste0('/home/jcaracappa/EDAB_Dev/jcaracappa/ROMS_GLORYS_bottom_temp_model_daily_epu_',1959,'_',format(Sys.time(), '%Y'),'.csv'), row.names =F)

season.index = data.frame(month = 1:12, season = rep(c('Winter','Spring','Summer','Fall'),each = 3))
data = glorys.heatwave.data %>% 
  dplyr::mutate(month = as.numeric(format(as.Date(date),format = '%m')),
                year = as.numeric(format(as.Date(date),format = '%Y'))) %>% 
  dplyr::left_join(season.index) %>% 
  dplyr::group_by(source,year,season,EPU) %>% 
  dplyr::summarise(Value = mean(BottomT.mean,na.rm=T))
  
library(ggplot2)
ggplot(data, aes(x = year, y = Value, color = source))+
  geom_line()+
  facet_grid(EPU~season)
ggsave(here::here('GLORYS_bottom_temp_2025.png'),width =12, height = 12)

model.anom.files = list.files(paste0(output.dir,'data/bottom_temp_model_anom/'),full.names = T)
model.anom.data = lapply(model.anom.files, read.csv) |>
  dplyr::bind_rows()
saveRDS(model.anom.data, file = 'V:/GLORYS_bottom_temp_model_anom_1993_2025.rds')

model.annual.files = list.files(paste0(output.dir,'data/bottom_temp_model_annual/'),full.names = T)
model.annual.data = lapply(model.annual.files, read.csv) |>
  dplyr::bind_rows()
saveRDS(model.annual.data, file = 'V:/GLORYS_bottom_temp_model_annual_1993_2025.rds')

model.gridded.files = list.files(paste0(output.dir,'data/bottom_temp_model_gridded/'),full.names = T)
model.gridded.data = lapply(model.gridded.files,read.csv) |>
  dplyr::bind_rows()
saveRDS(model.gridded.data, file = 'V:/GLORYS_bottom_temp_model_gridded_1993_2025.rds')

thermal.area.files = list.files(paste0(output.dir,'data/thermal_habitat_area/'),full.names = T)
thermal.area.data = lapply(thermal.area.files, read.csv) |>
  dplyr::bind_rows()
saveRDS(thermal.area.data, file = 'V:/GLORYS_thermal_habitat_area_1993_2025.rds')

#only get csv files
thermal.gridded.files = list.files(path = paste0(output.dir,'data/thermal_habitat_gridded/'),pattern = '*.csv',full.names = T)
thermal.gridded.data = lapply(thermal.gridded.files, read.csv) |>
  dplyr::bind_rows()
saveRDS(thermal.gridded.data, file = 'V:/GLORYS_thermal_habitat_gridded_1993_2025.rds')
