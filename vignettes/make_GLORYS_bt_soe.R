
#Gets arguments from command line
args = commandArgs(trailingOnly = TRUE)
if(length(args)>0){
  print(args)
  input.dir= args[1]
  supp.dir = args[2]
  output.dir = args[3]
  print('Using command line arguments')
}else{
  input.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/'
  output.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/READ_EDAB_GLORYS/'
  supp.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/READ_EDAB_GLORYS/data-raw/'
  
  print('Using default arguments')
}
shp.file = paste0(supp.dir,'geometry/EPU_NOESTUARIES.shp')

#Get year range
year.start = 2021
year.end = format(Sys.time(), '%Y')
run.years = year.start:year.end
y=1

#Make seasonal climatology
output.file1 =  paste0(output.dir, 'data/climatology/GLORYS_bottom_temp_seasonal_clim_1990_2020.csv')
if(!file.exists(output.file1)){
  EDAB.GLORYS::make_gridded_climatology(input.dir = input.dir,
                                        input.prefix = 'GLORYS_daily_BottomTemp_',
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
output.file2 = paste0(output.dir,'data/climatology/GLORYS_bottom_temp_annual_clim_1990_2020.csv')
if(!file.exists(output.file2)){
  EDAB.GLORYS::make_gridded_climatology(input.dir =input.dir,
                                        input.prefix = 'GLORYS_daily_BottomTemp_',
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
  
  this.year.file = paste0(input.dir,'GLORYS_daily_BottomTemp_',run.years[y],'.nc')
  
  if(!file.exists(this.year.file)){
    print(paste0('File does not exist: ',this.year.file))
    next()
  }
  
  #Make bottom_temp_model_gridded
  print(paste0('Starting bottom_temp_model_gridded for year: ',run.years[y], ' at ', Sys.time()))
  
  output.file3 = paste0(output.dir,'data/bottom_temp_model_gridded/GLORYS_bottom_temp_model_gridded_',run.years[y],'.csv')
  if(file.exists(output.file3)){
    print(paste0('File already exists: ',output.file3))
  }else{
    EDAB.GLORYS::make_bottom_temp_model_gridded(input.file = this.year.file,
                                                output.file = output.file3,
                                                shp.file = shp.file,
                                                file.year = run.years[y],
                                                write.out = T)
  }
  
  
  #Make bottom_temp_model_anom
  print(paste0('Starting bottom_temp_model_anom for year: ',run.years[y], ' at ', Sys.time()))
  
  output.file4 = paste0(output.dir,'data/bottom_temp_model_anom/GLORYS_bottom_temp_model_anom_',run.years[y],'.csv')
  if(file.exists(output.file4)){
    print(paste0('File already exists: ',output.file4))
  }else{
    EDAB.GLORYS::make_bottom_temp_model_anom(input.file = this.year.file,
                                             output.file = output.file4,
                                             shp.file =shp.file,
                                             file.year =  run.years[y],
                                             climatology.file =  paste0(output.dir,'data/climatology/GLORYS_bottom_temp_seasonal_clim_1990_2020.csv'),
                                             write.out =T)
  }
  
  #Make bottom_temp_model_annual
  print(paste0('Starting bottom_temp_model_annual for year: ',run.years[y], ' at ', Sys.time()))
  
  output.file5 = paste0(output.dir,'data/bottom_temp_model_annual/GLORYS_bottom_temp_model_annual_',run.years[y],'.csv')
  if(file.exists(output.file5)){
    print(paste0('File already exists: ',output.file5))
  }else{
    EDAB.GLORYS::make_bottom_temp_model_annual(input.file = this.year.file,
                                               output.file = output.file5,
                                               shp.file = shp.file,
                                               file.year = run.years[y],
                                               climatology.file =  paste0(output.dir,'data/climatology/GLORYS_bottom_temp_annual_clim_1990_2020.csv'),
                                               write.out =T
    )
                                               
  }
  
  #Make thermal_habitat_gridded
  print(paste0('Starting thermal_habitat_gridded for year: ',run.years[y], ' at ', Sys.time()))
        
  output.file6 = paste0(output.dir,'data/thermal_habitat_gridded/GLORYS_thermal_habitat_gridded_',run.years[y],'.nc')
  if(file.exists(output.file6)){
    print(paste0('File already exists: ',output.file6))
  }else{
    EDAB.GLORYS::make_thermal_habitat_gridded(input.file = this.year.file,
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
  
  if(file.exists(output.file7a) & file.exists(output.file7b)){
    print(paste0('File already exists: ',output.file7a, ' and ', output.file7b))
  }else{
    EDAB.GLORYS::make_thermal_habitat_area(input.file = this.year.file,
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




