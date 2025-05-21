
#Get year range
year.start = 2023
year.end = format(Sys.time(), '%Y')
run.years = year.start:year.end
y=1

#Make seasonal climatology
output.file1 = here::here('data','climatology','GLORYS_bottom_temp_seasonal_clim_1990_2020.csv')
if(!file.exists(output.file1)){
  EDAB.GLORYS::make_gridded_climatology(input.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/',
                                        input.prefix = 'GLORYS_daily_BottomTemp_',
                                        output.file = output.file1,
                                        shp.file = here::here('data-raw','geometry','EPU_NOESTUARIES.shp'),
                                        write.out = T,
                                        ref.year.start = 1990,
                                        ref.year.end = 2020,
                                        agg.time = 'season',
                                        var.name = 'bottomT')
  
}

#Make Annual Climatology
output.file2 = here::here('data','climatology','GLORYS_bottom_temp_annual_clim_1990_2020.csv')
if(!file.exists(output.file2)){
  EDAB.GLORYS::make_gridded_climatology(input.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/',
                                        input.prefix = 'GLORYS_daily_BottomTemp_',
                                        output.file = output.file2,
                                        shp.file = here::here('data-raw','geometry','EPU_NOESTUARIES.shp'),
                                        write.out = T,
                                        ref.year.start = 1990,
                                        ref.year.end = 2020,
                                        agg.time = 'years',
                                        var.name = 'bottomT')
}

#Produce each year's indicies
for(y in 1:length(run.years)){
  
  this.year.file = paste0('C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/GLORYS_daily_BottomTemp_',run.years[y],'.nc')
  
  if(!file.exists(this.year.file)){
    print(paste0('File does not exist: ',this.year.file))
    next()
  }
  
  #Make bottom_temp_model_gridded
  output.file3 = here::here('data','bottom_temp_model_gridded',paste0('GLORYS_bottom_temp_model_gridded_',run.years[y],'.csv'))
  if(file.exists(output.file3)){
    next()
  }else{
    EDAB.GLORYS::make_bottom_temp_model_gridded(input.file = this.year.file,
                                                output.file = output.file3,
                                                shp.file = here::here('data-raw','geometry','EPU_NOESTUARIES.shp'),
                                                file.year = run.years[y],
                                                write.out = T)
  }
  
  
  #Make bottom_temp_model_anom
  output.file4 = here::here('data','bottom_temp_model_anom',paste0('GLORYS_bottom_temp_model_anom_',run.years[y],'.csv'))
  if(file.exists(output.file4)){
    next()
  }else{
    EDAB.GLORYS::make_bottom_temp_model_anom(input.file = this.year.file,
                                             output.file = output.file4,
                                             shp.file = here::here('data-raw','geometry','EPU_NOESTUARIES.shp'),
                                             file.year =  run.years[y],
                                             climatology.file =  here::here('data','climatology','GLORYS_bottom_temp_seasonal_clim_1990_2020.csv'),
                                             write.out =T)
  }
  
  #Make bottom_temp_model_annual
  output.file5 = here::here('data','bottom_temp_model_annual',paste0('GLORYS_bottom_temp_model_annual_',run.years[y],'.csv'))
  if(file.exists(output.file5)){
    next()
  }else{
    EDAB.GLORYS::make_bottom_temp_model_annual(input.file = this.year.file,
                                               output.file = output.file5,
                                               shp.file = here::here('data-raw','geometry','EPU_NOESTUARIES.shp'),
                                               file.year = run.years[y],
                                               climatology.file =  here::here('data','GLORYS','GLORYS_bottom_temp_clim_1990_2020.csv'),
                                               write.out =T
    )
                                               
  }
  
  #Make thermal_habitat_gridded
  output.file6 = here::here('data','thermal_habitat_gridded',paste0('GLORYS_thermal_habitat_gridded_',run.years[y],'.nc'))
  if(file.exists(output.file6)){
    next()
  }else{
    EDAB.GLORYS::make_thermal_habitat_gridded(input.file = this.year.file,
                                              output.file =  output.file6,
                                              shp.file = here::here('data-raw','geometry','EPU_NOESTUARIES.shp'),
                                              file.year = run.years[y],
                                              write.out =T,
                                              t.max.seq = seq(0,1,0.5)
    )
  }
  
  #Make thermal_habitat_area
  output.file7a = here::here('data','thermal_habitat_area',paste0('GLORYS_thermal_habitat_area_',run.years[y],'.csv'))
  output.file7b = here::here('data','thermal_habitat_gridded',paste0('GLORYS_thermal_habitat_gridded_',run.years[y],'.csv'))
  
  if(file.exists(output.file7a) & file.exists(output.file7b)){
    next()
  }else{
    EDAB.GLORYS::make_thermal_habitat_area(input.file = this.year.file,
                                           output.file.area =  output.file7a,
                                           output.file.gridded = output.file7b  ,
                                           shp.file = here::here('data-raw','geometry','EPU_NOESTUARIES.shp'),
                                           file.year = run.years[y],
                                           write.area =T,
                                           write.gridded =T,
                                           t.max.seq = seq(0,1,0.5)
    )
  }
}




