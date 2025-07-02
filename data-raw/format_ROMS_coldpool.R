#Formats a static debiased ROMS bottom temperature for cold pool indices 

roms.file = 'C:/Users/joseph.caracappa/Documents/GitHub/bottom_temp_SOE/data/ROMS/bottom_temp_debiased_roms_reg112_1959_2004.nc'
roms.years = 1959:1992
output.dir = here::here('data','cold_pool','ROMS','/')

cp.shp = terra::vect(here::here('data-raw','geometry','cold_pool_area.shp'))
#Get ROMS data
roms.rast = terra::rast(roms.file)
roms.names = terra::names(roms.rast)
#Get first number from roms.names
roms.day = as.numeric(sub('^[^0-9]+([0-9]+).*', '\\1', roms.names))
roms.year = as.numeric(sub('.*=([0-9]{4})$', '\\1', roms.names))
roms.date = as.Date(paste(roms.year, roms.day), format = "%Y %j")

#Remove incorrect leap days and not within time period
roms.rm = is.na(roms.date) | roms.year< roms.years[1] | roms.year > roms.years[length(roms.years)]
roms.rast = terra::subset(roms.rast, !roms.rm)
roms.date = roms.date[!roms.rm]

#Rename ROMS raster layers
terra::time(roms.rast) = roms.date

#Crop and mask to cp.shp
roms.crop = roms.rast %>%
  terra::crop(cp.shp) %>%
  terra::mask(cp.shp)
terra::varnames(roms.crop) = 'BottomT'
names(roms.crop) = terra::time(roms.crop)

terra::writeCDF(roms.crop,varname = 'BottomT',filename=paste0(output.dir,'roms_debiased_cold_pool_',roms.years[1],'_',roms.years[length(roms.years)],'.nc'),overwrite =T)

#convert a layer of roms.crop to a supplemental cold pool grid file
roms.grid = terra::subset(roms.crop,1)
terra::values(roms.grid) = NA
terra::writeCDF(roms.grid,varname = 'BottomT',filename=here::here('data','cold_pool','roms_cold_pool_grid.nc'),overwrite =T)

roms.df = terra::as.data.frame(roms.crop,cell = T)%>%
  tidyr::gather(date,value,-cell)%>%
  dplyr::mutate(date = as.Date(date),
                month = format(date, "%m"),
                year = format(date,'%Y'))%>%
  dplyr::group_by(year,month,cell)%>%
  dplyr::summarise(value = mean(value,na.rm=T))%>%
  dplyr::mutate(source = 'ROMS')

saveRDS(roms.df, paste0(output.dir,'roms_debiased_cold_pool_monthly_',roms.years[1],'_',roms.years[length(roms.years)],'.rds'))
