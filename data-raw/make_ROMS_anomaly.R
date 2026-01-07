library(dplyr)
month.season = data.frame(month = 1:12, season = rep(1:4,each = 3),
                          season.name = rep(c('Winter','Spring','Summer','Fall'),each = 3 ))

roms.data  = read.csv('/home/jcaracappa/EDAB_Dev/jcaracappa/ROMS_NWA/ROMS_daily_epu_1959_1992.csv') %>% 
  mutate(month = as.numeric(format(as.Date(date),format = '%m')),
         year = format(as.Date(date),format = '%Y')) %>% 
  left_join(month.season) %>% 
  group_by(year,season,season.name,EPU,Source) %>% 
  summarize(value.raw = mean(BottomT.mean,na.rm=T))

glorys.clim = read.csv('/home/jcaracappa/EDAB_Dev/jcaracappa/glorys_my_soe/data/climatology/GLORYS_bottom_temp_seasonal_clim_1990_2020.csv') %>% 
  rename(season = 'time',
         EPU = 'area')

roms.anom = roms.data %>% 
  left_join(glorys.clim) %>% 
  mutate(Value = value.raw - value.clim) %>%
  mutate(Time = as.numeric(year),
         Var = paste0(season.name,'_Bottom Temp Anomaly'),
         Units = 'degree C') %>% 
  ungroup() %>% 
  select(Time, Value, EPU, Source, Var, Units)

write.csv(roms.anom,'/home/jcaracappa/EDAB_Datasets/ROMS_NWA/roms_seasonal_anomaly.csv',row.names=F)

# library(ggplot2)
# ggplot()+
#   geom_line(data=model.anom.data,aes(x= Time, y = Value, color = Source))+
#   facet_grid(EPU~Var)

