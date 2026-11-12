library(dplyr)
library(ggplot2)

soe.dir = '/home/jcaracappa/EDAB_Dev/jcaracappa/glorys_SOE_test/SOE_Indicators/'

#bottom_temp_model_gridded
bt.grid.ecodata = ecodata::bottom_temp_model_gridded |> 
  group_by(Time, Var) |> 
  summarise(value.mean = mean(Value,na.rm=T),
            value.sum = sum(Value,na.rm=T)) |> 
  mutate(source = 'ecodata')

colnames(ecodata::bottom_temp_model_gridded)

bt.grid.workflow = read.csv(paste0(soe.dir,'GLORYS_bottom_temp_model_gridded_1993_2026.csv')) |> 
  filter(Time %in% unique(bt.grid.ecodata$Time)) |> 
  group_by(Time, Var) |> 
  summarise(value.mean = mean(Value,na.rm=T),
            value.sum = sum(Value,na.rm=T)) |> 
  mutate(source = 'workflow')

bt.grid.combined =  bt.grid.ecodata |> 
  bind_rows(bt.grid.workflow)

ggplot(bt.grid.combined, aes(x = Time, y = value.mean, color = source))+
  geom_line()+
  facet_wrap(~Var)

#bottom_temp_model_anom
bt.anom.ecodata = ecodata::bottom_temp_model_anom |>  mutate(source = 'ecodata') |> filter(!grepl('ROMS',Var))
bt.anom.workflow = read.csv(paste0(soe.dir,'GLORYS_bottom_temp_model_anom_1993_2026.csv')) |> mutate(source = 'workflow') 
bt.annual.workflow = read.csv(paste0(soe.dir,'GLORYS_bottom_temp_model_annual_1993_2026.csv')) |> mutate(source = 'workflow')
bt.anom.combined = bt.anom.ecodata |> bind_rows(bt.anom.workflow) |> bind_rows(bt.annual.workflow)

colnames(bt.anom.ecodata)
unique(bt.anom.ecodata$Var)

ggplot(bt.anom.combined, aes(x = Time, y = Value, color = source))+
  geom_line()+
  facet_grid(EPU~Var) 

#cold_pool
cp.ecodata = ecodata::cold_pool |> mutate(source = 'ecodata') 
  
unique(cp.ecodata$Var)
cp.workflow = read.csv(paste0(soe_dir, 'cold_pool_indices_1959_2026.csv')) |> mutate(source = 'workflow') 

cp.combined = cp.ecodata |> bind_rows(cp.workflow)
  

ggplot(cp.combined, aes(x = Time, y = Value, color = source))+
  geom_line()+
  facet_wrap(~Var,ncol = 1,scale = 'free_y') 

#thermal_habitat_area

th.ecodata = ecodata::thermal_habitat_area
unique(th.ecodata$temp.threshold) |> sort()
unique(th.ecodata$Depth) |> sort()

th.workflow =read.csv(paste0(soe_dir, 'GLORYS_thermal_habitat_area_1993_2026.csv'))

colnames(th.ecodata)
colnames(th.workflow)
