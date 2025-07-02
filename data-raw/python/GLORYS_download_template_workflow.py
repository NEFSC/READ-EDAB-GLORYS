import datetime
import pandas
import os
from os import path

#Set start and end year
year_start=2021
year_end=2021
years=range(year_start,year_end+1) 

#Input CMEMS User and Password (case sensitive)
#USER = ""
#PASSWORD = ""

#Set Lat/lon bounds
min_lon = str(-82.5)
max_lon = str(-51.5)
min_lat = str(22.5)
max_lat = str(48.5)

var_names  = ["sea_water_potential_temperature_at_sea_floor","sea_water_potential_temperature","sea_water_salinity","ocean_mixed_layer_thickness_defined_by_sigma_theta"] #Add additional variables as needed
var_dirs = ["bottomT","temperature","salinity","mixedLayerDepth"] #Add additional variable directories as needed
is_3d = [False,True,True] #Add additional variables as needed

prod_id = ["cmems_mod_glo_phy_my_0.083deg_P1D-m","cmems_mod_glo_phy_myint_0.083deg_P1D-m"] #Add additional variables as needed


parent_dir = "C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/Daily/"


#setup combinations of years and var_names

for p in range(len(prod_id)):

  for v in range(len(var_names)):
    
    out_dir = parent_dir+var_dirs[v]+"/"
    
    for y in range(len(years)):
    
        
        if(not os.path.exists(out_dir)):
            os.makedirs(out_dir)
            
        
        if(is_3d[v]):
          
              dt = datetime.datetime(years[y],1,1)
              end = datetime.datetime(years[y],5,30)
              step = datetime.timedelta(days = 1)
              
              all_days = []
    
              while dt <= end:
                all_days.append(dt.strftime('%Y-%m-%d'))
                dt += step
              
              for d in range(len(all_days)):
                
                t1 = all_days[d]+"T00:00:00"
                t2 = all_days[d]+"T23:59:59"
                
                new_name = "GLORYS_REANALYSIS_DAILY_"+var_dirs[v]+'_'+all_days[d]+".nc"
                #Change to appropriate output path
                
                if(path.exists(out_dir+new_name)):
                   print(new_name+" EXISTS")
                   continue
                   
                #print(new_name)
                #If additional variables are desired: need to add "--variable var.name"
                
                command = "copernicusmarine subset -i "+prod_id[p]+" -x "+min_lon+" -X "+max_lon+" -y "+min_lat+" -Y "+max_lat+" -z 0. -Z 5000. -t "+t1+" -T "+t2+" -v "+var_names[v]+" -o "+out_dir+" -f "+new_name+" --force-download"
                
                #print(command)
                os.system(command)
        
            
        else:
      
          #Format the first day of the year 
          date_start =  datetime.datetime(years[y], 1, 1).strftime("%Y-%m-%d")+"T00:00:00"
          date_end = datetime.datetime(years[y],12,31).strftime("%Y-%m-%d")+"T23:59:59"
          
          new_name = "GLORYS_REANALYSIS_DAILY_"+var_dirs[v]+'_'+str(years[y])+".nc"
          #Change to appropriate output path
          
          if(path.exists(out_dir+new_name)):
             print(new_name+" EXISTS")
             continue
             
          #print(new_name)
          #If additional variables are desired: need to add "--variable var.name"
          
          command = "copernicusmarine subset -i "+prod_id[p]+" -x "+min_lon+" -X "+max_lon+" -y "+min_lat+" -Y "+max_lat+" -z 0. -Z 5000. -t "+date_start+" -T "+date_end+" -v "+var_names[v]+" -o "+out_dir+" -f "+new_name+" --force-download"
          
          print(command)
          os.system(command)
