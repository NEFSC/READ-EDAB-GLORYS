import datetime
import pandas
import os
from os import path

#Set start and end year
year_start=2024
year_end=2024
years=range(year_start,year_end+1) 

#Input CMEMS User and Password (case sensitive)
#USER = ""
#PASSWORD = ""

#Set Lat/lon bounds
min_lon = str(-82.5)
max_lon = str(-51.5)
min_lat = str(22.5)
max_lat = str(48.5)

for y in range(len(years)):

    out_dir = "C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/Daily_Bottom_Temp/"+str(years[y])+"/"
    if(not os.path.exists(out_dir)):
        os.makedirs(out_dir)
        
        
    dt = datetime.datetime(years[y],1,1)
    end = datetime.datetime(years[y],12,31)
    step = datetime.timedelta(days = 1)
    
    all_days = []
    
    while dt <= end:
        all_days.append(dt.strftime('%Y-%m-%d'))
        dt += step
        
    for d in range(len(all_days)):
        t1 = all_days[d]+"T00:00:00"
        t2 = all_days[d]+"T23:59:59"
        #Change to desired filename prefix
        new_name = "GLORYS_REANALYSIS_"+all_days[d]+".nc"
        #Change to appropriate output path
        
        
        if(path.exists(out_dir+new_name)):
           print(new_name+" EXISTS")
           continue
           
        #print(new_name)
        #If additional variables are desired: need to add "--variable var.name"
        
        command = "copernicusmarine subset -i cmems_mod_glo_phy_myint_0.083deg_P1D-m -x "+min_lon+" -X "+max_lon+" -y "+min_lat+" -Y "+max_lat+" -z 0. -Z 5000. -t "+t1+" -T "+t2+" -v sea_water_potential_temperature_at_sea_floor -o "+out_dir+" -f "+new_name+" --force-download"
        
        #print(command)
        os.system(command)