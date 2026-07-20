#' Formats data for soe vast models
#'
#' descriptions
#'
#' @param input.index Dataframe of series|dir.name|start.year|end.year
#' @param output.dir Full output file directory
#' @param output.prefix string. Prefix for the output files
#' @param shp.file Full input file directory for the cold pool area shape file
#' @param origin string. Origin date for the time variable in the netCDF files (default '1970-01-01')
#' @param time.unit string. Time unit for the time variable in the netCDF files (default 'days')
#' 
#' @return Annual RDS file with the processed data for GLORYS and ROMS (mod_bt longitude latitude time date year month day)
#' 
#' @importFrom magrittr "|>"
#' 
#' @export
#' 

# input.index = data.frame(
#   series = c('ROMS_debiased','cmems_mod_glo_phy_my_0.083deg_P1D-m'),
#   dir.name = c('W:/ROMS_NWA/',
#                'W:/GLORYS/glorys_bottomT/cmems_mod_glo_phy_my_0.083deg_P1D-m/bottomT/'),
#   start.year = c(1959,1993),
#   end.year = c(1992, 2025)
# )
# output.dir = 'W:/GLORYS/glorys_bottomT/annual_bottomT_vast/V2/'
# output.prefix = 'ROMS_GLORYS_bt_VAST_'
# shp.file = 'Y:/workflow_resources/shapefiles/EPU_NOESTUARIES.shp'
# i = yr = f = 1
# origin = '1950-01-01'
# time.unit = 'days'
# redo = T

make_bottom_temp_temp_vast = function(input.index, output.dir,output.prefix,shp.file, redo =F){

  shp  = terra::vect(shp.file)
  
  #Loop through each data source and get the full year range of each file in a list
  for(i in 1:nrow(input.index)){
    
    year.seq = input.index$start.year[i]:input.index$end.year[i]
    
    # Returns a single vector of all years found in file names
    series.files = list.files(input.index$dir.name[i],full.names = T)
    series.files.year.match <- regmatches(series.files, gregexpr("\\d{4}", series.files))
    
    # Convert the strings to numbers
    series.files.year <- lapply(series.files.year.match, as.numeric)
    multi.year.files = any(unlist(lapply(series.files.year, function(x) length(x) > 1)))
    
    for(yr in 1:length(year.seq)){
      
      is.leap.year = ((year.seq[yr] %% 4 == 0) & (year.seq[yr] %% 100 != 0)) | (year.seq[yr] %% 400 == 0)
      
      if(multi.year.files == T){
        
        #Create of list of year sequences based on series.files.year
        file.year.ls = lapply(series.files.year, function(x) seq(min(x), max(x)))
      }else{
        file.year.ls = unlist(series.files.year)
      }
      
      this.year.file.match = which(sapply(file.year.ls, function(x) year.seq[yr] %in% x))
      this.year.files = series.files[this.year.file.match]
      
      this.year.ls = list()
      for(f in 1:length(this.year.files)){
       
        this.output.file = paste0(output.dir, output.prefix, year.seq[yr],'.rds')
        if(redo == F & file.exists(this.output.file)){
          next()
        }
        file.data = terra::crop(terra::rast(this.year.files[f],subds = 'sea_water_temperature_at_sea_floor'), shp)
        
        file.time = terra::time(file.data)
        #If all time vars are empty probably formated for ROMS data with year and day in the variable name
        if(all(is.na(file.time))){
          #Get data names and years
          var.year =  as.numeric(gsub(".*(\\d{4}).*", '\\1', terra::names(file.data)))
          var.day = as.numeric(regmatches(terra::names(file.data),regexpr("\\d{1,3}$", terra::names(file.data))))
          
          if(is.leap.year){
            which.to.keep = which(var.year == year.seq[yr])
          }else{
            which.to.keep = which(var.year == year.seq[yr] & var.day != 366)
          }
          file.data.sub = terra::subset(file.data, which.to.keep)  
          #Subset to only this years matches
          
          file.data.names = terra::names(file.data.sub)
          #Get other time variables from names
          var.day = as.numeric(regmatches(file.data.names,regexpr("\\d{1,3}$", file.data.names)))
          
          var.date = as.Date(paste(year.seq[yr], var.day), format = "%Y %j")
          
          #Get the number of "time.units" from "origin"
          var.time = as.numeric(difftime(var.date, as.Date(origin), units = time.unit))
          
          #Extract all data in file.data into a data.frame
          var.step = 1:length(var.time)
          file.data.df = lapply(var.step, function(x){
            dat = terra::subset(file.data,x)
            
            df = terra::as.data.frame(dat, xy = T, na.rm=T) |> 
              dplyr::rename(longitude = 'x',latitude = 'y') |> 
              dplyr::mutate(date = var.date[x])
            colnames(df)[3] = 'mod_bt'
            return(df)
          })
          file.data.df = dplyr::bind_rows(file.data.df) |> 
            dplyr::mutate(
              time = as.numeric(difftime(date, as.Date(origin), units = time.unit)),
              year = as.numeric(format(date,format = '%Y')),
              month = as.numeric(format(date, format = '%m')),
              day = as.numeric(format(date,format = '%j'))
            )
          
        }else{
          
          file.date = as.Date(terra::time(file.data))
          file.data.df = terra::as.data.frame(file.data, xy = T, na.rm=T) |> 
            dplyr::mutate(
              date = file.date,
              time = as.numeric(difftime(date, as.Date(origin), units = time.unit)),
              year = as.numeric(format(date,format = '%Y')),
              month = as.numeric(format(date, format = '%m')),
              day = as.numeric(format(date,format = '%j'))
            )
          colnames(file.data.df)[3] = 'mod_bt'
          
        }
        
        print(this.year.files[f])
        this.year.ls[[f]] = file.data.df
      }
      saveRDS(dplyr::bind_rows(this.year.ls),this.output.file)
      print(year.seq[yr])
    }

    
    
  }
  
}
