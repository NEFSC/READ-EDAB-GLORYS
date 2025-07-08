#' Creates a monthly gridded summary of gridded data inputs
#'
#' descriptions
#'
#' @param input.dir String. The full input directory for daily input files
#' @param input.prefix string. The prefix of the input files
#' @param output.dir string. The full output directory for the gridded data
#' @param output.prefix string. The prefix of the output files
#' @param var.name string. The variable name for the gridded data
#' @param statistics string. The statistic to be used for the gridded data
#' 
#' @return a netCDF file with the monthly gridded data
#' 
#' @export
#' 

make_monthly_gridded = function(input.dir, input.prefix, output.dir, output.prefix,var.name, statistics){
  
  input.files.short = list.files(input.dir,input.prefix)
  
  #return YYYY-MM-DD from the file names
  input.file.date = as.Date(gsub( '.*_([0-9]{4})-([0-9]{2})-([0-9]{2}).*', '\\1-\\2-\\3', input.files.short))
  input.file.year = format(input.file.date,format = '%Y')
  input.file.month = format(input.file.date,format = '%m')

  #Find the last month/year in the output files
  output.files.short = list.files(output.dir,output.prefix)
  
  #Get current date
  current.date = Sys.Date()
  current.month = format(current.date, '%m')
  current.year = format(current.date, '%Y')
  #current.year = '2024'
  #current.month = '12'
  last.month = as.numeric(current.month) - 1
  
  #Find the first month/year in the input files
  first.year = min(input.file.year)
  first.year.first.month = which.min(as.numeric(input.file.month[which(input.file.year == first.year)]))
  first.year.first.month = ifelse(first.year.first.month < 10,paste0('0',first.year.first.month),first.year.first.month)
  
  #Generate all years/months the first month/year
  all.years = seq(first.year,current.year)
  
  possible.month.year = expand.grid(year = all.years, month = sprintf('%02d',1:12))
  
  #Do all missing month year combinations
  for(i in 1:nrow(possible.month.year)){
    
    this.month = possible.month.year[i,2]
    this.year = possible.month.year[i,1]
    
    #Which input.files match this.month and this.year
    this.month.files = input.files.short[which(input.file.year == this.year & input.file.month == this.month)]
    
    #Get number of days in a month
    expected.days <- as.numeric(format(seq.Date(from = as.Date(paste(possible.month.year$year[i], possible.month.year$month[i], "01", sep = "-")),by = "month", length.out = 2)[2] - 1, "%d"))
    
    if(paste0(output.prefix,possible.month.year$year[i], '_', possible.month.year$month[i],'.nc') %in% output.files.short){
      print(paste0('File for ',this.year,'-',this.month,' already exists. Skipping.'))
      next()
    }else if(length(this.month.files)!= expected.days){
        print(paste0('Number of files for ',this.year,'-',this.month,' is not equal to the number of days in the month. Check input directory and file names.'))
        next()
    }else{
      
      #Make the gridded summary
      data.summary =EDABUtilities::make_2d_summary_gridded(data.in = paste0(input.dir,this.month.files),
                                             file.time = 'daily',
                                             write.out =T,
                                             output.files = paste0(output.dir,output.prefix,this.year,'_',this.month,'.nc'),
                                             shp.file = NA,
                                             var.name = var.name,
                                             agg.time = 'months',
                                             statistic = statistics,
                                             area.names = NA)                                             
      
     
    }
  }
}

# make_monthly_gridded(input.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/Daily_Bottom_Temp/2024/',
#                      input.prefix = 'GLORYS_REANALYSIS_',
#                      output.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_monthly/',
#                      output.prefix = 'GLORYS_monthly_BottomTemp_',
#                      var.name = 'thetao',
#                      statistics = c('mean','median','sd'))
