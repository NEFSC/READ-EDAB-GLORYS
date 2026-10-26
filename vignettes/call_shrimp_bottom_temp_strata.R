root.dir = '/home/jcaracappa/'

data.dir <- paste0(root.dir,'EDAB_Datasets/GLORYS/V12/SOURCE/NES_9KM_DAILY/GLOBAL_MULTIYEAR_PHY_001_030/BOTTOMT/')
input.files <- list.files(
  path = data.dir,
  pattern = 'GLORYS_REANALYSIS-DAILY-BOTTOMT-',
  full.names = TRUE
)
input.files.date.str <- gsub(".*(\\d{8}).*", '\\1', basename(input.files))
input.files.date <- as.Date(input.files.date.str, format = '%Y%m%d')
input.files.year <- format(input.files.date, format = '%Y') |> as.numeric()

strata <- list(
  'STRATUMA' = c(
    '01240',
    '01260',
    '01270',
    '01280',
    '01370',
    '01380',
    '01390',
    '01400'
  )
)


shp.file <- paste0(root.dir,'EDAB_Resources/workflow_resources/shapefiles/NES_BOTTOM_TRAWL_STRATA.shp')

output.dir <- paste0(root.dir,'EDAB_Dev/jcaracappa/shrimp_strata/2026/')
if (!dir.exists(output.dir)) {
  dir.create(output.dir)
}
output.prefix <- 'bottom_temp_shrimp_strata_'

years <- sort(unique(input.files.year))

for (i in 1:length(years)) {
  this.year.input <- input.files[which(input.files.year == years[i])]

  this.output <- paste0(output.dir, output.prefix, years[i], '.csv')
  EDAB.GLORYS::make_bottom_temp_strata(
    input.files = this.year.input,
    output.file = this.output,
    shp.file = shp.file,
    var.name = 'BottomT',
    strata = strata,
    statistics = c('mean','sd'),
    write.out = T
  )
}

roms.file = paste0(root.dir,'EDAB_Datasets/ROMS_NWA/bottom_temp_debiased_roms_reg112_1959_2004.nc')
roms.years = 1959:1992

roms.rast = terra::rast(roms.file)

var.year <- as.numeric(gsub(
  ".*(\\d{4}).*",
  '\\1',
  terra::names(roms.rast)
))
var.day <- as.numeric(regmatches(
  terra::names(roms.rast),
  regexpr("\\d{1,3}$", terra::names(roms.rast))
))

file.data.names <- terra::names(roms.rast)

#Get other time variables from names
var.day <- as.numeric(regmatches(
  file.data.names,
  regexpr("\\d{1,3}$", file.data.names)
))

for(i in 1:length(roms.years)){
  
  is.leap.year <- ((roms.years[i] %% 4 == 0) & (roms.years[i]  %% 100 != 0)) |
    (roms.years[i]  %% 400 == 0)
  
  if (is.leap.year) {
    day.match <- var.day[which(var.year == roms.years[i])]
  } else {
    day.match <- var.day[which(var.year == roms.years[i] & var.day != 366)]
  }
  
  this.rast = terra::subset(roms.rast, day.match)
  
  these.dates = as.Date(paste(roms.years[i], var.day[day.match]), format = "%Y %j")
  
  terra::time(this.rast) = these.dates
  
  this.output <- paste0(output.dir, output.prefix, roms.years[i], '.csv')
  
  EDAB.GLORYS::make_bottom_temp_strata(
    input.files = this.rast,
    output.file = this.output,
    shp.file = shp.file,
    var.name = 'sea_water_temperature_at_sea_floor',
    strata = strata,
    statistics = c('mean','sd'),
    write.out = T
  )
}
