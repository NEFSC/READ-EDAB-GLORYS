data.dir <- '//nefscdata/EDAB_Datasets/GLORYS/V12/SOURCE/NES_9KM_DAILY/GLOBAL_MULTIYEAR_PHY_001_030/BOTTOMT/'
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
shp.file <- '//nefscdata/EDAB_Resources/workflow_resources/shapefiles/NES_BOTTOM_TRAWL_STRATA.shp'

output.dir <- '//nefscdata/EDAB_Dev/jcaracappa/shrimp_strata/2026/'
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
    strata = strata,
    statistics = 'mean',
    write.out = T
  )
}
