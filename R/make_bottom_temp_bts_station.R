#convert neus_grid.rds to shape file then extract coordinates
# grid.file = here::here('data-raw','neus_grid.rds')
# grid.sf = readRDS(grid.file)
# grid.sf = sf::st_as_sf(grid.sf)
# grid.shp = terra::vect(grid.sf)
# grid.center =terra::centroids(grid.shp)

data.dir <- '//nefscdata/EDAB_Datasets/GLORYS/V12/SOURCE/NES_9KM_DAILY/GLOBAL_MULTIYEAR_PHY_001_030/BOTTOMT/'
coord.file <- here::here('data-raw', 'station_locations.rds')
make_bottom_temp_bts_station <- function(data.dir, coord.file) {
  coord.df <- readRDS(coord.file) |>
    dplyr::rename(lat = 'LAT', lon = 'LON', date = 'EST_TOWDATE')

  coord.dates <- unique(coord.df$date) |> as.Date()

  input.files <- list.files(
    path = data.dir,
    pattern = 'GLORYS_REANALYSIS-DAILY-BOTTOMT-',
    full.names = TRUE
  )

  input.files.date.str <- gsub(".*(\\d{8}).*", '\\1', basename(input.files))
  input.files.date <- as.Date(input.files.date.str, format = '%Y%m%d')
  input.files.year <- format(input.files.date, format = '%Y') |> as.numeric()

  input.files.match <- input.files[which(input.files.date %in% coord.dates)]
  input.year.match <- input.files.year[which(input.files.date %in% coord.dates)]

  start.year <- min(input.year.match)
  end.year <- max(input.year.match)

  data.years <- unique(input.year.match)

  out.dir <- '//nefscdata/EDAB_Dev/jcaracappa/GLORYS_SOE_test/bottom_temp_bts_station/'
  if (!dir.exists(out.dir)) {
    dir.create(out.dir)
  }

  for (i in 1:length(data.years)) {
    out.file <- paste0(
      out.dir,
      'bottom_trawl_survey_stations_GLORYS_',
      data.years[i],
      '.csv'
    )

    this.year.files <- input.files[which(input.files.year == data.years[i])]
    this.year.files <- this.year.files[which(
      this.year.files %in% input.files.match
    )]

    EDABUtilities::extract_daily_coord(
      data.in = this.year.files,
      output.file = out.file,
      coord.df = coord.df,
      var.name = 'theao',
      statistics = c('mean', 'sd'),
      write.out = T
    )
  }
}
