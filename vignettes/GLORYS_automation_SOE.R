#Gets arguments from command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  print(args)
  input.dir <- args[1]
  supp.dir <- args[2]
  output.dir <- args[3]
  print('Using command line arguments')
} else {
  input.dir <- '//nefscdata/EDAB_Datasets/'
  output.dir <- '//nefscdata/EDAB_Dev/'
  supp.dir <- '//nefscdata/EDAB_Resources/'
  indicator.dir <- '//nefscdata/EDAB_Indicators/'

  print('Using default arguments')
}

#helper functions

##Creates missing directories
check.dir <- function(file) {
  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = T)
  }
}

##pulls date from file name
strp.date <- function(file) {
  file.date <- gsub(".*(\\d{4})-?(\\d{2})-?(\\d{2}).*", "\\1-\\2-\\3", file) |>
    as.Date()
  return(file.date)
}

get_recent_file <- function(registry.file, dataset.name) {
  registry <- read.csv(registry.file)
  current.date <- Sys.Date()

  dataset.files <- registry[registry$dataset.name == dataset.name, ]
  recent.file <- dataset.files[which.max(as.Date(dataset.files$run.time)), ]

  return(recent.file$file.name)
}

check.date.file <- function(registry.file, dataset.name, input.dates) {
  registry <- read.csv(registry.file)

  dataset.files <- registry[registry$dataset.name == dataset.name, ]

  if (nrow(dataset.files) == 0) {
    out.ls <- list(
      'same.run.date' = F,
      'same.input.date' = F,
      'file.exists' = F
    )
  } else {
    recent.file <- dataset.files[which.max(as.Date(dataset.files$run.time)), ]

    current.date <- Sys.Date()

    same.run.date <- ifelse(current.date == recent.file$run.date, T, F)

    new.start.date <- min(input.dates, na.rm = T)
    new.end.date <- max(input.dates, na.rm = T)

    same.input.date <- ifelse(
      new.start.date >= recent.file$start.date |
        new.end.date <= recent.file$end.date,
      T,
      F
    )

    out.ls <- list(
      'same.run.date' = same.run.date,
      'same.input.date' = same.input.date,
      'file.exists' = file.exists(recent.file$file.name)
    )
  }

  return(out.ls)
}

append.registry <- function(
  dataset.name,
  file.name,
  registry.file,
  input.dates
) {
  reg.entry <- data.frame(
    dataset.name = dataset.name,
    file.name = file.name,
    run.date = Sys.Date(),
    run.time = Sys.time(),
    file.checksum = tools::md5sum(file.name),
    start.date = min(input.dates, na.rm = T),
    end.date = max(input.dates, na.rm = T)
  )
  write.table(
    reg.entry,
    file = registry.file,
    append = T,
    sep = ',',
    col.names = F,
    row.names = F
  )
}


## Define indicator directories
glorys.in.dir <- file.path(
  input.dir,
  'GLORYS/V12/SOURCE/NES_9KM_DAILY/GLOBAL_MULTIYEAR_PHY_001_030/BOTTOMT/'
)
roms.in.dir <- file.path(input.dir, 'ROMS_NWA/')
out.dir <- file.path(output.dir, 'jcaracappa/glorys_SOE_test/')

shp.file <- file.path(
  supp.dir,
  'workflow_resources/shapefiles/EPU_NOESTUARIES.shp'
)
registry.file <- file.path(
  supp.dir,
  'workflow_resources/glorys_soe_registry.csv'
)

soe_dir <- file.path(out.dir, 'SOE_Indicators')

## Check directories
if (!file.exists(shp.file)) {
  stop('Warning: Shape file not found ', shp.file)
}

if (!dir.exists(glorys.in.dir)) {
  stop(paste0('GLORYS Input directory does not exist: ', input.dir))
}

if (!dir.exists(roms.in.dir)) {
  stop(paste0('ROMS Input directory does not exist: ', roms.in.dir))
}

if (!dir.exists(out.dir)) {
  dir.create(out.dir, recursive = T)
  message('Creating output directory in ', out.dir)
}

if (!dir.exists(soe_dir)) {
  dir.create(soe_dir, recursive = T)
  message('Creating output directory in ', soe_dir)
}

if (!file.exists(registry.file)) {
  reg.cols <- c(
    'dataset.name',
    'file.name',
    'run.date',
    'run.time',
    'file.checksum',
    'start.date',
    'end.date'
  )
  df.start <- data.frame(matrix(ncol = length(reg.cols), nrow = 0))
  colnames(df.start) <- reg.cols
  write.csv(df.start, registry.file, row.names = F)
}

#Define Input Files
glorys.prefix <- 'GLORYS_REANALYSIS-DAILY-BOTTOMT-'
input.files <- list.files(glorys.in.dir, glorys.prefix, full.names = T)
input.files.date.str <- gsub(".*(\\d{8}).*", '\\1', basename(input.files))
input.files.date <- as.Date(input.files.date.str, format = '%Y%m%d')

input.files.year <- format(input.files.date, format = '%Y') |> as.numeric()
run.years <- seq(
  min(input.files.year, na.rm = T),
  max(input.files.year, na.rm = T)
)

#Get system date
current.date <- Sys.Date()

#Make seasonal climatology----
season.clim.prefix <- 'GLORYS_bottom_temp_seasonal_clim_1990_2020_'
season.clim.file <- file.path(
  out.dir,
  paste0(season.clim.prefix, current.date, '.csv')
)
check.dir(season.clim.file)

season.clim.check <- check.date.file(
  registry.file,
  dataset.name = 'seasonal_climatology',
  input.files.date
)

if (season.clim.check$same.input.date & season.clim.check$file.exists) {
  message('Seasonal climatology already exists and is up to date. Skipping.')
  season.clim.file <- get_recent_file(
    registry.file,
    dataset.name = 'seasonal_climatology'
  )
} else if (!season.clim.check$same.input.date) {
  season.clim.df <- EDAB.GLORYS::make_gridded_climatology(
    input.dir = glorys.in.dir,
    input.prefix = glorys.prefix,
    output.file = season.clim.file,
    shp.file = shp.file,
    write.out = F,
    ref.year.start = 1990,
    ref.year.end = 2020,
    agg.time = 'season',
    var.name = 'bottomT'
  )

  write.csv(season.clim.df, season.clim.file, row.names = F)
  if (!season.clim.check$same.run.date) {
    append.registry(
      dataset.name = 'seasonal_climatology',
      file.name = season.clim.file,
      registry.file = registry.file,
      input.dates = input.files.date
    )
  } else {
    message(
      'Seasonal climatology already exists but input files have changed. Overwriting with new data. No registry Change'
    )
  }
}

#Make Annual Climatology----
annual.clim.prefix <- 'GLORYS_bottom_temp_annual_clim_1990_2020_'
annual.clim.file <- file.path(
  out.dir,
  paste0(annual.clim.prefix, current.date, '.csv')
)
check.dir(annual.clim.file)

annual.clim.check <- check.date.file(
  registry.file,
  dataset.name = 'annual_climatology',
  input.files.date
)

if (annual.clim.check$same.input.date & annual.clim.check$file.exists) {
  message('Annual climatology already exists and is up to date. Skipping.')
  annual.clim.file <- get_recent_file(
    registry.file,
    dataset.name = 'annual_climatology'
  )
} else if (!annual.clim.check$same.input.date) {
  annual.clim.df <- EDAB.GLORYS::make_gridded_climatology(
    input.dir = glorys.in.dir,
    input.prefix = glorys.prefix,
    output.file = output.file2,
    shp.file = shp.file,
    write.out = F,
    ref.year.start = 1990,
    ref.year.end = 2020,
    agg.time = 'years',
    var.name = 'bottomT'
  )

  write.csv(annual.clim.df, annual.clim.file, row.names = F)
  if (!annual.clim.check$same.run.date) {
    append.registry(
      dataset.name = 'annual_climatology',
      file.name = annual.clim.file,
      registry.file = registry.file,
      input.dates = input.files.date
    )
  } else {
    message(
      'Annual climatology already exists but input files have changed. Overwriting with new data. No registry Change'
    )
  }
}

#Loop through years----
y <- 1
for (y in 1:length(run.years)) {
  #Check files in this year
  which.files.year <- which(input.files.year == run.years[y])
  if (length(which.files.year) == 0) {
    message('no files in this year')
    next()
  }

  this.year.files <- input.files[which.files.year]
  if (any(file.size(this.year.files) == 0)) {
    message('some files have zero size')
    next()
  }

  year.dates <- seq.Date(
    as.Date(paste0(run.years[y], '-01-01')),
    as.Date(paste0(run.years[y], '-12-31')),
    by = '1 day'
  )
  file.dates <- strp.date(this.year.files)
  if (!all(year.dates %in% file.dates)) {
    print(paste0(
      'Missing Dates in ',
      run.years[y],
      ': ',
      paste0(year.dates[which(!(year.dates %in% file.dates))], collapse = ', ')
    ))
  }

  #Make bottom_temp_model_gridded----
  print(paste0(
    'Starting bottom_temp_model_gridded for year: ',
    run.years[y],
    ' at ',
    Sys.time()
  ))

  bt.gridded.prefix <- 'GLORYS_bottom_temp_model_gridded_'
  bt.gridded.file <- paste0(
    out.dir,
    '/bottom_temp_model_gridded/',
    bt.gridded.prefix,
    run.years[y],
    '.csv'
  )
  check.dir(bt.gridded.file)

  bt.gridded.check <- check.date.file(
    registry.file,
    dataset.name = paste0('bt_gridded_', run.years[y]),
    input.dates = file.dates
  )

  if (bt.gridded.check$same.input.date & bt.gridded.check$file.exists) {
    message(
      'bottom_temp_model_gridded_',
      run.years[y],
      ' already exists and is up to date. Skipping.'
    )
    bt.gridded.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('bt_gridded_', run.years[y])
    )
  } else if (
    !bt.gridded.check$same.input.date | !bt.gridded.check$file.exists
  ) {
    bt.gridded.df <- EDAB.GLORYS::make_bottom_temp_model_gridded(
      input.file = this.year.files,
      output.file = bt.gridded.file,
      shp.file = shp.file,
      file.year = run.years[y],
      write.out = F
    )

    write.csv(bt.gridded.df, bt.gridded.file, row.names = F)
    if (!bt.gridded.check$same.run.date) {
      append.registry(
        dataset.name = paste0('bt_gridded_', run.years[y]),
        file.name = bt.gridded.file,
        registry.file = registry.file,
        input.dates = file.dates
      )
    } else {
      message(
        'bottom_temp_model_gridded_',
        run.years[y],
        ' already exists but input files have changed. Overwriting with new data. No registry Change'
      )
    }
  }

  #Make bottom_temp_model_anom----
  print(paste0(
    'Starting bottom_temp_model_anom for year: ',
    run.years[y],
    ' at ',
    Sys.time()
  ))

  bt.anom.prefix <- 'GLORYS_bottom_temp_model_anom_'
  bt.anom.file <- paste0(
    out.dir,
    '/bottom_temp_model_anom/',
    bt.anom.prefix,
    run.years[y],
    '.csv'
  )
  check.dir(bt.anom.file)

  bt.anom.check <- check.date.file(
    registry.file,
    dataset.name = paste0('bt_anom_', run.years[y]),
    input.dates = file.dates
  )

  if (bt.anom.check$file.exists & bt.anom.check$same.input.date) {
    message(
      'bottom_temp_model_anom_',
      run.years[y],
      ' already exists and is up to date. Skipping.'
    )
    bt.anom.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('bt_anom_', run.years[y])
    )
  } else if (!bt.anom.check$same.input.date | !bt.anom.check$file.exists) {
    bt.anom.df <- EDAB.GLORYS::make_bottom_temp_model_anom(
      input.file = this.year.files,
      output.file = bt.anom.file,
      shp.file = shp.file,
      file.year = run.years[y],
      climatology.file = season.clim.file,
      write.out = F
    )

    write.csv(bt.anom.df, bt.anom.file, row.names = F)
    if (!bt.anom.check$same.run.date) {
      append.registry(
        dataset.name = paste0('bt_anom_', run.years[y]),
        file.name = bt.anom.file,
        registry.file = registry.file,
        input.dates = file.dates
      )
    } else {
      message(
        'bottom_temp_model_anom_',
        run.years[y],
        ' already exists but input files have changed. Overwriting with new data. No registry Change'
      )
    }
  }

  #Make bottom_temp_model_annual----
  print(paste0(
    'Starting bottom_temp_model_annual for year: ',
    run.years[y],
    ' at ',
    Sys.time()
  ))

  bt.annual.prefix <- 'GLORYS_bottom_temp_model_annual_'
  bt.annual.file <- paste0(
    out.dir,
    '/bottom_temp_model_annual/',
    bt.annual.prefix,
    run.years[y],
    '.csv'
  )
  check.dir(bt.annual.file)

  bt.annual.check <- check.date.file(
    registry.file,
    dataset.name = paste0('bt_annual_', run.years[y]),
    input.dates = file.dates
  )

  if (bt.annual.check$file.exists & bt.annual.check$same.input.date) {
    message(
      'bottom_temp_model_annual_',
      run.years[y],
      ' already exists and is up to date. Skipping.'
    )
    bt.annual.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('bt_anom_', run.years[y])
    )
  } else if (!bt.annual.check$same.input.date | !bt.annual.check$file.exists) {
    bt.annual.df <- EDAB.GLORYS::make_bottom_temp_model_annual(
      input.file = this.year.files,
      output.file = bt.annual.file,
      shp.file = shp.file,
      file.year = run.years[y],
      climatology.file = annual.clim.file,
      write.out = F
    )

    write.csv(bt.annual.df, bt.annual.file, row.names = F)
    if (!bt.annual.check$same.run.date) {
      append.registry(
        dataset.name = paste0('bt_annual_', run.years[y]),
        file.name = bt.annual.file,
        registry.file = registry.file,
        input.dates = file.dates
      )
    } else {
      message(
        'bottom_temp_model_annual_',
        run.years[y],
        ' already exists but input files have changed. Overwriting with new data. No registry Change'
      )
    }
  }

  print(paste0(
    'Starting bottom_temp_daily_epu for year: ',
    run.years[y],
    ' at ',
    Sys.time()
  ))

  bt.epu.prefix <- 'GLORYS_bottom_temp_daily_epu_'
  bt.epu.file <- paste0(
    out.dir,
    '/bottom_temp_daily_epu/',
    bt.epu.prefix,
    run.years[y],
    '.csv'
  )
  check.dir(bt.epu.file)

  bt.epu.check <- check.date.file(
    registry.file,
    dataset.name = paste0('bt_epu_', run.years[y]),
    input.dates = file.dates
  )

  if (bt.epu.check$file.exists & bt.epu.check$same.input.date) {
    message(
      'bottom_temp_daily_epu_',
      run.years[y],
      ' already exists and is up to date. Skipping.'
    )
    bt.epu.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('bt_anom_', run.years[y])
    )
  } else if (!bt.epu.check$same.input.date) {
    bt.epu.df <- EDAB.GLORYS::make_bottom_temp_daily_epu(
      input.file = this.year.files,
      output.file = bt.epu.file,
      shp.file = shp.file,
      file.year = run.years[y],
      write.out = F
    )

    write.csv(bt.epu.df, bt.epu.file, row.names = F)
    if (!bt.epu.check$same.run.date) {
      append.registry(
        dataset.name = paste0('bt_epu_', run.years[y]),
        file.name = bt.epu.file,
        registry.file = registry.file,
        input.dates = file.dates
      )
    } else {
      message(
        'bottom_temp_daily_epu_',
        run.years[y],
        ' already exists but input files have changed. Overwriting with new data. No registry Change'
      )
    }
  }

  #Make thermal_habitat_gridded----
  print(paste0(
    'Starting thermal_habitat_gridded for year: ',
    run.years[y],
    ' at ',
    Sys.time()
  ))

  th.grid.prefix <- 'GLORYS_thermal_habitat_gridded_'
  th.grid.file <- paste0(
    out.dir,
    '/thermal_habitat_gridded/',
    th.grid.prefix,
    run.years[y],
    '.nc'
  )
  check.dir(th.grid.file)

  th.grid.check <- check.date.file(
    registry.file,
    dataset.name = paste0('th_grid_', run.years[y]),
    input.dates = file.dates
  )

  if (th.grid.check$file.exists & th.grid.check$same.input.date) {
    message(
      'thermal_habitat_gridded_',
      run.years[y],
      ' already exists and is up to date. Skipping.'
    )
    th.grid.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('bt_anom_', run.years[y])
    )
  } else if (!th.grid.check$same.input.date | !th.grid.check$file.exists) {
    th.grid.df <- EDAB.GLORYS::make_thermal_habitat_gridded(
      input.file = this.year.files,
      output.file = th.grid.file,
      supp.dir = supp.dir,
      shp.file = shp.file,
      file.year = run.years[y],
      write.out = F,
      t.max.seq = seq(0, 1, 1)
    )

    write.csv(th.grid.df, th.grid.file, row.names = F)
    if (!th.grid.check$same.run.date) {
      append.registry(
        dataset.name = paste0('th_grid_', run.years[y]),
        file.name = th.grid.file,
        registry.file = registry.file,
        input.dates = file.dates
      )
    } else {
      message(
        'thermal_habitat_gridded_',
        run.years[y],
        ' already exists but input files have changed. Overwriting with new data. No registry Change'
      )
    }
  }

  #Make thermal_habitat_area----
  print(paste0(
    'Starting thermal_habitat_area for year: ',
    run.years[y],
    ' at ',
    Sys.time()
  ))

  th.area.prefix <- 'GLORYS_thermal_habitat_area_'
  th.grid.csv.prefix <- 'GLORYS_thermal_habitat_gridded_'
  th.area.file <- paste0(
    out.dir,
    '/thermal_habitat_area/',
    th.area.prefix,
    run.years[y],
    '.csv'
  )
  th.area.grid.csv.file <- paste0(
    out.dir,
    '/thermal_habitat_gridded/',
    th.grid.csv.prefix,
    run.years[y],
    '.csv'
  )
  check.dir(th.area.file)
  check.dir(th.area.grid.csv.file)

  th.area.check <- check.date.file(
    registry.file,
    dataset.name = paste0('th_area_', run.years[y]),
    input.dates = file.dates
  )

  if (th.area.check$file.exists & th.area.check$same.input.date) {
    message(
      'thermal_habitat_area_',
      run.years[y],
      ' already exists and is up to date. Skipping.'
    )
    th.area.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('th_area_', run.years[y])
    )
    th.area.grid.csv.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('th_grid_csv_', run.years[y])
    )
  } else if (!th.area.check$same.input.date | !th.area.check$file.exists) {
    th.area.df <- EDAB.GLORYS::make_thermal_habitat_area(
      input.file = this.year.files,
      output.file.area = th.area.file,
      output.file.gridded = th.area.grid.csv.file,
      shp.file = shp.file,
      file.year = run.years[y],
      write.area = F,
      write.gridded = F,
      t.max.seq = seq(0, 1, 1)
    )

    write.csv(th.area.df$thermal.area, th.area.file, row.names = F)
    write.csv(th.area.df$thermal.gridded, th.area.grid.csv.file, row.names = F)

    if (!th.area.check$same.run.date) {
      append.registry(
        dataset.name = paste0('th_area_', run.years[y]),
        file.name = th.area.file,
        registry.file = registry.file,
        input.dates = file.dates
      )
      append.registry(
        dataset.name = paste0('th_grid_csv_', run.years[y]),
        file.name = th.area.grid.csv.file,
        registry.file = registry.file,
        input.dates = file.dates
      )
    } else {
      message(
        'thermal_habitat_area_',
        run.years[y],
        ' already exists but input files have changed. Overwriting with new data. No registry Change'
      )
    }
  }

  #Make cold_pool_raw----
  print(paste0(
    'Starting cold_pool data for year: ',
    run.years[y],
    ' at ',
    Sys.time()
  ))

  cp.raw.prefix <- 'GLORYS_cold_pool_raw_'

  cp.raw.file <- paste0(
    out.dir,
    '/cold_pool_raw/',
    cp.raw.prefix,
    run.years[y],
    '.csv'
  )
  cp.raw.nc.file <- paste0(
    out.dir,
    '/cold_pool_raw/',
    cp.raw.prefix,
    run.years[y],
    '.nc'
  )
  check.dir(cp.raw.file)

  cp.raw.check <- check.date.file(
    registry.file,
    dataset.name = paste0('coldpool_raw_', run.years[y]),
    input.dates = file.dates
  )

  if (cp.raw.check$file.exists & cp.raw.check$same.input.date) {
    message(
      'cold_pool_raw_',
      run.years[y],
      ' already exists and is up to date. Skipping.'
    )
    cp.raw.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('coldpool_raw_', run.years[y])
    )
    cp.raw.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('coldpool_raw_nc_', run.years[y])
    )
    flag.cold.pool <- F
  } else if (!cp.raw.check$same.input.date | !cp.raw.check$file.exists) {
    cp.raw.df <- EDAB.GLORYS::make_cold_pool_data(
      input.files = this.year.files,
      output.dir = paste0(out.dir, '/cold_pool_raw/'),
      output.prefix = 'glorys_cold_pool_',
      cp.shp.file = paste0(supp.dir, 'GLORYS/cold_pool_area_GLORYS.shp'),
      write.out = F
    )

    write.csv(cp.raw.df$data.frame, cp.raw.file, row.names = F)
    terra::writeCDF(
      cp.raw.df$data.nc[[1]],
      varname = 'BottomT',
      filename = cp.raw.nc.file,
      overwrite = T
    )

    flag.cold.pool <- T
    if (!cp.raw.check$same.run.date) {
      append.registry(
        dataset.name = paste0('coldpool_raw_', run.years[y]),
        file.name = cp.raw.file,
        registry.file = registry.file,
        input.dates = file.dates
      )

      append.registry(
        dataset.name = paste0('coldpool_raw_nc', run.years[y]),
        file.name = cp.raw.nc.file,
        registry.file = registry.file,
        input.dates = file.dates
      )
    } else {
      message(
        'cold_pool_raw_',
        run.years[y],
        ' already exists but input files have changed. Overwriting with new data. No registry Change'
      )
    }
  }
}

#Make cold pool index----

if (flag.cold.pool) {
  message("Cold Pool files changed. Rerunning spatial extent and indices")
  cp.raw.files <- list.files(
    path = paste0(out.dir, '/cold_pool_raw/'),
    pattern = '^GLORYS_cold_pool_raw_.*\\.csv$',
    full.names = T
  )

  glorys.cp.data <- do.call(rbind, lapply(cp.raw.files, read.csv)) |>
    dplyr::mutate(source = 'GLORYS')
  roms.cp.data <- readRDS(here::here(
    'data',
    'cold_pool',
    'ROMS',
    'roms_debiased_cold_pool_monthly_1959_1992.rds'
  )) |>
    dplyr::mutate(
      source = 'ROMS',
      year = as.numeric(year),
      month = as.numeric(month),
      cell = as.numeric(cell)
    )

  cp.data.all <- dplyr::bind_rows(glorys.cp.data, roms.cp.data)

  cp.ind.dir <- paste0(out.dir, '/cold_pool_indices/')
  if (!dir.exists(cp.ind.dir)) {
    dir.create(cp.ind.dir)
  }

  cp.ext.res <- EDAB.GLORYS::make_cold_pool_extent(
    input.data = cp.data.all,
    output.dir = cp.ind.dir,
    shp.file = paste0(supp.dir, 'GLORYS/cold_pool_area_GLORYS.shp'),
    grid.file = paste0(
      supp.dir,
      'workflow_resources/statics/roms_cold_pool_grid.nc'
    )
  )

  cp.ind.res <- EDAB.GLORYS::make_cold_pool_indices(
    input.data = cp.data.all,
    cp.extent.file = cp.ext.res$max_extent_file,
    output.dir = cp.ind.dir
  )

  #Add to registry
  append.registry(
    dataset.name = paste0('coldpool_max_ext_'),
    file.name = cp.ext.res$max_extent_file,
    registry.file = registry.file,
    input.dates = input.files.date
  )

  append.registry(
    dataset.name = paste0('coldpool_annual_ext_'),
    file.name = cp.ext.res$annual_extent_file,
    registry.file = registry.file,
    input.dates = input.files.date
  )

  append.registry(
    dataset.name = paste0('coldpool_index_'),
    file.name = cp.ind.res$cp.index.file,
    registry.file = registry.file,
    input.dates = input.files.date
  )
}

#Consolidate outputs into EDAB Indicators----

soe_combine <- function(
  output.dataset,
  registry.file,
  soe_dir,
  output.prefix,
  roms.file = NA
) {
  if (!dir.exists(soe_dir)) {
    dir.create(soe_dir)
  }
  current.registry <- read.csv(registry.file)

  this.dataset.files <- current.registry |>
    dplyr::filter(grepl(output.dataset, dataset.name)) |>
    dplyr::pull(dataset.name) |>
    unique()

  recent.dataset.files <- sapply(this.dataset.files, function(x) {
    get_recent_file(registry.file, dataset.name = x)
  })

  recent.dataset.df <- lapply(recent.dataset.files, read.csv) |>
    dplyr::bind_rows()

  if (!is.na(roms.file)) {
    if (file.exists(roms.file)) {
      roms.df <- read.csv(roms.file)
      recent.dataset.df <- dplyr::bind_rows(recent.dataset.df, roms.df)
    }
  }
  data.dates <- as.Date(recent.dataset.df$date)

  start.date <- min(data.dates)
  end.date <- max(data.dates)

  out.name <- paste0(
    output.prefix,
    '_',
    format(start.date, '%Y%m%d'),
    '_',
    format(end.date, '%Y%m%d'),
    '.csv'
  )

  write.csv(recent.dataset.df, file.path(soe_dir, out.name), row.names = F)
}


#bottom_temp_daily_epu
soe_combine(
  output.dataset <- 'bt_epu',
  registry.file,
  soe_dir,
  output.prefix = 'GLORYS_bottom_temp_daily_epu',
  roms.file = file.path(
    supp.dir,
    'static_workflow_inputs',
    'ROMS_daily_epu_1959_1992.csv'
  )
)


#bottom_temp_model_annual
soe_combine(
  output.dataset <- 'bt_annual',
  registry.file,
  soe_dir,
  output.prefix = 'GLORYS_bottom_temp_model_annual'
)


#bottom_temp_model_anom
soe_combine(
  output.dataset <- 'bt_anom',
  registry.file,
  soe_dir,
  output.prefix = 'GLORYS_bottom_temp_model_anom'
)

#bottom_temp_gridded
soe_combine(
  output.dataset <- 'bt_gridded',
  registry.file,
  soe_dir,
  output.prefix = 'GLORYS_bottom_temp_model_gridded'
)

#thermal_habitat_area
soe_combine(
  output.dataset <- 'th_area',
  registry.file,
  soe_dir,
  output.prefix = 'GLORYS_thermal_habitat_area'
)

#thermal_habitat_gridded
soe_combine(
  output.dataset <- 'th_grid_csv',
  registry.file,
  soe_dir,
  output.prefix = 'GLORYS_thermal_habitat_gridded'
)
