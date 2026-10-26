#' Make gridded daily files split by EPU for heatwave index
#'
#' descriptions
#'
#' @param input.files Either a character vector of full input file names for a list of spatrasters
#' @param output.file character vector of full output file names corresponding to each element of strata
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. name of the variable to extract
#' @param strata list. Named list with item name the corresponding column in shape file with vectors of BTS Strata IDs to extract
#' @param statistics character.statistics to extract
#' @param write.out logical. If TRUE, the function will write out the gridded data to a netcdf file
#'
#' @return a dataframe (Time, Latitude, Longitude, var, value) or csv file of the gridded data
#'
#'
#' @export
#'

make_bottom_temp_strata <- function(
  input.files,
  output.file,
  shp.file,
  var.name,
  strata,
  statistics = 'mean',
  write.out = T
) {
  shp <- terra::vect(shp.file)
  shp.df <- terra::as.data.frame(shp)

  strata.names <- names(strata)

  out.ls <- list()

  for (i in 1:length(strata.names)) {
    this.strata <- strata[[i]]
    this.shp <- shp[match(this.strata, shp.df[, strata.names[i]]), ]

    this.data.rast <- EDABUtilities::make_2d_summary_ts(
      data.in = input.files,
      write.out = F,
      shp.file = this.shp,
      var.name = var.name,
      agg.time = 'days',
      statistics = statistics,
      touches = F,
      file.time = 'daily',
      area.names = this.strata
    )

    if (write.out) {
      write.csv(this.data.rast, output.file, row.names = F)
    } else {
      out.ls[[i]] <- this.data.rast
    }
  }

  if (!write.out) {
    out.df <- dply::bind_rows(out.ls)
    return(out.df)
  }
}
