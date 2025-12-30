#' Formats data for cold pool analysis
#'
#' Refactored to accept either annual NetCDF files (with daily layers)
#' or vectors of daily NetCDF files. Outputs remain annual.
#'
#' @param input.files Vector of file paths. Can be a mix of annual files or daily files.
#' @param output.dir Full output file directory
#' @param output.prefix string. Prefix for the output files
#' @param cp.shp.file Full input file directory for the cold pool area shape file
#' @param redo logical. If FALSE, skips processing for years where output .nc already exists.
#' 
#' @return RDS file with the processed monthly data for all years
#' 
#' @importFrom magrittr "|>"
#' @importFrom terra rast time vect crop mask writeCDF as.data.frame
#' @importFrom dplyr mutate group_by summarise bind_rows
#' @importFrom tidyr gather
#' 
#' @export

make_cold_pool_data = function(input.files, output.dir, output.prefix, cp.shp.file, redo = FALSE) {
  
  # 1. Setup
  message("Loading Shapefile...")
  cp.shp = terra::vect(cp.shp.file)
  
  data.df.ls = list()
  
  # 2. Inventory Input Files
  # We must determine which files belong to which year before processing.
  # UPDATED: We now parse filenames instead of reading file headers for speed.
  message("Scanning input filenames to group by year...")
  
  file_inventory <- lapply(input.files, function(f) {
    fname <- basename(f)
    yr <- NULL
    
    # Logic: 
    # 1. Check for specific daily format (YYYY-MM-DD) first. 
    #    This prevents issues if filenames have other 4-digit numbers (like versions).
    # 2. If not found, look for a standalone 4-digit year.
    
    daily_match <- regexpr("(\\d{4})-\\d{2}-\\d{2}", fname)
    
    if (daily_match != -1) {
      # Found YYYY-MM-DD, extract the YYYY part
      full_date_str <- regmatches(fname, daily_match)
      yr <- substr(full_date_str, 1, 4)
    } else {
      # Fallback: Look for the first 4-digit sequence (Annual file)
      year_match <- regexpr("\\d{4}", fname)
      if (year_match != -1) {
        yr <- regmatches(fname, year_match)
      } else {
        warning(paste("Could not determine year from filename:", fname))
        return(NULL)
      }
    }
    
    return(data.frame(file = f, year = yr, stringsAsFactors = FALSE))
  })
  
  # Combine and group
  inventory_df <- dplyr::bind_rows(file_inventory)
  
  if (nrow(inventory_df) == 0) stop("No valid input files with time metadata found.")
  
  # Split files by year
  files_by_year <- split(inventory_df$file, inventory_df$year)
  years <- names(files_by_year)
  
  # 3. Process Per Year
  for (yr in years) {
    
    # Identify target output file for this year
    output_nc_file <- paste0(output.dir, output.prefix, yr, '.nc')
    files_current_year <- files_by_year[[yr]]
    
    # Initialize the object to hold the cropped data
    data.crop <- NULL
    
    # --- CHECK REDO / EXISTENCE ---
    if (redo == FALSE && file.exists(output_nc_file)) {
      message(paste0("Output exists for ", yr, ". Skipping processing and loading existing NC..."))
      
      # We load the existing processed NC so we can still calculate the monthly means 
      # for the final RDS file without re-doing the expensive crop/mask.
      data.crop <- terra::rast(output_nc_file)
      
    } else {
      # --- PROCESS FROM RAW INPUTS ---
      message(paste0('Processing Year: ', yr, ' (Inputs: ', length(files_current_year), ' files)'))
      
      # Load all files for this year. 
      # If multiple files (daily), terra::rast() stacks them.
      # If one file (annual), terra::rast() loads the layers.
      r_stack <- terra::rast(files_current_year)
      
      # Ensure time order (critical if daily files were passed unsorted)
      r_times <- terra::time(r_stack)
      if (is.unsorted(r_times)) {
        r_stack <- r_stack[[order(r_times)]]
      }
      
      # Crop and Mask
      data.crop <- r_stack |> 
        terra::crop(cp.shp) |> 
        terra::mask(cp.shp)
      
      # Ensure names are dates (helps with debugging/checking)
      names(data.crop) <- terra::time(data.crop)
      
      # Write Annual NetCDF
      terra::writeCDF(data.crop, 
                      varname = 'BottomT', 
                      filename = output_nc_file, 
                      overwrite = TRUE)
    }
    
    # --- CALCULATE MONTHLY MEANS FOR RDS ---
    # Convert raster to dataframe and aggregate
    # Note: We do this even if we skipped processing, using the loaded data.crop
    
    message(paste0("Calculating monthly means for ", yr, "..."))
    
    df_chunk <- terra::as.data.frame(data.crop, cell = TRUE)  |> 
      tidyr::gather(date, value, -cell) |>
      dplyr::mutate(date = as.Date(date),
                    month = format(date, "%m"),
                    year = format(date, '%Y')) |>
      dplyr::group_by(year, month, cell) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')
    
    data.df.ls[[yr]] <- df_chunk
  }
  
  # 4. Finalize
  message("Binding all years and saving RDS...")
  glorys.df = dplyr::bind_rows(data.df.ls)
  
  # Determine year range for filename based on actual data
  start_year <- min(glorys.df$year)
  end_year <- max(glorys.df$year)
  
  saveRDS(glorys.df, paste0(output.dir, output.prefix, 'monthly_', start_year, '_', end_year, '.rds'))
  
  message("Done.")
}