#' Creates the cold pool index, persistence, and extent
#'
#' descriptions
#'
#' @param input.data Full input file directory for bottom temp data returned from make_cold_pool_data
#' @param cp.extent.file Full input file directory for cold pool extent data returned from make_cold_pool_extent
#' @param output.dir Full output file directory
#' 
#' @return csv with cold pool indices
#' 
#' @importFrom magrittr "|>"
#' 
#' @export
#' 

make_cold_pool_indices = function(input.data, cp.extent.file,output.dir){
  
  cp_max_extent = readRDS(cp.extent.file)
  cp.months = 6:9
  
  #Calculate Cold Pool Indices
  bt_temp_time_series_month_cpi = dplyr::filter(input.data, cell %in% unique(cp_max_extent$cell))  |>
    dplyr::mutate(month = as.numeric(month))
  
  year.min = min(bt_temp_time_series_month_cpi$year)
  year.max = max(bt_temp_time_series_month_cpi$year)
  
  # mean over the period 1972-2018 between June and September
  dt_avg_bt<-bt_temp_time_series_month_cpi |>
    dplyr::filter(month %in% cp.months) |>
    dplyr::group_by(cell) |>
    dplyr::summarise(mean_bt_temp=mean(value)) |> #  mean temp over the time period
    as.data.frame()
  
  #Cold Pool Index calculation
  dt_cpi_soe <- bt_temp_time_series_month_cpi |>
    dplyr::filter(month %in% cp.months ) |>
    dplyr::group_by(year,cell) |>
    dplyr::summarise(bt_temp=mean(value)) |>
    as.data.frame() |>
    dplyr::inner_join(dt_avg_bt, by="cell") |>
    dplyr::mutate(res_bottom_t=bt_temp-mean_bt_temp) |>
    dplyr::group_by(year) |>
    dplyr::summarise(cold_pool_index=mean(res_bottom_t),
              sd_cold_pool_index=sd(res_bottom_t),
              mean_bottom_temp=mean(bt_temp),
              nbr_cell=length(res_bottom_t)) |>
    as.data.frame() |>
    dplyr::mutate(se_cold_pool_index=sd_cold_pool_index/sqrt(nbr_cell)) |>
    dplyr::select(year,cold_pool_index,sd_cold_pool_index,se_cold_pool_index,mean_bottom_temp) |>
    dplyr::mutate(type= "Model_CPI")
  
  #Cold Pool Persistence Index calculation
  dt_end_month<-data.frame()
  dt_bt_sep_oct<-dplyr::filter(bt_temp_time_series_month_cpi,month %in% 6:10)
  for (y in sort(unique(dt_bt_sep_oct$year))) { # for each year
    dt_pre_year<-dplyr::filter(dt_bt_sep_oct,year==y) |>
      dplyr::mutate(below = ifelse(value<10, 1, 0))
    # cell where temperature below 10˚ at least once betweem June and October
    dt_cell_cold <- dt_pre_year |> 
      dplyr::group_by(cell) |>
      dplyr::summarise(length_below=sum(below)) |>
      dplyr::filter(length_below!=0) |>
      as.data.frame()  |>
      dplyr::select(cell) 
    # First month when the temperature is below 10˚C 
    dt_start_below <- dplyr::filter(dt_pre_year,cell%in%dt_cell_cold[,1] & below==1) |>
      dplyr::group_by(cell) |>
      dplyr::summarise(start_below=min(month)) |> as.data.frame() 
    # First month when the temperature is below 10˚C 
    dt_end_month_year <- dplyr::filter(dt_pre_year,cell%in%dt_cell_cold[,1]) |>
      dplyr::inner_join(dt_start_below,by="cell") |>
      dplyr::filter((month-start_below)>0) # we keep only the cell where temperature falls below 10˚
    # if temperature raise again above 10˚C
    dt_end_month_year_during <- dplyr::filter(dt_end_month_year,below==0) |> # selection of days where temp > 10˚C
      dplyr::group_by(cell) |>
      dplyr::summarise(end_month=min(month))  # selection the first month where temp > 10˚C
    # if temperature remain below 10˚C
    dt_end_month_year_above <- dplyr::anti_join(dt_end_month_year, dt_end_month_year_during,by="cell") |>
      dplyr::filter(below==1)  
    #
    if (nrow(dt_end_month_year_above)>0) { # selection the first month where temp > 10˚C
      dt_end_month_year_above <- dt_end_month_year_above |> 
        dplyr::group_by(cell) |> 
        dplyr::summarise(end_month=max(month))
    } 
    dt_end_month_year<-rbind(dt_end_month_year_during,dt_end_month_year_above)|>
      dplyr:: mutate(year=y) |> as.data.frame()
    dt_end_month<-rbind(dt_end_month,dt_end_month_year)
  }
  ##Mean Persistence
  mean_end_month<-dt_end_month |>
    dplyr::group_by(cell) |>
    dplyr::summarise(mean_end_month=mean(end_month)) |>
    as.data.frame()
  # residual persistence and index calculation
  dt_persistence_index_soe<-dplyr::inner_join(mean_end_month,dt_end_month,by="cell") |>
    dplyr::mutate(res_month=end_month-mean_end_month) |>
    dplyr::group_by(year) |>
    dplyr::summarise(persistence_index=mean(res_month),
              sd_persistence_index=sd(res_month),
              mean_end_month=mean(end_month),
              count_cell=length(res_month)) |>
    as.data.frame() |>
    dplyr::mutate(se_persistence_index=sd_persistence_index/sqrt(count_cell)) |>
    dplyr::select(year,persistence_index,sd_persistence_index,se_persistence_index,mean_end_month) |>
    dplyr::mutate(type= "Model_PI")
  
  #Calculate spatial extent index
  bt_temp_time_series_month_extent <- dplyr::filter(bt_temp_time_series_month_cpi, month %in% (6:9))|>
    dplyr::rename(bt_temp = 'value')
  dt_bt_ts_mean<-bt_temp_time_series_month_extent |>
    dplyr::group_by(year) |>
    dplyr::summarise(bt_temp=mean(bt_temp)) |>
    as.data.frame()
  # -----------------------------------------------------------
  dt_extent_year<-data.frame()
  for (y in sort(unique(bt_temp_time_series_month_extent$year))) { # for each year
    dt_pre_year<-dplyr::filter(bt_temp_time_series_month_extent,year==y) |>
      dplyr::mutate(below = ifelse(bt_temp<10, 1, 0))
    # cell where monthly temperature below 10˚ at least once between June and September
    dt_cell_cold <- dt_pre_year |> 
      dplyr::group_by(cell) |>
      dplyr::summarise(length_below=sum(below)) |>
      dplyr::filter(length_below>=2) |>
      dplyr::mutate(year=y) |> as.data.frame() |>
      dplyr::select(year, cell)  
    dt_extent_year<-rbind(dt_extent_year,dt_cell_cold)
  }
  dt_extent_year<-dplyr::group_by(dt_extent_year,year) |>
    dplyr::summarise(nbr_cell=length(cell)) |>
    as.data.frame()
  # Mean extent
  mean_nbr_cell<-dt_extent_year |>
    dplyr::summarise(mean_nbr_cell=mean(nbr_cell)) |>
    as.data.frame()
  # spatial extent index calculation
  dt_extent_index<-dt_extent_year |>
    dplyr::mutate(extent_index=nbr_cell-mean_nbr_cell$mean_nbr_cell) 
  # sd of the index
  dt_sd_extent_index<-dt_extent_index |>
   dplyr::summarise(sd_extent_index=sd(extent_index),
              se_extent_index=sd(extent_index)/nrow(dt_extent_index))
  # bind sd and index
  dt_extent_index_soe<-dplyr::mutate(dt_extent_index, sd_extent_index=dt_sd_extent_index$sd_extent_index,
                              se_extent_index=dt_sd_extent_index$se_extent_index) |>
    dplyr::select(year,extent_index,sd_extent_index,se_extent_index) |>
    dplyr::mutate(type= "Model_SEI")
  
  #Combine to cold pool index object
  dt_cp<-dplyr::select(dt_cpi_soe,year,cold_pool_index,se_cold_pool_index)  |>
    dplyr::inner_join(dplyr::select(dt_persistence_index_soe,year,persistence_index,se_persistence_index), by="year") |>
    dplyr::inner_join(dplyr::select(dt_extent_index_soe,year,extent_index,se_extent_index), by="year") |>
    dplyr::select(year,cold_pool_index,se_cold_pool_index,persistence_index,se_persistence_index,extent_index,se_extent_index)
  write.csv(file=paste0(output.dir,"cold_pool_indices_",year.min,"_",year.max,".csv"),x=dt_cp)

  # #Test vs ecodata
  # soe23 = read.csv('C:/Users/joseph.caracappa/Documents/GitHub/bottom_temp_SOE/data/SOE/cold_pool_indices_1959_2023.csv')|>
  #   filter(source %in% c("GLORYS","ROMS")) |>
  #   rename(Time = 'year')|>
  #   arrange(Time)
  # soe24 = ecodata::cold_pool |>
  #   tidyr::spread(Var,Value)|>
  #   filter(Source %in% c("GLORYS","ROMS")) |>
  #   arrange(Time)
  # 
  # 
  # #Index
  # plot(cold_pool_index~year,dt_cp,type='l',ylim =c(-2.5,3))
  # lines(cold_pool_index~Time,soe23,col =2)
  # lines(cold_pool_index~Time,soe24,col=3)
  # abline(h =0)
  # 
  # #Persistence
  # plot(persistence_index~year,dt_cp,type='l',ylim = c(-2,1))
  # lines(persistence_index~Time,soe23,col =2)
  # lines(persistence_index~Time,soe24,col =3)
  # abline(h=0)
  # 
  # #Extent
  # plot(extent_index~year,dt_cp,type='l')
  # lines(extent_index~Time,soe23,col =2)
  # lines(extent_index~Time,soe24,col =3)
  # abline(h=0)
}





