
source(here::here('data-raw','format_ROMS_coldpool.R'))

make_cold_pool_data(input.files = list.files('C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/','GLORYS_daily_BottomTemp_',full.names = T),
                    output.dir = here::here('data','cold_pool','GLORYS','/'),
                    output.prefix = 'glorys_cold_pool_',
                    cp.shp.file = here::here('data-raw','geometry','cold_pool_area_GLORYS.shp')
)

glorys.data = readRDS(here::here('data','cold_pool','GLORYS','glorys_cold_pool_monthly_1993_2024.rds')) %>%
  dplyr::mutate(source = 'GLORYS')
roms.data = readRDS(here::here('data','cold_pool','ROMS','roms_debiased_cold_pool_monthly_1959_1992.rds'))%>%
  dplyr::mutate(source = 'ROMS')
input.data = dplyr::bind_rows(glorys.data,roms.data)
rm(glorys.data,roms.data)

make_cold_pool_extent(input.data = input.data,
                      output.dir = here::here('data','cold_pool','/'),
                      shp.file = here::here('data-raw','geometry','cold_pool_area.shp'),
                      grid.file = here::here('data','cold_pool','roms_cold_pool_grid.nc')
)
make_cold_pool_indices(input.data = input.data,
                       cp.extent.file = here::here('data','cold_pool','cold_pool_maximum_extent_1959_2024.rds'),
                       output.dir = here::here('data','cold_pool','/')
)
