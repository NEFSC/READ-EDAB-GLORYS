#saves a crop of GLORYS bathymetry to NES


bathy.shp = terra::rast(here::here('data-raw','GLORYS','GLO-MFC_001_024_mask_bathy.nc'),subds = 'deptho')

min.x = -85
max.x = -60
min.y = 25
max.y = 48

bathy.crop = terra::crop(bathy.shp,terra::ext(min.x,max.x,min.y,max.y))
terra::plot(bathy.crop)

terra::writeCDF(bathy.crop,here::here('data-raw','GLORYS','GLORYS_bathymetry_east_coast_crop.nc'))
