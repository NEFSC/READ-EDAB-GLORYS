#GLORYS files

input.files = list.files('/home/jcaracappa/EDAB_Datasets/GLORYS/glorys_bottomT/cmems_mod_glo_phy_my_0.083deg_P1D-m/bottomT/',include.dirs = F, full.names =T)
input.files.short = list.files('/home/jcaracappa/EDAB_Datasets/GLORYS/glorys_bottomT/cmems_mod_glo_phy_my_0.083deg_P1D-m/bottomT/',include.dirs = F, full.names =F)
input.files.date = sub(".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", input.files.short)
output.dir = '/home/jcaracappa/EDAB_Datasets/GLORYS/glorys_bottomT/cmems_mod_glo_phy_my_0.083deg_P1D-m/bottomT_NEUS/'
output.prefix = 'GLORYS_REANALYSIS_DAILY_cmems_mod_glo_phy_my_0.083deg_P1D-m_bottomT_NEUS_'
output.names = paste0(output.prefix,input.files.date,'.nc')
output.files.now = list.files(output.dir,full.names = F, include.dirs = F)

which.missing = which(!(output.names %in% output.files.now))
input.files.todo = input.files[which.missing]

EDAB.GLORYS::make_bottom_temp_daily_NEUS(input.file = input.files.todo,
                                        output.dir = output.dir,
                                        output.prefix = output.prefix,
                                        write.out =T,
                                        shp.file = '/home/jcaracappa/EDAB_Resources/workflow_resources/shapefiles/EPU_NOESTUARIES.shp'
                                        )
