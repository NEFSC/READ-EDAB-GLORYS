#CommandLine Local
# Rscript vignettes/GLORYS_automation_test.R "W:/GLORYS/v12/SOURCE/NES_9KM_DAILY/BOTTOMT/" "Y:/workflow_resources/" "X:/jcaracappa/glorys_my_soe/monthly/" 
#Command Line Cloud
#Rscript //nefscdata/EDAB_Workflows/GLORYS_automation_test.R "//nefscdata/EDAB_Datasets/GLORYS/v12/SOURCE/NES_9KM_DAILY/BOTTOMT/" "//nefscdatac/EDAB_Resources/workflow_resources/" "//nefscdata/EDAB_Dev/jcaracappa/glorys_my_soe/monthly/" 

#Gets arguments from command line
args = commandArgs(trailingOnly = TRUE)
if(length(args)>0){
  print(args)
  input.dir= args[1]
  supp.dir = args[2]
  output.dir = args[3]
  print('Using command line arguments')
}else{
  input.dir = 'W:/GLORYS/v12/SOURCE/NES_9KM_DAILY/BOTTOMT/'
  # input.dir = '/home/jcaracappa/EDAB_Datasets/GLORYS/glorys_bottomT/cmems_mod_glo_phy_my_0.083deg_P1D-m/bottomT_NEUS/'
  output.dir = 'X:/jcaracappa/glorys_my_soe/monthly/'
  # output.dir = '/home/jcaracappa/EDAB_Dev/jcaracappa/glorys_my_soe/'
  supp.dir = 'Y:/workflow_resources/'
  # supp.dir = '/home/jcaracappa/EDAB_Resources/workflow_resources/'

  print('Using default arguments')
}

message(paste0('input.dir: ',input.dir))
message(paste0('write.dir: ',output.dir))
message(paste0('supp.dir: ',supp.dir))

input.prefix = 'GLORYS_REANALYSIS_DAILY_cmems_mod_glo_phy_my_0.083deg_P1D-m_bottomT_'
output.prefix = 'GLORYS_REANALYSIS_DAILY_cmems_mod_glo_phy_my_0.083deg_P1D-m_bottomT_MONTHLY_'

check.dir = function(file){
  if(!dir.exists(dirname(file))){dir.create(dirname(file),recursive =T)}
}

check.dir(output.dir)
if(!dir.exists(input.dir)){stop(paste0('Input directory does not exist: ',input.dir))}
if(!dir.exists(supp.dir)){stop(paste0('Supplemental directory does not exist: ',supp.dir))}

EDAB.GLORYS::make_monthly_gridded(input.dir = input.dir,
                                  input.prefix = input.prefix,
                                  output.dir = output.dir,
                                  output.prefix = output.prefix,
                                  var.name = 'bottomT',
                                  statistics = 'mean')

message('Done: make_monthly_gridded')