#Initial Data
#Type R.Data
load("D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/fire_modeling/qb_sdm_ot_5_fin/qb_sdm_ot_6.RData")

#Input Req: data kovariat (raster) untuk detil raster bisa dicek pada script update_comp_lyr dibawah
# raster stack
comp_lyr <- stack(raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_dem.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_dist_def_log.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_dist_hph_log.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_dist_hti_log.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_dist_kebun_log.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_dist_set_log.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_dist_trans_log.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_distkanal_log.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_distpeat_log.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_distriver_log.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_distroad_log.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_eco.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_kaw.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_LC.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_pop.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_prec_dry.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_prec_warm.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_temp_dry.tif'),
                  raster('D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/Sum_temp_warm.tif'))

sdm_dir <- "SDMod"
sdm_dir <- "D:/GGP/Jambi/Fire_modeling/_Tes" #temp
if(!dir.exists(sdm_dir))dir.create(sdm_dir)
setwd(sdm_dir)

# hotspot data
pres_dat <- readOGR("D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Hotspot/HS_Jambi2017_UTM48S.shp", "HS_Jambi2017_UTM48S")
bg <- readOGR("D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Hotspot/hotspot_ran8.shp", "hotspot_ran8")
# covariate layers
cov_dir <- "D:/GGP/Jambi/Fire_modeling/GGP_Fire_Jambi/Maxent_model/"









# set target paths
urlAddressRaster='D:/GG_Papua/process/lumens_dir/IDH/raster/' # ./30m or ./100m
urlAddressTabular='D:/GG_Papua/process/lumens_dir/IDH/table/'
urlAddressVector='D:/GG_Papua/process/lumens_dir/IDH/Vector/'

analyzedLocation='Papua'
iteration=5

#Handler to change temporal resolution
iterationAnnual=6