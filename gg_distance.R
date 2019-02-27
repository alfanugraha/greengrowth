lusimInitialLCFile <- paste0(ggRasterDirectory, "lc2015_NAsync.tif")
lusimFinalLCFile <- paste0(ggRasterDirectory, "lc_GG_2018.tif")

library(spatialtoolbox)


ggLookupZone <- read.table(paste0(ggTableDirectory, 'pu_lookup_f.csv'), header = T, sep = ",", stringsAsFactors = F)

ggLanduse1 <- raster(lusimInitialLCFile)
ggLanduse2 <- raster(lusimFinalLCFile)
ggZone <- raster(lusimPlanningUnitFile)

# ggLanduseLength <- nrow(lusimLookupLC)
# ggZoneLength <- nrow(ggLookupPlanningUnit)
# 
# dummy1 <- data.frame(nPU=ggLookupZone$ID, divider=ggLanduseLength*ggLanduseLength)
# dummy1 <- expandRows(dummy1, 'divider')
# 
# dummy2 <- data.frame(nT1=lusimLookupLC$ID, divider=ggLanduseLength)
# dummy2 <- expandRows(dummy2, 'divider')
# dummy2 <- data.frame(nT1=rep(dummy2$nT1, ggZoneLength))
# 
# dummy3 <- data.frame(nT2=rep(rep(lusimLookupLC$ID, ggLanduseLength), ggZoneLength))
# 
# landUseChangeMapDummy <- cbind(dummy1, dummy2, dummy3)
# colnames(landUseChangeMapDummy) <- c('ZONE', 'ID_LC1', 'ID_LC2')

ggChangeMap <- (ggLanduse1*1) + (ggLanduse2*100^1) 
ggChangesDatabase <- as.data.frame(freq(ggChangeMap))
ggChangesDatabase <- na.omit(ggChangesDatabase)
n <- 2
k <- 0
ggChangesDatabase$value_temp <- ggChangesDatabase$value
while(k < n) {
  eval(parse(text=(paste("ggChangesDatabase$Var", n-k, " <- ggChangesDatabase$value_temp %% 100", sep=""))))  
  ggChangesDatabase$value_temp <- floor(ggChangesDatabase$value_temp/100)
  k=k+1
}
ggChangesDatabase$value_temp <- NULL
colnames(ggChangesDatabase) = c('ID_CHG', 'COUNT', 'ID_LC1', 'ID_LC2')
writeRaster(ggChangeMap, paste0(lusimSimulation, '/changeMap_', ggInitialYear, '_', ggFinalYear, '.tif'))

lusimLookupLC$CLS <- 1
lusimMaskData <- reclassify(ggLanduse1, lusimLookupLC[, c("ID", "CLS")])

# FOR JAMBI
# !24 -> 24 d_conv_infrastructure
# reclassify
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_infrastructure <- ifelse(ID_LC1 != 24 & ID_LC2 == 24, 1, NA) })
lusimDistanceToInfrastructure <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_infrastructure")])
lusimDistanceToInfrastructureFile <- paste0(lusimSimulation, '/d_conv_infrastructure_', ggInitialYear, '_', ggFinalYear, '.tif')
# create distance
lusimDistanceToInfrastructureTemp <- proximity(lusimDistanceToInfrastructure, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToInfrastructureTemp <- spatial_sync_raster(lusimDistanceToInfrastructureTemp, ggZone, method="ngb")
lusimDistanceToInfrastructureTemp <- lusimDistanceToInfrastructureTemp * lusimMaskData
writeRaster(lusimDistanceToInfrastructureTemp, lusimDistanceToInfrastructureFile) 
rm(lusimDistanceToInfrastructure, lusimDistanceToInfrastructureTemp)

# !8 -> 8 d_conv_pulp
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_pulp <- ifelse(ID_LC1 != 8 & ID_LC2 == 8, 1, NA) })
lusimDistanceToPulp <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_pulp")])
lusimDistanceToPulpFile <- paste0(lusimSimulation, '/d_conv_pulp_', ggInitialYear, '_', ggFinalYear, '.tif')
# create distance
lusimDistanceToPulpTemp <- proximity(lusimDistanceToPulp, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToPulpTemp <- spatial_sync_raster(lusimDistanceToPulpTemp, ggZone, method="ngb")
lusimDistanceToPulpTemp <- lusimDistanceToPulpTemp * lusimMaskData
writeRaster(lusimDistanceToPulpTemp, lusimDistanceToPulpFile)  
rm(lusimDistanceToPulp, lusimDistanceToPulpTemp)

# !11,!12 -> 11, 12 d_conv_rubber
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_rubber <- ifelse((ID_LC1 !=  11 & ID_LC2 == 11) | (ID_LC1 !=  12 & ID_LC2 == 12), 1, NA) })
lusimDistanceToRubber <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_rubber")])
lusimDistanceToRubberFile <- paste0(lusimSimulation, '/d_conv_rubber_', ggInitialYear, '_', ggFinalYear, '.tif')
# create distance
lusimDistanceToRubberTemp <- proximity(lusimDistanceToRubber, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToRubberTemp <- spatial_sync_raster(lusimDistanceToRubberTemp, ggZone, method="ngb")
lusimDistanceToRubberTemp <- lusimDistanceToRubberTemp * lusimMaskData
writeRaster(lusimDistanceToRubberTemp, lusimDistanceToRubberFile)  
rm(lusimDistanceToRubber, lusimDistanceToRubberTemp)

# !10 -> 10 d_conv_coffee
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_coffee <- ifelse(ID_LC1 != 10 & ID_LC2 == 10, 1, NA) })
lusimDistanceToCoffee <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_coffee")])
lusimDistanceToCoffeeFile <- paste0(lusimSimulation, '/d_conv_coffee_', ggInitialYear, '_', ggFinalYear, '.tif')
# create distance
lusimDistanceToCoffeeTemp <- proximity(lusimDistanceToCoffee, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToCoffeeTemp <- spatial_sync_raster(lusimDistanceToCoffeeTemp, ggZone, method="ngb")
lusimDistanceToCoffeeTemp <- lusimDistanceToCoffeeTemp * lusimMaskData
writeRaster(lusimDistanceToCoffeeTemp, lusimDistanceToCoffeeFile)  
rm(lusimDistanceToCoffee, lusimDistanceToCoffeeTemp)

# !13 -> 13 d_conv_oilpalm
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_oilpalm <- ifelse(ID_LC1 != 13 & ID_LC2 == 13, 1, NA) })
lusimDistanceToOilPalm <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_oilpalm")])
lusimDistanceToOilPalmFile <- paste0(lusimSimulation, '/d_conv_oilpalm_', ggInitialYear, '_', ggFinalYear, '.tif')
# create distance
lusimDistanceToOilPalmTemp <- proximity(lusimDistanceToOilPalm, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToOilPalmTemp <- spatial_sync_raster(lusimDistanceToOilPalmTemp, ggZone, method="ngb")
lusimDistanceToOilPalmTemp <- lusimDistanceToOilPalmTemp * lusimMaskData
writeRaster(lusimDistanceToOilPalmTemp, lusimDistanceToOilPalmFile)  
rm(lusimDistanceToOilPalm, lusimDistanceToOilPalmTemp)

# !19 -> 19 d_conv_rice
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_rice <- ifelse(ID_LC1 != 19 & ID_LC2 == 19, 1, NA) })
lusimDistanceToRice <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_rice")])
lusimDistanceToRiceFile <- paste0(lusimSimulation, '/d_conv_rice_', ggInitialYear, '_', ggFinalYear, '.tif') 
# create distance
lusimDistanceToRiceTemp <- proximity(lusimDistanceToRice, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToRiceTemp <- spatial_sync_raster(lusimDistanceToRiceTemp, ggZone, method="ngb")
lusimDistanceToRiceTemp <- lusimDistanceToRiceTemp * lusimMaskData
writeRaster(lusimDistanceToRiceTemp, lusimDistanceToRiceFile)  
rm(lusimDistanceToRice, lusimDistanceToRiceTemp)

# !1-7 -> 1-7 d_defor
ggChangesDatabase <- within(ggChangesDatabase, { d_defor <- ifelse(!ID_LC2 %in% c(1:7) & ID_LC1 %in% c(1:7), 1, NA) })
lusimDistanceToDeforestation <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_defor")])
lusimDistanceToDeforestationFile <- paste0(lusimSimulation, '/d_defor_', ggInitialYear, '_', ggFinalYear, '.tif')
# create distance
lusimDistanceToDeforestationTemp <- proximity(lusimDistanceToDeforestation, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToDeforestationTemp <- spatial_sync_raster(lusimDistanceToDeforestationTemp, ggZone, method="ngb")
lusimDistanceToDeforestationTemp <- lusimDistanceToDeforestationTemp * lusimMaskData
writeRaster(lusimDistanceToDeforestationTemp, lusimDistanceToDeforestationFile)  
rm(lusimDistanceToDeforestation, lusimDistanceToDeforestationTemp)

# FOR PAPUA
# !8 -> 8 dis_aca
lusimChangesDatabase <- within(lusimChangesDatabase, { dis_aca <- ifelse(ID_LC1 != 8 & ID_LC2 == 8, 1, NA) })
lusimDistanceToAcacia <- reclassify(lusimChangeMap, lusimChangesDatabase[, c("ID_CHG", "dis_aca")])
lusimDistanceToAcaciaFile <- paste0(lusimSimulationStepFactorsFolder, '/factor_dis_aca.tif')
# create distance
lusimDistanceToAcaciaTemp <- proximity(lusimDistanceToAcacia, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToAcaciaTemp <- spatial_sync_raster(lusimDistanceToAcaciaTemp, lusimZone, method="ngb")
lusimDistanceToAcaciaTemp <- lusimDistanceToAcaciaTemp * lusimMaskData
writeRaster(lusimDistanceToAcaciaTemp, lusimDistanceToAcaciaFile, format="GTiff", overwrite=T)  
rm(lusimDistanceToAcacia, lusimDistanceToAcaciaTemp)

# !9 -> 9 dis_rub
lusimChangesDatabase <- within(lusimChangesDatabase, { dis_rub <- ifelse(ID_LC1 !=  9 & ID_LC2 == 9, 1, NA) })
lusimDistanceToRubber <- reclassify(lusimChangeMap, lusimChangesDatabase[, c("ID_CHG", "dis_rub")])
lusimDistanceToRubberFile <- paste0(lusimSimulationStepFactorsFolder, '/factor_dis_rub.tif')
# create distance
lusimDistanceToRubberTemp <- proximity(lusimDistanceToRubber, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToRubberTemp <- spatial_sync_raster(lusimDistanceToRubberTemp, lusimZone, method="ngb")
lusimDistanceToRubberTemp <- lusimDistanceToRubberTemp * lusimMaskData
writeRaster(lusimDistanceToRubberTemp, lusimDistanceToRubberFile, format="GTiff", overwrite=T)  
rm(lusimDistanceToRubber, lusimDistanceToRubberTemp)

# !10 -> 10 dis_op
lusimChangesDatabase <- within(lusimChangesDatabase, { dis_op <- ifelse(ID_LC1 != 10 & ID_LC2 == 10, 1, NA) })
lusimDistanceToOilPalm <- reclassify(lusimChangeMap, lusimChangesDatabase[, c("ID_CHG", "dis_op")])
lusimDistanceToOilPalmFile <- paste0(lusimSimulationStepFactorsFolder, '/factor_dis_op.tif')
# create distance
lusimDistanceToOilPalmTemp <- proximity(lusimDistanceToOilPalm, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToOilPalmTemp <- spatial_sync_raster(lusimDistanceToOilPalmTemp, lusimZone, method="ngb")
lusimDistanceToOilPalmTemp <- lusimDistanceToOilPalmTemp * lusimMaskData
writeRaster(lusimDistanceToOilPalmTemp, lusimDistanceToOilPalmFile, format="GTiff", overwrite=T)  
rm(lusimDistanceToOilPalm, lusimDistanceToOilPalmTemp)

# !1-6 -> 1-6 dis_def
lusimChangesDatabase <- within(lusimChangesDatabase, { dis_def <- ifelse(!ID_LC2 %in% c(1:6) & ID_LC1 %in% c(1:6), 1, NA) })
lusimDistanceToDeforestation <- reclassify(lusimChangeMap, lusimChangesDatabase[, c("ID_CHG", "dis_def")])
lusimDistanceToDeforestationFile <- paste0(lusimSimulationStepFactorsFolder, '/factor_dis_def.tif')
# create distance
lusimDistanceToDeforestationTemp <- proximity(lusimDistanceToDeforestation, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToDeforestationTemp <- spatial_sync_raster(lusimDistanceToDeforestationTemp, lusimZone, method="ngb")
lusimDistanceToDeforestationTemp <- lusimDistanceToDeforestationTemp * lusimMaskData
writeRaster(lusimDistanceToDeforestationTemp, lusimDistanceToDeforestationFile, format="GTiff", overwrite=T)  
rm(lusimDistanceToDeforestation, lusimDistanceToDeforestationTemp)

# 1-2, 3-4, 5-6 dis_deg
lusimChangesDatabase <- within(lusimChangesDatabase, { dis_deg <- ifelse((ID_LC1 ==  1 & ID_LC2 == 2) | (ID_LC1 ==  3 & ID_LC2 == 4) | (ID_LC1 ==  5 & ID_LC2 == 6), 1, NA) })
lusimDistanceToDegradation <- reclassify(lusimChangeMap, lusimChangesDatabase[, c("ID_CHG", "dis_deg")])
lusimDistanceToDegradationFile <- paste0(lusimSimulationStepFactorsFolder, '/factor_dis_deg.tif')
# create distance
lusimDistanceToDegradationTemp <- proximity(lusimDistanceToDegradation, values = 1, in_meters = TRUE)
# extract by mask
lusimDistanceToDegradationTemp <- spatial_sync_raster(lusimDistanceToDegradationTemp, lusimZone, method="ngb")
lusimDistanceToDegradationTemp <- lusimDistanceToDegradationTemp * lusimMaskData
writeRaster(lusimDistanceToDegradationTemp, lusimDistanceToDegradationFile, format="GTiff", overwrite=T)  
rm(lusimDistanceToDegradation, lusimDistanceToDegradationTemp)





