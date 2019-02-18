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

# reclassify
# !24 -> 24 d_conv_infrastructure
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_infrastructure <- ifelse(ID_LC1 != 24 & ID_LC2 == 24, ID_CHG, NA) })
ggDistanceToInfrastructure <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_infrastructure")])
writeRaster(ggDistanceToInfrastructure, paste0(lusimSimulation, '/d_conv_infrastructure_', ggInitialYear, '_', ggFinalYear, '.tif'))  

# !8 -> 8 d_conv_pulp
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_pulp <- ifelse(ID_LC1 != 8 & ID_LC2 == 8, ID_CHG, NA) })
ggDistanceToPulp <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_pulp")])
writeRaster(ggDistanceToPulp, paste0(lusimSimulation, '/d_conv_pulp_', ggInitialYear, '_', ggFinalYear, '.tif'))  

# !11,!12 -> 11, 12 d_conv_rubber
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_rubber <- ifelse((ID_LC1 !=  11 & ID_LC2 == 11) | (ID_LC1 !=  12 & ID_LC2 == 12), ID_CHG, NA) })
ggDistanceToRubber <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_rubber")])
writeRaster(ggDistanceToRubber, paste0(lusimSimulation, '/d_conv_rubber_', ggInitialYear, '_', ggFinalYear, '.tif'))  

# !10 -> 10 d_conv_coffee
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_coffee <- ifelse(ID_LC1 != 10 & ID_LC2 == 10, ID_CHG, NA) })
ggDistanceToCoffee <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_coffee")])
writeRaster(ggDistanceToCoffee, paste0(lusimSimulation, '/d_conv_coffee_', ggInitialYear, '_', ggFinalYear, '.tif'))  

# !13 -> 13 d_conv_oilpalm
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_oilpalm <- ifelse(ID_LC1 != 13 & ID_LC2 == 13, ID_CHG, NA) })
ggDistanceToOilPalm <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_oilpalm")])
writeRaster(ggDistanceToOilPalm, paste0(lusimSimulation, '/d_conv_oilpalm_', ggInitialYear, '_', ggFinalYear, '.tif'))  

# !19 -> 19 d_conv_rice
ggChangesDatabase <- within(ggChangesDatabase, { d_conv_rice <- ifelse(ID_LC1 != 19 & ID_LC2 == 19, ID_CHG, NA) })
ggDistanceToRice <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_conv_rice")])
writeRaster(ggDistanceToRice, paste0(lusimSimulation, '/d_conv_rice_', ggInitialYear, '_', ggFinalYear, '.tif'))  

# !1-7 -> 1-7 d_defor
ggChangesDatabase <- within(ggChangesDatabase, { d_defor <- ifelse(!ID_LC2 %in% c(1:7) & ID_LC1 %in% c(1:7), ID_CHG, NA) })
ggDistanceToRice <- reclassify(ggChangeMap, ggChangesDatabase[, c("ID_CHG", "d_defor")])
writeRaster(ggDistanceToRice, paste0(lusimSimulation, '/d_defor_', ggInitialYear, '_', ggFinalYear, '.tif'))  



# create distance
proximity(ggDistanceToInfrastructure, values = c(ggChangesDatabase$d_conv_infrastructure), units = 1, in_meters = FALSE, filename = NULL)






