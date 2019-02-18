#######################################################################
## gg_lu_simulation.R                                                 #
## ------------------                                                 #
## Date              : February, 2019                                 #
## Author            : Alfa Nugraha                                   #
## Script purpose    :                                                #                 
##                                                                    #                                             
##                                                                    #
##                                                                    #
##                                                                    #
##                                                                    #
#######################################################################


# Section 1: CALCULATE TRANSITION ####
##########################################################

# set target paths
ggDataDirectory = 'D:/GGP/project_dir/data/'
ggRasterDirectory = paste0(ggDataDirectory, 'raster/')
ggFactorDirectory = paste0(ggRasterDirectory, 'factors/')
ggFactorCategoricalDirectory = paste0(ggFactorDirectory, 'categorical/')
ggFactorContinuousDirectory = paste0(ggFactorDirectory, 'continuous/')
ggTableDirectory = paste0(ggDataDirectory, 'table/')
ggVectorDirectory = paste0(ggDataDirectory, 'vector/')

ggAnalyzedLocation = 'Jambi'

ggInitialYear <- 2015
ggFinalYear <- 2018
ggTimeStep <- ggInitialYear-ggFinalYear

lusimDinamicaIteration = 1
lusimRepetition = 9

lusimResultDirectory <- "D:/GGP/project_dir/data/raster/GG_"
lusimInitialLCFile <- paste0(ggRasterDirectory, "lc2015_NAsync.tif")
lusimFinalLCFile <- paste0(ggRasterDirectory, "lc_GG_2018.tif")
lusimPlanningUnitFile <- paste0(ggRasterDirectory, "plan_unit_jambi_final_f.tif")

dir.create(lusimResultDirectory)
lusimTransition <- paste(lusimResultDirectory,"/1_transition", sep="")
dir.create(lusimTransition)
setwd(lusimTransition)

lusimDinamicaExe <- paste0(Sys.getenv("ProgramFiles"), "\\Dinamica EGO\\DinamicaConsole.exe")
if (file.exists(lusimDinamicaExe)){
  lusimDinamicaConsole = lusimDinamicaExe
} else{
  lusimDinamicaExe<-paste0(Sys.getenv("ProgramFiles(x86)"), "\\Dinamica EGO\\DinamicaConsole.exe")
  lusimDinamicaConsole = lusimDinamicaExe
}

# begin writing tag
con <- xmlOutputDOM(tag="script")
# add property
con$addTag("property", attrs=c(key="dff.date", value="2016-Oct-17 12:02:15"))
con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))

# begin.
# add functor = LoadCategoricalMap-PU
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Regions"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Municipalities"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimPlanningUnitFile, '"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), 0)
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id=paste("v",1,sep="")))
con$closeTag("functor")
# end.

# begin.
# add functor = LoadCategoricalMap-LANDUSE_1
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Initial landscape map"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimInitialLCFile, '"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), 0)
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id=paste("v",2,sep="")))
con$closeTag("functor")
# end.

# begin.
# add functor = LoadCategoricalMap-LANDUSE_2
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Final landscape map"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimFinalLCFile, '"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), 0)
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id=paste("v",3,sep="")))
con$closeTag("functor")
# end.

# begin.
# add containerfunctor = ForEachRegion
con$addTag("containerfunctor", attrs=c(name="ForEachRegion"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="forEachRegion"))
con$addTag("inputport", attrs=c(name="regions", peerid="v3"))
con$addTag("inputport", attrs=c(name="borderCells"), 0)
con$addTag("internaloutputport", attrs=c(name="regionManager", id="v4"))
con$addTag("internaloutputport", attrs=c(name="step", id="v5"))

# add subtag functor for Landuse1
con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v1"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v5"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v6"))
con$closeTag("functor")

# add subtag functor for Landuse2
con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v2"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v5"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v7"))
con$closeTag("functor")

# add subtag functor for DetermineTransitionMatrix
con$addTag("functor", attrs=c(name="DetermineTransitionMatrix"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Transition Rates"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Calculate the transition rates"))
con$addTag("inputport", attrs=c(name="initialLandscape", peerid="v6"))
con$addTag("inputport", attrs=c(name="finalLandscape", peerid="v7"))
con$addTag("inputport", attrs=c(name="timeSteps"), ggTimeStep)
con$addTag("outputport", attrs=c(name="singleStepMatrix", id="v8"))
con$addTag("outputport", attrs=c(name="multiStepMatrix", id="v9"))
con$closeTag("functor")

# add subtag functor for SaveTable
con$addTag("functor", attrs=c(name="SaveTable"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveTable567"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Single-step transition matrix."))
con$addTag("inputport", attrs=c(name="table", peerid="v8"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimTransition, '/Single_step.csv"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

# add subtag functor for SaveTable
con$addTag("functor", attrs=c(name="SaveTable"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveTable566"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Multi-step transition matrix."))
con$addTag("inputport", attrs=c(name="table", peerid="v9"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimTransition, '/Multi_step.csv"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

con$closeTag("containerfunctor")  

saveXML(con$value(), file=paste(lusimTransition, "/1_transition_matrix_per_region.egoml", sep=''))
lusimCommand<-paste('"', lusimDinamicaConsole, '" -processors 0 -log-level 4 "', lusimTransition, '/1_transition_matrix_per_region.egoml"', sep="")
system(lusimCommand)

# Section 2: RASTER CUBE ####
##########################################################

lusimRasterCube <- paste(lusimResultDirectory,"/2_raster_cube", sep="")
dir.create(lusimRasterCube)
setwd(lusimRasterCube)

# preparing factors
lusimListCategoricalFactor <- list.files(ggFactorContinuousDirectory, full.names=TRUE, pattern=".tif$")
lusimListContinuousFactor <- list.files(ggFactorCategoricalDirectory, full.names=TRUE, pattern=".tif$")
lusimListFactors <- c(lusimListContinuousFactor, lusimListCategoricalFactor)

nFactors <- length(lusimListFactors)

lusimAliasFactor <- NULL
for (a in 1:nFactors) {
  paste(eval(parse(text=(paste("lusimTemp <- substr(basename(lusimListFactors[", a, "]), 1, nchar(basename(lusimListFactors[", a, "])) - 4)", sep="")))))
  lusimAliasFactor <- c(lusimAliasFactor, lusimTemp)
}

# begin writing tag
con <- xmlOutputDOM(tag="script")
# add property
con$addTag("property", attrs=c(key="dff.date", value="2016-Oct-17 12:02:15"))
con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))

# begin.
# add functor = SaveMap
con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap1680"))
con$addTag("inputport", attrs=c(name="map", peerid=paste("v", nFactors+1,sep="")))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimRasterCube, '/raster_cube.ers"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 2)
con$addTag("inputport", attrs=c(name="step"), ".none")
con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")
# end.

# begin.
# add functor = LoadMap
for (b in 1:nFactors){
  con$addTag("functor", attrs=c(name="LoadMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value=lusimAliasFactor[b]))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimListFactors[b], '"', sep=""))
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), ".none")
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id=paste("v",b,sep="")))
  con$closeTag("functor") 
}
# end.

# begin.
# add containerfunctor = CreateCubeMap
con$addTag("containerfunctor", attrs=c(name="CreateCubeMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="createCubeMap1678"))
con$addTag("inputport", attrs=c(name="cellType"), ".float32")
con$addTag("inputport", attrs=c(name="nullValue"), "-9999")
con$addTag("outputport", attrs=c(name="map", id=paste("v", nFactors+1, sep="")))
# add subtag functor for CreateCubeMap
for (c in 1:nFactors) {
  con$addTag("functor", attrs=c(name="NumberAndNameMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value=lusimAliasFactor[c]))
  con$addTag("inputport", attrs=c(name="map", peerid=paste("v", c, sep="")))
  con$addTag("inputport", attrs=c(name="mapName"), paste('"', lusimAliasFactor[c], '"', sep=""))
  con$addTag("inputport", attrs=c(name="mapNumber"), 0)
  con$closeTag("functor")
}
con$closeTag("containerfunctor")
# end.

# write egoml
saveXML(con$value(), file=paste(lusimRasterCube, "/2_create_raster_cube.egoml", sep=''))
lusimCommand <- paste('"', lusimDinamicaConsole, '" -processors 0 -log-level 4 "', lusimRasterCube, '/2_create_raster_cube.egoml"', sep="")
system(lusimCommand)

# Section 3: WEIGHT OF EVIDENCE ####
##########################################################

lusimWOE <- paste(lusimResultDirectory,"/3_woe", sep="")
dir.create(lusimWOE)
setwd(lusimWOE)

lusimLookupLC <- paste0(ggTableDirectory, 'lut_lc_f.csv')
lusimLookupLC <- read.table(lusimLookupLC, header = T, sep = ",", stringsAsFactors = F)

lusimStaticVar <- data.frame(lusimAliasFactor)
lusimStaticVar$identifier <- paste('&quot;static_var/', lusimStaticVar$lusimAliasFactor, '&quot; 10 500000 1 5,&#x0A;', sep='')

identifier <- do.call(paste, c(as.list(lusimStaticVar$identifier), sep="        "))

lusimStart <- as.numeric(lusimLookupLC[1, 1])
lusimLength <- as.numeric(nrow(lusimLookupLC))
lusimEnd <- as.numeric(lusimLookupLC[lusimLength, 1])

lusimSkeleton1 <- data.frame(nT1=c(lusimStart:lusimEnd), divider=lusimLength)
lusimSkeleton1 <- expandRows(lusimSkeleton1, 'divider')
lusimSkeleton2 <- data.frame(nT2=rep(rep(c(lusimStart:lusimEnd), lusimLength)))

lusimSkeleton <- cbind(lusimSkeleton1, lusimSkeleton2)
lusimSkeleton$key <- do.call(paste, c(lusimSkeleton[c("nT1", "nT2")], sep = "-&gt;"))

lusimSkeleton$transition <- paste("&#x0A;    ", lusimSkeleton$key, " [&#x0A;        ", identifier, "    ]", sep='')

lusimSkeletonFinal <- do.call(paste, c(as.list(lusimSkeleton$transition), sep=","))
lusimSkeletonFinal <- paste('[', lusimSkeletonFinal, "&#x0A;]", sep='')

# begin writing tag
con <- xmlOutputDOM(tag="script")
# add property
con$addTag("property", attrs=c(key="dff.date", value="2016-Oct-18 12:59:40"))
con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))

# begin.
# add functor = LoadCategoricalMap
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimFinalLCFile, '"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), "0")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id="v1"))
con$closeTag("functor")
# end.

# begin.
# add functor = LoadCategoricalMap
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimInitialLCFile, '"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), "0")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id="v2"))
con$closeTag("functor")
# end.

# begin.
# add functor = LoadMap
con$addTag("functor", attrs=c(name="LoadMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Static Variables"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimRasterCube, '/raster_cube.ers"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), "0")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id="v3"))
con$closeTag("functor") 
# end.

# begin.
# add functor = LoadCategoricalMap
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Regions"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimPlanningUnitFile, '"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), "0")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id="v4"))
con$closeTag("functor")
# end.
# begin.

# begin.
# add containerfunctor = ForEachRegion
con$addTag("containerfunctor", attrs=c(name="ForEachRegion"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="forEachRegion"))
con$addTag("inputport", attrs=c(name="regions", peerid="v4"))
con$addTag("inputport", attrs=c(name="borderCells"), 0)
con$addTag("internaloutputport", attrs=c(name="regionManager", id="v5"))
con$addTag("internaloutputport", attrs=c(name="step", id="v6"))

# add subtag functor for SaveWeights
con$addTag("functor", attrs=c(name="SaveWeights"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveWeights"))
con$addTag("inputport", attrs=c(name="weights", peerid="v10"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimWOE, '/woe.dcf"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v6"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

# add subtag functor for RegionalizeCategoricalMap
con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v1"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v6"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v5"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v7"))
con$closeTag("functor")

# add subtag functor for RegionalizeCategoricalMap
con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v2"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v6"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v5"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v8"))
con$closeTag("functor")

# add subtag functor for RegionalizeMap
con$addTag("functor", attrs=c(name="RegionalizeMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Static Variables (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v3"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v6"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v5"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v9"))
con$closeTag("functor")

# add subtag functor for SaveTable
con$addTag("functor", attrs=c(name="SaveTable"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveTable"))
con$addTag("inputport", attrs=c(name="table", peerid="v11"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimWOE, '/weight_report.csv"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 2)
con$addTag("inputport", attrs=c(name="step", peerid="v6"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

# add subtag functor for DetermineWeightsOfEvidenceCoefficients
con$addTag("containerfunctor", attrs=c(name="DetermineWeightsOfEvidenceCoefficients"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Weight of Evidence Coefficients"))
con$addTag("inputport", attrs=c(name="initialLandscape", peerid="v8"))
con$addTag("inputport", attrs=c(name="finalLandscape", peerid="v7"))
con$addTag("inputport", attrs=c(name="ranges", peerid="v12"))
con$addTag("inputport", attrs=c(name="fixAbnormalWeights"), ".no")
con$addTag("outputport", attrs=c(name="weights", id="v10"))
con$addTag("outputport", attrs=c(name="report", id="v11"))

con$addTag("functor", attrs=c(name="NameMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="nameMapCoeff"))
con$addTag("inputport", attrs=c(name="map", peerid="v9"))
con$addTag("inputport", attrs=c(name="mapName"), '"static_var"')
con$closeTag("functor")

con$closeTag("containerfunctor")  

# add subtag functor for DetermineWeightsOfEvidenceRanges
con$addTag("containerfunctor", attrs=c(name="DetermineWeightsOfEvidenceRanges"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Weight of Evidence Ranges"))
con$addTag("inputport", attrs=c(name="initialLandscape", peerid="v8"))
con$addTag("inputport", attrs=c(name="finalLandscape", peerid="v7"))
con$addTag("inputport", attrs=c(name="skeleton"), lusimSkeletonFinal)
con$addTag("inputport", attrs=c(name="fixAbnormalWeights"), ".no")
con$addTag("outputport", attrs=c(name="ranges", id="v12"))

con$addTag("functor", attrs=c(name="NameMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="nameMapRanges"))
con$addTag("inputport", attrs=c(name="map", peerid="v9"))
con$addTag("inputport", attrs=c(name="mapName"), '"static_var"')
con$closeTag("functor")

con$closeTag("containerfunctor")
con$closeTag("containerfunctor")
# end.

# write egoml
lusimEgomlWOE <- paste(lusimWOE, "/3_woe_per_region_.egoml", sep='')
saveXML(con$value(), file=lusimEgomlWOE)
# replace ampersand code character
lusimEgomlWOEText <- readLines(lusimEgomlWOE)
lusimEgomlWOETextNew <- gsub(pattern='&amp;', replace='&', x=lusimEgomlWOEText)
writeLines(lusimEgomlWOETextNew, con=lusimEgomlWOE)
lusimCommand <- paste('"', lusimDinamicaConsole, '" -processors 0 -log-level 4 "', lusimEgomlWOE, "'", sep="")
system(lusimCommand)


# Section 4: SIMULATION ####
##########################################################

lusimSimulation <- paste(lusimResultDirectory,"/4_simulation", sep="")
dir.create(lusimSimulation)
setwd(lusimSimulation)

lusimSkeletonAllocateTransition <- subset(lusimSkeleton, select=c(nT1, nT2))
lusimSkeletonAllocateTransition <- lusimSkeletonAllocateTransition[lusimSkeletonAllocateTransition$nT1 != lusimSkeletonAllocateTransition$nT2, ]

lusimSkeletonAllocateTransition$char <- paste(lusimSkeletonAllocateTransition$nT1, lusimSkeletonAllocateTransition$nT2, sep = "-&gt;")
lusimSkeletonAllocateTransition$char_fx <- paste0(lusimSkeletonAllocateTransition$char, " 0.3,&#x0A;")
lusimSkeletonAllocateTransition[nrow(lusimSkeletonAllocateTransition), "char_fx"] <- gsub("3,&", "3&", lusimSkeletonAllocateTransition[nrow(lusimSkeletonAllocateTransition), "char_fx"])
lusimTextSkeleton1 <- paste(lusimSkeletonAllocateTransition$char_fx, collapse = "    ")
lusimTextSkeleton2 <- gsub("0.3", "2 1 1", lusimTextSkeleton1)
lusimTextSkeleton3 <- gsub("2 1 1", "1 1 1", lusimTextSkeleton2)

# begin writing tag
con <- xmlOutputDOM(tag="script")
# add property
con$addTag("property", attrs=c(key="dff.date", value="2016-Nov-09 17:01:03"))
con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))

# begin.
# add functor = LoadMap
con$addTag("functor", attrs=c(name="LoadMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Static Variables"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Static variable maps."))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimRasterCube, '/raster_cube.ers"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), "0")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id="v1"))
con$closeTag("functor") 
# end.

# begin.
# add functor = LoadCategoricalMap
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Initial landscape maps."))
con$addTag("inputport", attrs=c(name="filename"), paste0('"', lusimFinalLCFile, '"')) 
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), "0")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id="v2"))
con$closeTag("functor")
# end.

# begin.
# add functor = LoadCategoricalMap
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Municipalities"))
con$addTag("inputport", attrs=c(name="filename"), paste0('"', lusimPlanningUnitFile, '"')) # ADedit
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), "0")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id="v3"))
con$closeTag("functor")
# end.

# begin.
# add containerfunctor = ForEachRegion
con$addTag("containerfunctor", attrs=c(name="RegionManager"), close=FALSE)
con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
con$addTag("property", attrs=c(key="dff.functor.alias", value="regionManager3260"))
con$addTag("inputport", attrs=c(name="regions", peerid="v3"))
con$addTag("inputport", attrs=c(name="borderCells"), 0)
con$addTag("internaloutputport", attrs=c(name="regionManager", id="v4"))

# add containerfunctor = Repeat
con$addTag("containerfunctor", attrs=c(name="Repeat"), close=FALSE)
con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
con$addTag("property", attrs=c(key="dff.functor.alias", value="repeat279"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Simulation model."))
con$addTag("inputport", attrs=c(name="iterations"), ggRepetition)
con$addTag("internaloutputport", attrs=c(name="step", id="v5"))

# add functor = LoadCategoricalMap
con$addTag("functor", attrs=c(name="MuxCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Landscape"))
con$addTag("inputport", attrs=c(name="initial", peerid="v2"))
con$addTag("inputport", attrs=c(name="feedback", peerid="v15"))
con$addTag("outputport", attrs=c(name="map", id="v6"))
con$closeTag("functor")

# add functor = SaveMap
con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap282"))
con$addTag("inputport", attrs=c(name="map", peerid="v15"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimSimulation, '/Landscape.tif"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 4)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

# add functor = SaveMap
con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap3414"))
con$addTag("inputport", attrs=c(name="map", peerid="v16"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimSimulation, '/Probabilities.tif"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 4)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

# add containerfunctor = ForEachCategory
con$addTag("containerfunctor", attrs=c(name="ForEachCategory"), close=FALSE)
con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
con$addTag("property", attrs=c(key="dff.functor.alias", value="forEachCategory283"))
con$addTag("inputport", attrs=c(name="categorization", peerid="v3"))
con$addTag("internaloutputport", attrs=c(name="step", id="v7"))

con$addTag("functor", attrs=c(name="IntegerValue"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="int290"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="This operator is used here to force a dependence between two groups."))
con$addTag("inputport", attrs=c(name="constant"), 0)
con$addTag("outputport", attrs=c(name="object", id="v8"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="LoadTable"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Transition Matrix"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Load transition matrix."))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimTransition, '/Single_step.csv"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v7"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="table", id="v9"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="LoadWeights"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Weights of Evidence Coefficients"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Load Weights of Evidence coefficients."))
con$addTag("inputport", attrs=c(name="filename"), paste('"', lusimWOE, '/woe.dcf"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v7"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="weights", id="v10"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="RegionalCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="regionalCategoricalMap289"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Assign a map to the region using the given identifier."))
con$addTag("inputport", attrs=c(name="globalMapName"), paste('"landscape"', sep=''))
con$addTag("inputport", attrs=c(name="regionalMap", peerid="v11"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="AllocateTransitions"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Updated Landscape (Region)"))
con$addTag("inputport", attrs=c(name="lanscape", peerid="v13"))
con$addTag("inputport", attrs=c(name="probabilities", peerid="v14"))
con$addTag("inputport", attrs=c(name="transitionMatrix", peerid="v9"))
con$addTag("inputport", attrs=c(name="percentOfTransitionsByExpansion"), paste('[&#x0A;    ', lusimTextSkeleton1, ']', sep=''))
con$addTag("inputport", attrs=c(name="patchExpansionParameters"), paste('[&#x0A;    ', lusimTextSkeleton2, ']', sep=''))
con$addTag("inputport", attrs=c(name="patchGenerationParameters"), paste('[&#x0A;    ', lusimTextSkeleton3, ']', sep=''))
con$addTag("inputport", attrs=c(name="printTransitionInfo"), ".no")
con$addTag("outputport", attrs=c(name="resultingLanscape", id="v11"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="RegionalizeMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Static Variables (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v1"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v12"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Landscape (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v6"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v13"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="RegionalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="regionalMap3412"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Assign a map to the region using the given identifier."))
con$addTag("inputport", attrs=c(name="globalMapName"), paste('"probabilities"', sep=''))
con$addTag("inputport", attrs=c(name="regionalMap", peerid="v14"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$closeTag("functor")

con$addTag("containerfunctor", attrs=c(name="CalcWOfEProbabilityMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
con$addTag("property", attrs=c(key="dff.functor.alias", value="Probabilities (Region)"))
con$addTag("property", attrs=c(key="dff.functor.extendedcomment", value="Calculate probability map."))
con$addTag("inputport", attrs=c(name="landscape", peerid="v13"))
con$addTag("inputport", attrs=c(name="weights", peerid="v10"))
con$addTag("inputport", attrs=c(name="transitions"), paste('[ ', paste(lusimSkeletonAllocateTransition$char, collapse = ", "), ']', sep=''))
con$addTag("inputport", attrs=c(name="cellType"), ".uint8")
con$addTag("inputport", attrs=c(name="nullValue"), ".default")
con$addTag("outputport", attrs=c(name="probabilities", id="v14"))

con$addTag("functor", attrs=c(name="NameMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="nameMap298"))
con$addTag("inputport", attrs=c(name="map", peerid="v12"))
con$addTag("inputport", attrs=c(name="mapName"), paste('"static_var"', sep=''))
con$closeTag("functor")

con$closeTag("containerfunctor") # CalcWOfEProbabilityMap

con$closeTag("containerfunctor") # ForEachCategory

# add containerfunctor = Group
con$addTag("containerfunctor", attrs=c(name="Group"), close=FALSE)
con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
con$addTag("property", attrs=c(key="dff.functor.alias", value="group300"))

con$addTag("functor", attrs=c(name="IntegerValue"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="int302"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="This operator is used here to force a dependence between two groups."))
con$addTag("inputport", attrs=c(name="constant", peerid="v8"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="MergeRegionalCategoricalMaps"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Updated Landscape"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Merge all maps assigned to the regions using the given identifier."))
con$addTag("inputport", attrs=c(name="globalMapName"), paste('"landscape"', sep=''))
con$addTag("inputport", attrs=c(name="mergeNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="globalMap", id="v15"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="MergeRegionalMaps"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="mergeRegionalMaps3413"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Merge all maps assigned to the regions using the given identifier."))
con$addTag("inputport", attrs=c(name="globalMapName"), paste('"probabilities"', sep=''))
con$addTag("inputport", attrs=c(name="mergeNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="globalMap", id="v16"))
con$closeTag("functor")

con$closeTag("containerfunctor") # Group

con$closeTag("containerfunctor") # Repeat
con$closeTag("containerfunctor") # RegionManager
# end.

# write egoml
lusimEgomlSimulation <- paste(lusimSimulation, "/4_simulation_per_regions.egoml", sep='')
saveXML(con$value(), file=lusimEgomlSimulation)
# replace ampersand code character
lusimEgomlSimulationText <- readLines(lusimEgomlSimulation)
lusimEgomlSimulationTextNew <- gsub(pattern='&amp;', replace='&', x=lusimEgomlSimulationText)
writeLines(lusimEgomlSimulationTextNew, con=lusimEgomlSimulation)
lusimCommand <- paste('"', lusimDinamicaConsole, '" -processors 0 -log-level 4 "', lusimEgomlSimulation, "'", sep="")
system(lusimCommand)
