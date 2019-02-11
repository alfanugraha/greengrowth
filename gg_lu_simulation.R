#CALCULTE TRANSITION####
##proj.file=string
##landuse_1=string
##landuse_2=string
##planning_unit=string
##statusoutput=output table

data_dir<-"D:/GGP_LamDong/Data/Raster/"
result_dir<-"D:/GGP_LamDong/Simulation"
lu1_file<-paste0(data_dir, "lc2010.tif")
lu2_file<-paste0(data_dir, "lc2015.tif")
pu_file<-paste0(data_dir, "pu_final.tif")
T1<-2005
T2<-2010


library(spatial.tools)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(XML)
dir.create(result_dir)
transition_dir <- paste(result_dir,"/1_transition", sep="")
dir.create(transition_dir)
setwd(transition_dir)

urlAddressRaster <- data_dir
urlEgoml <- result_dir
timestep <- T2-T1

DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles"), "\\Dinamica EGO\\DinamicaConsole.exe")
if (file.exists(DINAMICA_exe)){
  urlDINAMICAConsole = DINAMICA_exe
} else{
  DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles(x86)"), "\\Dinamica EGO\\DinamicaConsole.exe")
  urlDINAMICAConsole = DINAMICA_exe
}

# DETERMINE TRANSITION MATRIX   
setwd(urlEgoml)
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', pu_file, '"', sep=''))
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', lu1_file, '"', sep=''))
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', lu2_file, '"', sep=''))
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
con$addTag("inputport", attrs=c(name="timeSteps"), timestep)
con$addTag("outputport", attrs=c(name="singleStepMatrix", id="v8"))
con$addTag("outputport", attrs=c(name="multiStepMatrix", id="v9"))
con$closeTag("functor")

# add subtag functor for SaveTable
con$addTag("functor", attrs=c(name="SaveTable"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveTable567"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Single-step transition matrix."))
con$addTag("inputport", attrs=c(name="table", peerid="v8"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', transition_dir, '/Single_step.csv"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

# add subtag functor for SaveTable
con$addTag("functor", attrs=c(name="SaveTable"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveTable566"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Multi-step transition matrix."))
con$addTag("inputport", attrs=c(name="table", peerid="v9"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', transition_dir, '/Multi_step.csv"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

con$closeTag("containerfunctor")  

saveXML(con$value(), file=paste(urlEgoml, "/1_Transition_Matrix_per_Region.egoml", sep=''))
command<-paste('"', urlDINAMICAConsole, '" -processors 0 -log-level 4 "', urlEgoml, '/1_Transition_Matrix_per_Region.egoml"', sep="")
system(command)


#RASTER CUBE####
##proj.file=string
##SCIENDO_LUCM_index=string
##factor_folder=string
##statusoutput=output table

# INPUT
SCIENDO_LUCM_index <- "BAU_LUCM" 
result_dir <- "D:/GGP_LamDong/Simulation"
data_dir <- "D:/GGP_LamDong/Data/Raster"
factor_folder <- "D:/GGP_LamDong/Data/Factor/"


library(spatial.tools)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(XML)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#=Set working directory
setwd(result_dir)
factor_dir <- (paste(result_dir,"/2_raster_cube", sep="")) #ADinput
dir.create(factor_dir)
urlAddressRaster <- factor_folder

# preparing factors
listFactors <- list.files(urlAddressRaster, full.names=TRUE, pattern=".tif$")
editorFactors<-data.frame(file=listFactors, select=1)

selectedFactors<-subset(editorFactors, select==1)
factors<-as.character(selectedFactors$file)
nFactors <- length(factors)

aliasFactor<-NULL
for (a in 1:nFactors) {
  paste(eval(parse(text=(paste("temp<-substr(basename(factors[", a, "]), 1, nchar(basename(factors[", a, "])) - 4)", sep="")))))
  aliasFactor<-c(aliasFactor,temp)
}

DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles"), "\\Dinamica EGO\\DinamicaConsole.exe")
if (file.exists(DINAMICA_exe)){
  urlDINAMICAConsole = DINAMICA_exe
} else{
  DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles(x86)"), "\\Dinamica EGO\\DinamicaConsole.exe")
  urlDINAMICAConsole = DINAMICA_exe
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', factor_dir, '/sciendo_factor.ers"', sep=''))
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
  con$addTag("property", attrs=c(key="dff.functor.alias", value=aliasFactor[b]))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', factors[b], '"', sep=""))
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
  con$addTag("property", attrs=c(key="dff.functor.alias", value=aliasFactor[c]))
  con$addTag("inputport", attrs=c(name="map", peerid=paste("v", c, sep="")))
  con$addTag("inputport", attrs=c(name="mapName"), paste('"', aliasFactor[c], '"', sep=""))
  con$addTag("inputport", attrs=c(name="mapNumber"), 0)
  con$closeTag("functor")
}
con$closeTag("containerfunctor")
# end.

# print(con$value())
# write egoml
saveXML(con$value(), file=paste(result_dir, "/2_Create_Raster_Cube.egoml", sep=''))
command<-paste('"', urlDINAMICAConsole, '" -processors 0 -log-level 4 "', result_dir, '/2_Create_Raster_Cube.egoml"', sep="")
system(command)
save(list = c("aliasFactor", "SCIENDO_LUCM_index", "SCIENDO_folder", "result_dir"), file = paste0(result_dir, "/passFrom1.RData"))# to be passed on to the next step
       # 1. aliasFactor: vector of the names of the component of the raster cube
       # 2. SCIENDO_LUCM_index
       # 3. SCIENDO_folder
       # 4. result_dir


#WOE####
##proj.file=string
##SCIENDO_LUCM_index=string
##lc_lut=string
##statusoutput=output table


result_dir <- "D:/GGP_LamDong/Simulation"
load(paste0(result_dir, "/passFrom1.RData"))

data_dir <- "D:/GGP_LamDong/Data/Raster"
lu1_file <- paste0(data_dir, "lc2010.tif")
lu2_file <- paste0(data_dir, "lc2015.tif")
pu_file <- paste0(data_dir, "pu_final.tif")
lusim_lc <- "D:/GGP_LamDong/Data/Table/LamDong_classes.csv" 
lusim_lc <- read.table(lusim_lc, header = T, sep = ";", stringsAsFactors = F)


library(spatial.tools)
library(splitstackshape)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(XML)

setwd(result_dir)
# defining rastercube location
factor_dir <- (paste(result_dir,"/factor1_final/", sep="")) # ADinput /factor1/ for calibration
DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles"), "\\Dinamica EGO\\DinamicaConsole.exe")
if (file.exists(DINAMICA_exe)){
  urlDINAMICAConsole = DINAMICA_exe
} else{
  DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles(x86)"), "\\Dinamica EGO\\DinamicaConsole.exe")
  urlDINAMICAConsole = DINAMICA_exe
}

########################################################################################################################
# CALCULATE WEIGHT OF EVIDENCE                                                                                         #
########################################################################################################################

static_var<-data.frame(aliasFactor)
static_var$identifier<-paste('&quot;static_var/', static_var$aliasFactor, '&quot; 10 500000 1 5,&#x0A;', sep='')

identifier<-do.call(paste, c(as.list(static_var$identifier), sep="        "))

start <- as.numeric(lusim_lc[1,1])
lenght <- as.numeric(nrow(lusim_lc))
end <- as.numeric(lusim_lc[lenght,1])

skeleton1<-data.frame(nT1=c(start:end), divider=lenght)
skeleton1<-expandRows(skeleton1, 'divider')
skeleton2<-data.frame(nT2=rep(rep(c(start:end), lenght)))

skeleton<-cbind(skeleton1, skeleton2)
skeleton$key<-do.call(paste, c(skeleton[c("nT1", "nT2")], sep = "-&gt;"))

skeleton$transition<-paste("&#x0A;    ", skeleton$key, " [&#x0A;        ", identifier, "    ]", sep='')

skeletonFinal<-do.call(paste, c(as.list(skeleton$transition), sep=","))
skeletonFinal<-paste('[', skeletonFinal, "&#x0A;]", sep='')

# begin writing tag
con <- xmlOutputDOM(tag="script")
# add property
con$addTag("property", attrs=c(key="dff.date", value="2016-Oct-18 12:59:40"))
con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))

# begin.
# add functor = LoadCategoricalMap
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', l2_file, '"', sep=''))
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', l1_file, '"', sep=''))
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', factor_dir, 'sciendo_factor.ers"', sep=''))
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', pu_file, '"', sep=''))
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', factor_dir, '/woe.dcf"', sep=''))
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', factor_dir, '/weight_report.csv"', sep=''))
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
con$addTag("inputport", attrs=c(name="skeleton"), skeletonFinal)
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

# print(con$value())
# write egoml
egoml_file=paste(result_dir, "/3_Weight_of_Evidence_per_Region.egoml", sep='')
saveXML(con$value(), file=egoml_file)

# replace ampersand code character
egoml_text  <- readLines(egoml_file)
egoml_text_new  <- gsub(pattern="amp;", replace="", x=egoml_text)
writeLines(egoml_text_new, con=egoml_file)

command<-paste('"', urlDINAMICAConsole, '" -processors 0 -log-level 4 "', result_dir, '/3_Weight_of_Evidence_per_Region.egoml"', sep="")

system(command)

#SIMULATION####
##proj.file=string
##SCIENDO_LUCM_index=string
##n_rep = number 4
##statusoutput=output table

library(spatial.tools)
library(splitstackshape)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(XML)

n_rep <- 7 #ADinput


# Input directories====
raster_dir <- "D:/GG_Jambi/General/data/spatial/raster/"
factor_dir <- (paste(result_dir,"/factor2_final/", sep="")) #note that in projection, factor2 is used instead
raster_sink <- paste0(result_dir, "/result")
rate_dir_raw <- "D:/GG_Jambi/process/lumens_dir/SCIENDO/BAU_LUCM/transition_1112/" # INPUT newly defined variable; later will be defined as rate_dir #ADCHECK
rate_dir <- paste0(result_dir, "/transition_filter1412") # containing filtered rate #ADCHECK
if(!dir.exists(rate_dir))dir.create(rate_dir)
dir.create(raster_sink)
# Input files==== # ADadd
file_lookup_transition <- "D:/GG_Jambi/General/data/table/allow_transition.csv"
file_init_lc <- paste0(raster_dir, "landcover_1012/adjustedNA/lc2015_NAsync.tif")
pu_file <- paste0(raster_dir, "pu_Jambi1012/plan_unit_jambi_final.tif")
id_oilpalm <- 13 #ADCHECK
adj_coef <- 1 #ADCHECK


# Load transition lookup to determine allowed transitions
lookup_transition <- read.csv(file_lookup_transition, stringsAsFactors = FALSE, header = FALSE) # first two columns contain information on "ID" and "class name". No header
subLookup_trans <- lookup_transition[, 3:ncol(lookup_transition)]
# static_var<-data.frame(aliasFactor)
# static_var$identifier<-paste('&quot;static_var/', static_var$aliasFactor, '&quot; 10 500000 1 5,&#x0A;', sep='')
# 
# identifier<-do.call(paste, c(as.list(static_var$identifier), sep="        "))


# Rate filtering mechanism====
zone_rates <- list.files(rate_dir_raw, pattern = "^Single", recursive = FALSE, full.names = TRUE)
for(z in 1:length(zone_rates)){
  # creating z_id variable to be pasted in the saving process
  if(z < 10) z_id <- paste0("00000", z) else z_id <- paste0("0000", z)
  # reading the raw rate table
  z_rate <- read.csv(zone_rates[z], stringsAsFactors = FALSE)
  # loop to filter rates not allowed
  if(nrow(z_rate) > 0){# add handler for zero transition rate
  z_rate$allow <- 0
  for(rr in 1: nrow(z_rate)){
    if(eval(parse(text= paste0("subLookup_trans[", (z_rate[rr, 1]+1), ", ", (z_rate[rr, 2]+1), "] == 1")))) z_rate[rr, "allow"] <- 1
    if(z_rate[rr, 2] == id_oilpalm) z_rate[rr, 3] <- adj_coef * z_rate[rr, 3]#ADCHECK
  }
  z_rate <- z_rate[z_rate$allow==1, 1:4]
  }
  names(z_rate) <- c("From*", "To*", "Rate", "")
  write.table(z_rate, file= paste0(rate_dir, "/Single_step", z_id, ".csv"), row.names = FALSE, quote = FALSE, na = "", sep = ", ")
}



# skeleton builder====
lusim_lc <- "D:/GG_Jambi/General/data/table/lut_lu.csv" #ADinput
lusim_lc <- read.csv(lusim_lc, stringsAsFactors = FALSE)# a data.frame which contains the columns: "ID", "COUNT", "Legend", and "Classified"

start <- as.numeric(lusim_lc[1,1])
lenght <- as.numeric(nrow(lusim_lc))
end <- as.numeric(lusim_lc[lenght,1])

skeleton1<-data.frame(nT1=c(start:end), divider=lenght)
skeleton1<-expandRows(skeleton1, 'divider')
skeleton2<-data.frame(nT2=rep(rep(c(start:end), lenght)))

skeleton<-cbind(skeleton1, skeleton2)
skeleton <- skeleton[skeleton$nT1 != skeleton$nT2, ]
# AD add filter here====
skeleton$allow <- 0
for(rs in 1:nrow(skeleton)){
  if(eval(parse(text= paste0("subLookup_trans[", (skeleton[rs, "nT1"]+1), ", ", (skeleton[rs, "nT2"]+1), "] == 1")))) skeleton[rs, "allow"] <- 1
}
skeleton <- skeleton[skeleton$allow==1, c("nT1", "nT2")]
# rebuild the chunk
skeleton$char <- paste(skeleton$nT1, skeleton$nT2, sep = "-&gt;")
skeleton$char_fx <- paste0(skeleton$char, " 0.3,&#x0A;")
skeleton[nrow(skeleton), "char_fx"] <- gsub("3,&", "3&", skeleton[nrow(skeleton), "char_fx"])
txt_skl <- paste(skeleton$char_fx, collapse = "    ")
txt_skl2 <- gsub("0.3", "2 1 1", txt_skl)
txt_skl3 <- gsub("2 1 1", "1 1 1", txt_skl2)
# ADHERE
# skeleton$key<-do.call(paste, c(skeleton[c("nT1", "nT2")], sep = "-&gt;"))
# 
# skeleton$transition<-paste("&#x0A;    ", skeleton$key, " &#x0A;        ", sep='')
# 
# skeletonFinal<-do.call(paste, c(as.list(skeleton$transition), sep=","))
# skeletonFinal<-paste('[', skeletonFinal, "&#x0A;]", sep='')

# Variable list====
file_ers <- "sciendo_factor.ers"
# file_init_lc <- "/landuse_2.tif"
# zone <- "/zone.tif"
DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles"), "\\Dinamica EGO\\DinamicaConsole.exe")
if (file.exists(DINAMICA_exe)){
  urlDINAMICAConsole = DINAMICA_exe
} else{
  DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles(x86)"), "\\Dinamica EGO\\DinamicaConsole.exe")
  urlDINAMICAConsole = DINAMICA_exe
}


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
con$addTag("inputport", attrs=c(name="filename"), paste('"', factor_dir, file_ers, '"', sep=''))
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
con$addTag("inputport", attrs=c(name="filename"), paste0('"', file_init_lc, '"')) #ADedit
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
con$addTag("inputport", attrs=c(name="filename"), paste0('"', pu_file, '"')) # ADedit
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
con$addTag("inputport", attrs=c(name="iterations"), n_rep)
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', raster_sink, '/Landscape.tif"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 4)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

# add functor = SaveMap
con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap3414"))
con$addTag("inputport", attrs=c(name="map", peerid="v16"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', raster_sink, '/Probabilities.tif"', sep=''))
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
con$addTag("inputport", attrs=c(name="filename"), paste('"', rate_dir, '/Single_step.csv"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v7"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="table", id="v9"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="LoadWeights"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Weights of Evidence Coefficients"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Load Weights of Evidence coefficients."))
con$addTag("inputport", attrs=c(name="filename"), paste('"', factor_dir, '/WoE.dcf"', sep=''))
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
con$addTag("inputport", attrs=c(name="percentOfTransitionsByExpansion"), paste('[&#x0A;    ', txt_skl, ']', sep=''))
con$addTag("inputport", attrs=c(name="patchExpansionParameters"), paste('[&#x0A;    ', txt_skl2, ']', sep=''))
con$addTag("inputport", attrs=c(name="patchGenerationParameters"), paste('[&#x0A;    ', txt_skl3, ']', sep=''))
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
con$addTag("inputport", attrs=c(name="transitions"), paste('[ ', paste(skeleton$char, collapse = ", "), ']', sep=''))
con$addTag("inputport", attrs=c(name="cellType"), ".uint8")
con$addTag("inputport", attrs=c(name="nullValue"), ".default")
con$addTag("outputport", attrs=c(name="probabilities", id="v14"))

con$addTag("functor", attrs=c(name="NameMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="nameMap298"))
con$addTag("inputport", attrs=c(name="map", peerid="v12"))
con$addTag("inputport", attrs=c(name="mapName"), paste('"static_var"', sep=''))
con$closeTag("functor")

con$closeTag("containerfunctor") #    CalcWOfEProbabilityMap

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

con$closeTag("containerfunctor")  # Repeat
con$closeTag("containerfunctor") # RegionManager
# end.

# write egoml
egoml_file=paste(result_dir, "/4_Simulation_per_Regions.egoml", sep='')
saveXML(con$value(), file=egoml_file)

# replace ampersand code character
egoml_text  <- readLines(egoml_file)
egoml_text_new  <- gsub(pattern="amp;", replace="", x=egoml_text)
writeLines(egoml_text_new, con=egoml_file)

command<-paste('"', urlDINAMICAConsole, '" -processors 0 -log-level 4 "', result_dir, '/4_Simulation_per_Regions.egoml"', sep="")

system(command)


