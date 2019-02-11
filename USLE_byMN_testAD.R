#--------------------------------------------------------------------
#                               USLE
#--------------------------------------------------------------------

## define initial file
start<-Sys.time()
working_directory="D:/gg_scripts/git/greengrowth/testing/USLE"
DEM_raw_hyd="data_jambi/raster/DEM_musi.tif"

## Library
library (rgdal)
library (RSAGA)
library (raster)
library (maptools)
library (svDialogs)
library (rgeos)

#--------------------------------------------------------------------
#                                SWAT
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#file path to raw data and result folder
#--------------------------------------------------------------------
#NOTE****
#put folder raw data in document file ("c:/Users/name user/Documents)
#Save script R in those folder as well

#1. Setting up working directory
#set working directory in RSWAT file
Parent<-setwd(working_directory)
#Parent folder using default working directory in R
#AD avoid using the getwd() function to determine parent directory
#create RSWAT folder 
dir.create(paste(Parent, "/RSWAT", sep=""))
#create Watershed delineation directory
dir.create(paste(Parent,"/RSWAT/Watershed_delineation",sep=""))
#create directory for HRU definition
dir.create(paste(Parent,"/RSWAT/HRU",sep=""))
#Working directory in RSWAT folder
wd <- paste0(Parent,"/RSWAT")
#Setting up raw path (raw data path)
raw_path <- dirname("D:/gg_scripts/git/greengrowth/data_jambi/")

#2. Setting up output folder

#2.1 Create output folder in Watershed folder
#result
dir.create(paste(wd,"/Watershed_delineation/Result", sep=""))
#output file path
output_path_result_wd<-paste(wd,"/Watershed_delineation/Result", sep="")

#2.2 create output folder in HRU Folder
#result
dir.create(paste(wd,"/HRU/Result", sep=""))
#output file path
output_path_result_HRU<-paste(wd,"/HRU/Result", sep="")

#3. Setting up saga environment
#Note: Insert saga installment path in your computer
#SAGA-GIS executables environment environment

# Folder.raw <- c("C:/Program Files/QGIS Essen/apps/saga/")
# Folder.saga <- NULL
# #search directory that contain file name saga_cmd.exe
# #default directory for SAGA_GIS executable is in C://Program files
# #if saga gis wasn't found in users' pc, choose directory path that contain file name saga_cmd.exe
# #(usually in qgis pass folder)
# List.folder <- list.files(Folder.raw, pattern="saga_cmd.exe", recursive = TRUE,
#                           full.names=TRUE)
# if (all(grepl("saga", List.folder))==T){
#   Folder.saga <- dirname(grep("QGIS Essen", List.folder[which(grepl("QGIS Essen", List.folder))], value = TRUE))
# } else {
#   Folder.saga <- choose.dir()
# }
Folder.saga <- "D:/LUMENS/Supporting_softwares/saga-7.0.0_x64/"
#------------------------------------------------------------------------------
#INPUT AND OUTPUT DATA (Defined by user)
#------------------------------------------------------------------------------
#1. WATERSHED DELINEATION

#file path to raw data (DEM)
DEM_raw_raster <- "D:/gg_scripts/git/greengrowth/data_jambi/raster/DEM_Musi.tif"
DEM_name<-"DEM_raw"

#file path to DEM saga grid format
DEM_sgrd <- paste0(wd, "/", DEM_name, ".sgrd", sep="")

#Setting up value by user for Watershed delineation process
#1. Preprocessing (fillsink) minimum threshold (degree)
#note : minimum threshold define minimum slope gradient to preserve from cell to cell,
#with a values of zero sinks are fill up to the spill elevation
# (which results in flat areas)
# example for this study case:
#  fillsink_threshold<-0.001 #unit in degree
fillsink_threshold <- as.numeric(0.001)

#2. Formula for reclass upslop
#Note: a = grid or raster 1
#This formula used for removing zero or no data value and value below certain value,
#in order to get watershed boundaries
# example for this study case
# a: raster 1
Formula <- "(a)/(a>90)"

#4. Initiation threshold in channel network process
#Note: The higher you set the threshold, 
#the less cells that will be found in the initiation grid 
#which fulﬁll the initiation condition, so the less channels that will be deﬁned. 
## example for this study case
#Initiation_threshold2 <- 1000000000 #unit in number of cells 
Initiation_threshold <- as.numeric(1000000000)

#5. Minimum size of number of basins
#Note: Minimum size of basins (unit in cells)
## example for this study case
#min_size <- 0 #by default 0
min_size <- as.numeric(0)


#DEM raster preprocessing
Fill_sink_raster <- paste(wd, "/Fill_sink_DEM.sgrd", sep="")
Fill_sink_tif <- paste (wd, "/Fill_sink_DEM.tif", sep="")
#Flow direction
Flow_direction_raster <- paste(wd, "/Flow_direction.sgrd", sep="")
Flow_direction_tif<- paste(wd, "/Flow_direction.tif", sep="")
#Catchment area
Catchment_area_raster <- paste(wd, "/Catchment_area.sgrd", sep="")

#Upslope area
Upslope_area_raster <- paste(wd, "/Upslope_area.sgrd", sep="")
Upslope_area_raster_reclass <- paste(wd, "/Upslope_area_reclass.sgrd", sep="")

#Channel network
Channel_network_raster <- paste(wd, "/Channel_network.sgrd", sep="")
Channel_network_vector <- paste(wd, "/Channel_network.shp", sep="" )
Channel_network_vector_clipped <- paste(output_path_result_wd, "/Channel_network_clipped.shp", sep="")
Channel_network_tif <- paste (output_path_result_wd, "Channel_network.tif", sep="")

#watershed basins
Watershed_basins_raster <- paste(wd, "/Watershed_basins.sgrd", sep="")

#watershed raster
Watershed_raster.raw <- paste(output_path_result_wd, "/watershed_raster.sgrd", sep="")
Watershed_vector <- paste(output_path_result_wd, "/watershed_delineation.shp", sep="")

#Watershed delineation
Watershed_raster <- paste(output_path_result_wd, "/Watershed_delineation.tif", sep="")

#slope length
slope_dem_sgrd <- paste(output_path_result_wd, "/slope_dem.sgrd", sep="")
slope_dem_tif <- paste(output_path_result_wd, "/slope_dem.tif", sep="")

#--------------------------------------------------------------------------------------------
#                                     PROCESS
#--------------------------------------------------------------------------------------------
#Setting up SAGA environment
myenv <- rsaga.env(workspace=Folder.saga, path=Folder.saga)

#input DEM DATA
DEM<-raster(DEM_raw_raster)

#---------------------------------
# PROCESSING IN SAGA
#---------------------------------
#Note: format raster in saga grid should be .sgrd

#convert to SAGA grid format
DEMsgrd <- rsaga.geoprocessor(lib = "io_gdal",
                              module = 0,
                              param = list(FILES = DEM_raw_raster,
                                           GRIDS = DEM_sgrd, #output
                                           TRANSFORM = 1,
                                           RESAMPLING = 4), #default method (B-Spline Interpolation)
                              env=myenv)

#    PREPROCESSING (FILL SINK PROCESS) using Wang Liu, 2006 method
# DEMs are not always prepared for hydrological analysis, 
#so before we can start extracting hydrological information from a DEM, 
#some modiﬁcations have to be usually done to it so as to ensure that other modules 
#(which we will shortly see) will not yield wrong results.
#The main source of these wrong results are pits, that is, 
#cells or groups of cells that are surrounded by others with higher elevation. 
#Since there are no lower cells through which route the ﬂow, 
#ﬂow routing algorithms behave badly in this cells. 
#They have to be ﬁlled before using any module that involves ﬂow routing
fillsink <- rsaga.fill.sinks(in.dem =  DEM_sgrd, 
                             out.dem = Fill_sink_raster,#output
                             out.flowdir = Flow_direction_raster, #output
                             method = "wang.liu.2006",
                             minslope = fillsink_threshold,
                             env=myenv)

#     CATCHMENT AREA
#The most important parameter that can be derived is the catchmentarea, 
#also know as ﬂow accumulation. 
#The catchment area of a cell indicates the area upslope that 
#cell whose ﬂow will eventually reach it
Catchment_area <- rsaga.geoprocessor(lib= "ta_hydrology", module = "Flow Accumulation (Top-Down)", param = list(
  ELEVATION = Fill_sink_raster,
  FLOW = Catchment_area_raster, #output
  METHOD = "4",
  LINEAR_MIN = 500, #default when using multiple flow direction method (mfd), 
  CONVERGENCE = 1.1), #default, 
  env = myenv)

# cHANNEL NETWORK
#Channels are usually located in areas through which a great amount of water ﬂows, 
#so this water deﬁnes the channel itself and modelates its shape. 
#Therefore, it is quite logical to try to extract those channels from catchment area grids, 
#which indicate the amount of cells located upslope each one and, consequently, 
#can be used to evaluate the amount of water that will ﬂow through it coming from all those 
#upslope cells.
channel_network <- rsaga.geoprocessor(lib = "ta_channels",                                     
                                      module = "Channel Network", 
                                      param = list(ELEVATION = Fill_sink_raster,
                                                   SINKROUTE = Flow_direction_raster,
                                                   CHNLNTWRK = Channel_network_raster,#output
                                                   SHAPES = Channel_network_vector, #output
                                                   INIT_GRID = Catchment_area_raster,
                                                   INIT_VALUE = Initiation_threshold,
                                                   MINLEN = 10),   #default
                                      env=myenv)

# ADhere
#UPSLOPE AREA
#Defining the shape of the associated basins
#Note: Users should define coordinate cartesian (x,y) where the outlet is
#2. Upslope area coordinate
#note: users should define where the outlet location is
# unit in cartesian (x,y)
# example for this study case:
#Upslope_x <- 503258.49844200001
#Upslope_y <- 9735600.1766030006
channel_shp_test <- try(readOGR(Channel_network_vector), silent = T)
if (class(channel_shp_test)=="try-error"){
  dlgMessage("Channel network not created! Check your initiation threshold value", type="ok")
} else {
  channel_shp <- readOGR(Channel_network_vector)
}
unique_value <-unique(channel_shp@data$Order)
#------------------------------------------------------------------------------------------
#   function for choose water outlet coordinate
coordinate_intersection_table <- data.frame(x=0,y=0)
for (i in unique_value){
  for (j in unique_value){
    if (i==j || j<i){
      next
    }
    s <- print(paste0(i,"-",j))
    y <- gIntersection(channel_shp[channel_shp$Order==i,], channel_shp[channel_shp$Order==j,])
    if (is.null(y)){
      next
    }
    coordinate_intersection_table <- rbind(coordinate_intersection_table, coordinates(y))
    if (is.null(coordinates(y))){
      next
    }
  }
}
coordinate_intersection_table <- coordinate_intersection_table[-1,]
coordinate_intersection_table <- coordinate_intersection_table[!duplicated(coordinate_intersection_table),]
coordinate_intersection_table$ID <- 1:nrow(coordinate_intersection_table)

#function for open graphic window
# #resize plot view in R
# resize.win <- function(Width=6, Height=6)
# {
#   # works for windows
#   dev.off(); # dev.new(width=6, height=6)
#   windows(record=TRUE, width=Width, height=Height)
# }
# resize.win(15,15)

plot(channel_shp)
points(coordinate_intersection_table$x, coordinate_intersection_table$y,
       main ="River intersection point",
       col="red", pch=19, lty="solid", cex=0.75, lwd=2)
pointLabel(coordinate_intersection_table$x,coordinate_intersection_table$y, labels=as.character(coordinate_intersection_table$ID), 
           offset=-0.5, cex=0.75)
water_outlet_code <- NULL
dlgMessage("Look at the image to choose one of water outlet coordinate")
res <- dlgList(coordinate_intersection_table$ID, title="Choose ID code coordinate", multiple = TRUE)$res
if (!length(res)) {
  dlgMessage(print(paste0("You cancelled the choice!\n")))
} else {
  dlgMessage(print(paste0("You selected water intersection code", ": ", res)))
  water_outlet_code <- as.numeric(res)
}

water.outlet.coordinate <- function(x){
  z <- coordinate_intersection_table[coordinate_intersection_table$ID==x,1:2]
  Coordinate_x <- z$x
  Coordinate_y <- z$y
  Coordinate_water_outlet <- data.frame(x=Coordinate_x,y=Coordinate_y)
  return(Coordinate_water_outlet)
}
coordinate_intersection_wateroutlet <- water.outlet.coordinate(water_outlet_code)
Intersection_x <- coordinate_intersection_wateroutlet$x
Intersection_y <- coordinate_intersection_wateroutlet$y

#creating buffer
buffer_point <- seq(0, 2 * pi, length.out = 50)
buffer_polygon <- cbind(Intersection_x + 300 * sin(buffer_point), Intersection_y + 300 * cos(buffer_point))
buffer_polygon_shp <- SpatialLines(list(Lines(list(Line(buffer_polygon)), "line")), proj4string = CRS(proj4string(channel_shp)))

#checking from graph
plot(channel_shp)
plot(buffer_polygon_shp, add=TRUE)
points(coordinates(gIntersection(buffer_polygon_shp, channel_shp)),
       col="blue", pch=20, lty="solid")

#checking the intersect shapefile
intersect_buff_line<- over(buffer_polygon_shp, channel_shp, returnList = TRUE)
unlist_intersect_buff_line <- as.numeric(sapply(intersect_buff_line, '[[', "Order"))
max_stream_order <- max(unlist_intersect_buff_line)

#intersection stream and buffer coordinate
coordinate_out<- data.frame(coordinates(gIntersection(buffer_polygon_shp, channel_shp[channel_shp$Order==max_stream_order,])))

#check water outlet coordinate 
plot(channel_shp)
points(coordinates(gIntersection(buffer_polygon_shp, channel_shp[channel_shp$Order==max_stream_order,])),
       pch=20, col="red")

#upslope_coordinate
Upslope_x <- coordinate_out$x
Upslope_y <- coordinate_out$y


#---------------------------------------------------------------------------------------------
Upslope_area <- rsaga.geoprocessor(lib = "ta_hydrology",
                                   module = 4, #upslope area,
                                   param = list(ELEVATION = Fill_sink_raster,
                                                AREA = Upslope_area_raster, #output
                                                TARGET_PT_X = Upslope_x,
                                                TARGET_PT_Y = Upslope_y,
                                                METHOD = 0),
                                   env=myenv)

#removing zero value in uplope area (watershed)
#reclass upslope area to remove zero or no data value
Uplope_area_nozero <- rsaga.geoprocessor(lib = "grid_calculus",
                                         module = "Grid Calculator",
                                         param = list(GRIDS = Upslope_area_raster,
                                                      RESULT = Upslope_area_raster_reclass, #output
                                                      FORMULA = Formula),
                                         env = myenv)

# WATERSHED BASINS
#Each segment from the just created channel network has an associated basin 
#(which corresponds to the catchment area of the lower point of 
#the segment minus the basins associated to other segment located upslope)
watershed_basins <-rsaga.geoprocessor(lib = "ta_channels",
                                      module = 1, #watershed basins
                                      param = list(ELEVATION = Fill_sink_raster,
                                                   CHANNELS = Channel_network_raster,
                                                   BASINS = Watershed_basins_raster,
                                                   MINSIZE = min_size),
                                      env=myenv)

# MASKING WATERSHED BASING WITH UPSLOPE AREA
masking_layer <- rsaga.geoprocessor(lib = "grid_tools",
                                    module = "Grid Masking",
                                    param = list(GRID = Watershed_basins_raster,
                                                 MASK = Upslope_area_raster_reclass,
                                                 MASKED = Watershed_raster.raw),
                                    env = myenv)

#export to raster
#1. Watershed delineation vector
Watershed_delineation_vector <- rsaga.geoprocessor(lib = "shapes_grid",
                                                   module = "Vectorising Grid Classes",
                                                   param = list(GRID = Watershed_raster.raw,
                                                                POLYGONS = Watershed_vector),
                                                   env = myenv)
#2. channel network raster
channel_raster <- rsaga.geoprocessor(lib="io_gdal",
                                     module = "GDAL: Export Raster to GeoTIFF",
                                     param = list(GRIDS = Channel_network_raster,
                                                  FILE=Channel_network_tif),
                                     env=myenv)

Channel_network_clip <- rsaga.geoprocessor(lib = "shapes_polygons",
                                           module = "Polygon Clipping",
                                           param = list(CLIP = Watershed_vector,
                                                        S_INPUT = Channel_network_vector,
                                                        S_OUTPUT = Channel_network_vector_clipped,
                                                        M_INPUT = Channel_network_vector,
                                                        M_OUTPUT = Channel_network_vector_clipped),
                                           env = myenv)
#3. watershed delineation raster
watershed_delineation_raster <- rsaga.geoprocessor(lib="io_gdal",
                                                   module = "GDAL: Export Raster to GeoTIFF",
                                                   param = list(GRIDS = Watershed_raster.raw,
                                                                FILE = Watershed_raster),
                                                   env=myenv)

#4. Fill sink raster
Fill_sink <- rsaga.geoprocessor(lib="io_gdal",
                                module = "GDAL: Export Raster to GeoTIFF",
                                param = list(GRIDS = Fill_sink_raster,
                                             FILE = Fill_sink_tif),
                                env=myenv)

#4. Slope dem raster
slope_length <- rsaga.geoprocessor(lib="ta_hydrology",
                                   module = "Slope Length",
                                   param = list(DEM = DEM_sgrd,
                                                LENGTH = slope_dem_sgrd),
                                   env=myenv)

slope_length_raster <- rsaga.geoprocessor(lib="io_gdal",
                                          module = "GDAL: Export Raster to GeoTIFF",
                                          param = list(GRIDS = slope_dem_sgrd,
                                                       FILE = slope_dem_tif),
                                          env=myenv)

flow_accumulation_raster <- rsaga.geoprocessor(lib="io_gdal",
                                               module = "GDAL: Export Raster to GeoTIFF",
                                               param = list(GRIDS = Flow_direction_raster,
                                                            FILE = Flow_direction_tif),
                                               env=myenv)

#Save global environment
save(myenv, raw_path ,Watershed_raster, Watershed_vector, LU_path, SM_path,
     DEM_path, threshold_slope, threshold_soil, threshold_landuse, Channel_network_vector_clipped, Channel_network_vector, 
     output_path_result_wd, output_path_result_HRU, slope_length_raster, slope_dem_tif,
     Output_HRU, channel_shp, Upslope_y, Upslope_x, file = "WD.RData", envir = .GlobalEnv) 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#============================================================================================
#                                     END
#============================================================================================
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
graphics.off()
end<-Sys.time()
time_diff<-end-start