#--------------------------------------------------------------------
#                               USLE
#--------------------------------------------------------------------

## define initial file
start<-Sys.time()
working_directory="D:/ICRAF/GGP_Jambi/RScript/USLE/"
lookup_lc="data_jambi/table/Land_cover_legend_jambi.csv"
#base_map="data_jambi/raster/*basemap.tif*"
DEM_raw_hyd="data_jambi/raster/DEM_raw_hyd.tif"

## Library
library (rgdal)
library (RSAGA)
library (raster)
library (maptools)
library (svDialogs)
library (rgeos)
library (rlist)
library (sf)

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
raw_path <- dirname("d:/ICRAF/_R_Project/greengrowth/data_jambi/raster/")

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

#Folder.raw <- c("C:/Program Files/saga-7.1.1_x64/")
#Folder.saga <- NULL
#search directory that contain file name saga_cmd.exe
#default directory for SAGA_GIS executable is in C://Program files
#if saga gis wasn't found in users' pc, choose directory path that contain file name saga_cmd.exe
#(usually in qgis pass folder)
#List.folder <- list.files(Folder.raw, pattern="saga_cmd.exe", recursive = TRUE,
#                          full.names=TRUE)
#if (all(grepl("saga", List.folder))==T){
#  Folder.saga <- dirname(grep("saga-7.1.1_x64", List.folder[which(grepl("saga-7.1.1_x64", List.folder))], value = TRUE))
#} else {
#  Folder.saga <- choose.dir()
#}
Folder.saga <- "c:/Program Files/saga-7.0.0_x64/"
#------------------------------------------------------------------------------
#INPUT AND OUTPUT DATA (Defined by user)
#------------------------------------------------------------------------------
#1. WATERSHED DELINEATION

#file path to raw data (DEM)
DEM_raw_raster <- "d:/ICRAF/_R_Project/greengrowth/data_jambi/raster/DEM_raw_hyd.tif"
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
order_threshold <- as.numeric(5)
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

# Newly defined variables from untitled2.R====
Direction_raster <- paste0(wd,"/Flow_direct_rst.sgrd")

Order_raster <- paste0(wd,"/Segment_order.sgrd")
Basins_raster <- paste0(wd,"/Basins.sgrd")
# Channel_segments <- paste0(tes_dir,"Segments.shp") AD changed into Channel_network_vector
Basins_vector <- paste0(wd,"/Basins.shp")
Nodes <- paste0(wd,"/Nodes.shp")
# segment_raster <- paste0(tes_dir,"segments.sgrd") AD changed into Channel_network_raster
# segment_tif <- paste0(tes_dir, "segments.tif") AD changed into Channel_network_tif
Clipped_fillsink <- paste0(wd, "/Upslp_Fill_sink_DEM.sgrd", sep="")
Clipped_fillsink_tif <- paste0(wd,"/Fillsink_DEM.tif")

# Watershed_sbasins_raster <- paste(tes_dir, "Watershed_subbasins.sgrd", sep="") # AD rmve
# Watershed_sbasins_tif <- paste0(output_path_result_wd,"/Watershed_subbasins.tif") AD chged into Watershed_raster
# Watershed_basins_v <- paste(tes_dir, "Watershed_basins.shp", sep="") AD rmve
# Watershed_sbasins_v <- paste(output_path_result_wd, "/Watershed_subbasins.shp", sep="") AD chged into Watershed_vector
# Watershed_heads <- paste(tes_dir, "Watershed_heads.shp", sep="") AD rmve
# Watershed_mouths <- paste(tes_dir, "Watershed_mouths.shp", sep="") AD rmve


#misc variables
upslope_vector <- paste0(wd,"/upslope.shp")

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


# segment selection framework [31/07/17] ====
channel_shp <- readOGR(Channel_network_vector)
# convert 'ORDER' and 'SEGMENT_ID' class into numeric
channel_shp@data$Order <- as.numeric(as.character(channel_shp@data$Order)) #factor to numeric converstion must involve conversion from factor to character beforehand 
channel_shp@data$SegmentID <- as.numeric(as.character(channel_shp@data$SegmentID)) #factor to numeric converstion must involve conversion from factor to character beforehand 
# Identify the highest channel order in the dataset
highest_order <- max(channel_shp@data$Order)
# subset the channel network to encompass only the top 3 or 2 order
if(highest_order > 2) high_order_segments <- channel_shp[channel_shp@data$Order >= (highest_order-2),] else {
  high_order_segments <- channel_shp[channel_shp@data$Order >= (highest_order-1),]
}
plot(high_order_segments)

# input from AD
# extract only highest segment
highest_segment <- high_order_segments[high_order_segments$Order== 14,]
# define highest segment
coord_highestSegment <- highest_segment@lines[[1]]@Lines[[1]]@coords


# generate 'mid_pts' to allow user to select the segments with the desired final outlet
mid_pts <- SpatialLinesMidPoints(high_order_segments)


# selection framework
points(mid_pts@coords[,1], mid_pts@coords[,2],
       col="red", pch=19, lty="solid", cex=0.75, lwd=2)
#   Display the points label in accordance to the segment ID
pointLabel(mid_pts@coords[,1], mid_pts@coords[,2], labels=as.character(mid_pts@data$SegmentID), 
           offset=0, cex=1)
#   Selection interface
water_outlet_code <- NULL
dlgMessage("Look at the image to choose one of water outlet coordinate")
res <- dlgList(mid_pts@data$SegmentID, title="Choose ID code coordinate", multiple = TRUE)$res
if (length(res)==0) {
  dlgMessage(print(paste0("You cancelled the choice!\n")))
} else {
  dlgMessage(print(paste0("You selected segment ID", ": ", res)))
  water_outlet_code <- as.numeric(res)
}

plot(channel_shp[channel_shp@data$SegmentID==water_outlet_code,])

nodes_outlet_segment <- readOGR(Nodes, stringsAsFactors = FALSE)
# factor data conversion
# nodes_outlet_segment@data$NODE_ID <- as.numeric(as.character(nodes_outlet_segment@data$NODE_ID)) No longer required because the factors have been converted by the 'stringsAsFactors' argument)

nodeID_selections <- c(as.character(channel_shp@data[channel_shp@data$SEGMENT_ID==water_outlet_code,"NODE_A"]),as.character(channel_shp@data[channel_shp@data$SEGMENT_ID==water_outlet_code,"NODE_B"]))
nodes_outlet_segment <- nodes_outlet_segment[nodes_outlet_segment@data$NODE_ID %in% nodeID_selections,]
points(nodes_outlet_segment@coords[,1], nodes_outlet_segment@coords[,2], col="green", pch = 19, lty = "solid", cex = 0.75, lwd = 2)
pointLabel(nodes_outlet_segment@coords[,1], nodes_outlet_segment@coords[,2], labels = nodes_outlet_segment@data$NODE_ID, cex = 0.75)
water_outlet_code <- NULL
dlgMessage("Look at the image to choose one of water outlet coordinate")
res <- dlgList(nodes_outlet_segment@data$NODE_ID, title="Choose ID code coordinate", multiple = TRUE)$res
if (length(res)==0) {
  dlgMessage(print(paste0("You cancelled the choice!\n")))
} else {
  dlgMessage(print(paste0("You selected outlet ID", ": ", res)))
  water_outlet_code <- as.numeric(res)
}

outlet_coords <- nodes_outlet_segment[nodes_outlet_segment@data$NODE_ID == water_outlet_code,]@coords

Upslope_x <-outlet_coords[,1]
Upslope_y <- outlet_coords[,2]

#      CATCHMENT AREA
# The most important parameter that can be derived is the catchmentarea, 
# also know as ﬂow accumulation. 
# The catchment area of a cell indicates the area upslope that 
# cell whose ﬂow will eventually reach it
# Catchment_area <- rsaga.parallel.processing(in.dem = Fill_sink_raster,
#                                             out.carea= Catchment_area_raster, #output
#                                             method = "mfd",
#                                             linear.threshold = 500, #default when using multiple flow direction method (mfd), 
#                                             convergence = 1.1, #default, 
#                                             env = myenv)
# 
#  cHANNEL NETWORK
# Channels are usually located in areas through which a great amount of water ﬂows, 
# so this water deﬁnes the channel itself and modelates its shape. 
# Therefore, it is quite logical to try to extract those channels from catchment area grids, 
# which indicate the amount of cells located upslope each one and, consequently, 
# can be used to evaluate the amount of water that will ﬂow through it coming from all those 
# upslope cells.
# AD corr
# channel_network <- rsaga.geoprocessor(lib = "ta_channels",
#                                       module = "Channel Network", 
#                                       param = list(ELEVATION = Fill_sink_raster,
#                                                    CHNLROUTE = Flow_direction_raster,
#                                                    CHNLNTWRK = Channel_network_raster,#output
#                                                    SHAPES = Channel_network_vector, #output
#                                                    INIT_GRID = Catchment_area_raster,
#                                                    INIT_VALUE = Initiation_threshold,
#                                                    MINLEN = 10),   #default
#                                       env=myenv)
# 
# UPSLOPE AREA
# Defining the shape of the associated basins
# Note: Users should define coordinate cartesian (x,y) where the outlet is
# 2. Upslope area coordinate
# note: users should define where the outlet location is
#  unit in cartesian (x,y)
#  example for this study case:
# Upslope_x <- 503258.49844200001
# Upslope_y <- 9735600.1766030006
# channel_shp_test <- try(readOGR(Channel_network_vector), silent = T)
# if (class(channel_shp_test)=="try-error"){
#   dlgMessage("Channel network not created! Check your initiation threshold value", type="ok")
# } else {
#   channel_shp <- readOGR(Channel_network_vector)
# }
# unique_value <-unique(channel_shp@data$Order)
#  Framework to select the desired channel segments and outlet AD corr------------------------------------------------------------------------------------------
#    function for choose water outlet coordinate
# coordinate_intersection_table <- data.frame(x=0,y=0)
# for (i in unique_value){
#   for (j in unique_value){
#     if (i==j || j<i){
#       next
#     }
#     s <- print(paste0(i,"-",j))
#     y <- gIntersection(channel_shp[channel_shp$Order==i,], channel_shp[channel_shp$Order==j,])
#     if (is.null(y)){
#       next
#     }
#     coordinate_intersection_table <- rbind(coordinate_intersection_table, coordinates(y))
#     if (is.null(coordinates(y))){
#       next
#     }
#   }
# }
# coordinate_intersection_table <- coordinate_intersection_table[-1,]
# coordinate_intersection_table <- coordinate_intersection_table[!duplicated(coordinate_intersection_table),]
# coordinate_intersection_table$ID <- 1:nrow(coordinate_intersection_table)
# 
# function for open graphic window
#  #resize plot view in R
#  resize.win <- function(Width=6, Height=6)
#  {
#    # works for windows
#    dev.off(); # dev.new(width=6, height=6)
#    windows(record=TRUE, width=Width, height=Height)
#  }
#  resize.win(15,15)
# 
# plot(channel_shp)
# points(coordinate_intersection_table$x, coordinate_intersection_table$y,
#        main ="River intersection point",
#        col="red", pch=19, lty="solid", cex=0.75, lwd=2)
# pointLabel(coordinate_intersection_table$x,coordinate_intersection_table$y, labels=as.character(coordinate_intersection_table$ID), 
#            offset=-0.5, cex=0.75)
# water_outlet_code <- NULL
# dlgMessage("Look at the image to choose one of water outlet coordinate")
# res <- dlgList(coordinate_intersection_table$ID, title="Choose ID code coordinate", multiple = TRUE)$res
# if (!length(res)) {
#   dlgMessage(print(paste0("You cancelled the choice!\n")))
# } else {
#   dlgMessage(print(paste0("You selected water intersection code", ": ", res)))
#   water_outlet_code <- as.numeric(res)
# }
# 
# water.outlet.coordinate <- function(x){
#   z <- coordinate_intersection_table[coordinate_intersection_table$ID==x,1:2]
#   Coordinate_x <- z$x
#   Coordinate_y <- z$y
#   Coordinate_water_outlet <- data.frame(x=Coordinate_x,y=Coordinate_y)
#   return(Coordinate_water_outlet)
# }
# coordinate_intersection_wateroutlet <- water.outlet.coordinate(water_outlet_code)
# Intersection_x <- coordinate_intersection_wateroutlet$x
# Intersection_y <- coordinate_intersection_wateroutlet$y
# 
# creating buffer
# buffer_point <- seq(0, 2 * pi, length.out = 50)
# buffer_polygon <- cbind(Intersection_x + 300 * sin(buffer_point), Intersection_y + 300 * cos(buffer_point))
# buffer_polygon_shp <- SpatialLines(list(Lines(list(Line(buffer_polygon)), "line")), proj4string = CRS(proj4string(channel_shp)))
# 
# checking from graph
# plot(channel_shp)
# plot(buffer_polygon_shp, add=TRUE)
# points(coordinates(gIntersection(buffer_polygon_shp, channel_shp)),
#        col="blue", pch=20, lty="solid")
# 
# checking the intersect shapefile
# intersect_buff_line<- over(buffer_polygon_shp, channel_shp, returnList = TRUE)
# unlist_intersect_buff_line <- as.numeric(sapply(intersect_buff_line, '[[', "Order"))
# max_stream_order <- max(unlist_intersect_buff_line)
# 
# intersection stream and buffer coordinate
# coordinate_out<- data.frame(coordinates(gIntersection(buffer_polygon_shp, channel_shp[channel_shp$Order==max_stream_order,])))
# 
# check water outlet coordinate 
# plot(channel_shp)
# points(coordinates(gIntersection(buffer_polygon_shp, channel_shp[channel_shp$Order==max_stream_order,])),
#        pch=20, col="red")
# 
# upslope_coordinate
# Upslope_x <- coordinate_out$x
# Upslope_y <- coordinate_out$y
#  AD test only upslope_coordinate----
# Upslope_x <- coordinates(end_segment_end_pts)[2,1]
# Upslope_y <- coordinates(end_segment_end_pts)[2,2]


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
#reclass upslope area to remove zero or no data value AD corr
Upslope_area_nozero <- rsaga.geoprocessor(lib = "grid_tools",
                                          module = "Reclassify Grid Values",
                                          param = list(INPUT = Upslope_area_raster,
                                                       RESULT = Upslope_area_raster_reclass, #output
                                                       METHOD = 0, OLD = 0.0000, NEW=-99999.0000, SOPERATOR= "=",
                                                       NODATA = "-99999", RESULT_NODATA_CHOICE = "1"),
                                          env = myenv)
# AD add: reclass once more to convert 100 into 1
Upslope_area_nozero <- rsaga.geoprocessor(lib = "grid_tools",
                                          module = "Reclassify Grid Values",
                                          param = list(INPUT = Upslope_area_raster_reclass,
                                                       RESULT = Upslope_area_raster_reclass, #output
                                                       METHOD = 0, OLD = 1.00, NEW=1.0000, SOPERATOR= ">="),
                                          env = myenv)

# AD add: polygonize the upslope_area_raster_reclass for the purpose of channel segment clipping
rsaga.geoprocessor(lib = "shapes_grid",
                   module = "Vectorising Grid Classes",
                   param = list(GRID = Upslope_area_raster_reclass,
                                POLYGONS = upslope_vector),
                   env = myenv)


# AD add: clipping the channel network based on the upslope_vector
# channel <- readOGR(Channel_network_vector, stringsAsFactors = FALSE) AD rmve: 'channel_shp' already exists
bound <- readOGR(upslope_vector)
crs(channel_shp) <- crs(bound)
channel_shp <- channel_shp[bound,]
writeOGR(channel_shp, dirname(Channel_network_vector), layer = "Channel_network", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#AD add: Clipping nodes based on upslope_vector
nodes <- readOGR(Nodes, stringsAsFactors = FALSE)
crs(nodes) <- crs(bound)
nodes <- nodes[bound,]
writeOGR(nodes, dirname(Nodes), layer = "Nodes", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# produce the channel.sgrd from the segments.shp====
# 1. Generate the data frame to store the desired information on the segment ID and the order====
channel_table <- channel_shp@data
for(c in 1:ncol(channel_table)){
  channel_table[,c] <- as.numeric(as.character(channel_table[,c]))
}
segment_order_table <- data.frame(seg_ID = channel_table$SEGMENT_ID, SHERV = 0)
# assign value 1 to first ordered segments 
segment_order_table[segment_order_table$seg_ID %in% channel_table[channel_table$ORDER==1,"SEGMENT_ID"],"SHERV"] <- 1
# 2. Subset junctions from 'Nodes'====
# 'junction_ID' variable should be transformed into hierarchical list
# nodes <- readOGR(Nodes, stringsAsFactors = FALSE)
junction_ID <- nodes@data
junction_ID <- junction_ID[junction_ID$TYPE == "Junction",]
junction_ID$NODE_ID <- as.numeric(as.character(junction_ID$NODE_ID))
junction_ID$solved <- 0
junction_ID <- junction_ID[,c("NODE_ID", "solved")]

# 3. generate the hierarchical list based which contains the information about segments related with each nodes
# generate the small function to fill the segment_A or segment_B information
fill_segment_info <- function(seg = "A", nodeID = n){
  segIDs <- channel_table[eval(parse(text=paste0("channel_table$NODE_",seg))) == nodeID, "SEGMENT_ID"]
  order <- segment_order_table[segment_order_table$seg_ID %in% segIDs,]
  return(order)
}

# junction_list <- vector("list", length(junction_ID$NODE_ID))
junction_list <- list()
for(n in 1: nrow(junction_ID)){
  nodeID <- junction_ID[n,"NODE_ID"]
  app_list <- list(segment_B= fill_segment_info("B", nodeID), segment_A= fill_segment_info("A", nodeID))
  junction_list <- list.append(junction_list, app_list)
  if(n == nrow(junction_ID)) names(junction_list) <- as.character(junction_ID$NODE_ID)
}

repeat{
  # identify the readily calculated junction
  nID_calculate_ready <- names(which(list.mapv(junction_list, ! 0 %in% segment_B$SHERV & segment_A$SHERV== 0)))
  # conditional test to break the loop
  if(length(nID_calculate_ready) ==0){
    if(length(which(list.mapv(junction_list, segment_A$SHERV ==0)))==0){
      print("Calculation process has been completed successfully")
      break
    } else{
      print("Calculation process couldn't be resumed. Please check the data")
      break
    }
  }
  # looping
  print(paste0("Processing the calculation process for element(s) '", paste(nID_calculate_ready, collapse = ","),"'..."))
  for(c in 1: length(nID_calculate_ready)){
    # calculate the order of B segment
    result <- sum(junction_list[[nID_calculate_ready[c]]]$segment_B$SHERV)
    updated_ID <-junction_list[[nID_calculate_ready[c]]]$segment_A$seg_ID
    junction_list[[nID_calculate_ready[c]]]$segment_A$SHERV <- result
    
    # update all other order under nodes in which the corresponding segment exist
    element_tobe_updated <- names(which(list.mapv(junction_list, updated_ID %in% segment_B$seg_ID)))
    if(length(element_tobe_updated) !=0){
      for(e in 1: length(element_tobe_updated)){
        if(e==1) print(paste0("Updating value in other related element(s): '", element_tobe_updated,"'..."))
        junction_list[[element_tobe_updated[e]]]$segment_B[junction_list[[element_tobe_updated[e]]]$segment_B$seg_ID == updated_ID,"SHERV"] <- result
      }
    }
    # update the detail in 'segment_order_table': 'seg_ID'  'SHERV'
    segment_order_table[segment_order_table$seg_ID == updated_ID, "SHERV"] <- result
    # loop closure
  }
  # repeat closure
}

# 4. update the attribute table of channel .shp with the segment_order_table====
channel_shp@data <- merge(channel_shp@data,segment_order_table, by.x = "SEGMENT_ID", by.y = "seg_ID", all.x =TRUE)
channel_shp@data <- channel_shp@data[,c(1,2,3,5,7,8)]
channel_shp <- channel_shp
writeOGR(channel_shp, dirname(Channel_network_vector), layer = "Channel_network", driver = "ESRI Shapefile", overwrite_layer = TRUE)
# convert the "ORDER" table attribute into numeric AD rmove
# rsaga.geoprocessor(lib= "table_tools", module="Change Field Type", param= list(TABLE = Channel_network_vector,
#                                                                                FIELD = "ORDER",
#                                                                                TYPE = 8), env= myenv)


rsaga.geoprocessor(lib="grid_gridding", module = "Shapes to Grid", param=list(INPUT=Channel_network_vector,
                                                                              FIELD= "SHERV",
                                                                              MULTIPLE = 3,
                                                                              LINE_TYPE = 0,
                                                                              GRID_TYPE=2,
                                                                              TARGET = 1, #TARGET=1 implies that the settings should follow the file's
                                                                              GRID_GRID = Channel_network_raster),
                   env=myenv)

# Generate the Channel_segmentID_raster based on the Channel_network_vector
rsaga.geoprocessor(lib="grid_gridding", module = "Shapes to Grid", param=list(INPUT=Channel_network_vector,
                                                                              FIELD= "SEGMENT_ID",
                                                                              MULTIPLE = 1,
                                                                              LINE_TYPE = 0,
                                                                              GRID_TYPE=2,
                                                                              TARGET = 1, #TARGET=1 implies that the settings should follow the file's
                                                                              GRID_GRID = Channel_segmentID_raster),
                   env=myenv)

# AD here 080817
# .sgrd to geotiff exploration====
# Channel_network_raster <- paste(wd, "/Channel_network.sgrd", sep="")
# Channel_network_tif <- paste (wd, "Channel_network.tif", sep="")
# AD rmve====
# rsaga.geoprocessor(lib="io_gdal",
#                    module = "GDAL: Export Raster to GeoTIFF",
#                    param = list(GRIDS = Channel_network_raster,
#                                 FILE=t_Channel_order_tif),
#                    env=myenv)


# AD add =====
# Fuzzy intersect the upslope_area_nozero with the fill_sink_raster
rsaga.geoprocessor(lib="grid_calculus", 12, param = list(GRIDS = paste(Upslope_area_raster_reclass, Fill_sink_raster, sep = ";"),
                                                         AND= Clipped_fillsink, TYPE = 1), env=myenv)
# Fuzzy intersect the channel_network_raster with the Upslope_area_raster_reclass AD rmove
# rsaga.geoprocessor(lib="grid_calculus", 12, param = list(GRIDS = paste(Upslope_area_raster_reclass, Channel_network_raster, sep = ";"),
#                                                          AND= Channel_network_raster, TYPE = 1), env=myenv)
# framework to adjust the cell values (-1) at end of source segments
# 5. identify the downstream most segment
# bottom_ID <- segment_order_table[segment_order_table$SHERV == max(segment_order_table$SHERV),"seg_ID"] AD remove: the last segment somehow has been clipped at the very end
# generate function to extract the coordinate before the last from the list of lines
second_to_last <- function(x){
  xy_matrix <- slot(x, name="Lines")[[1]]@coords
  extracted_xy <- xy_matrix[(nrow(xy_matrix)-1),]
  return(extracted_xy)
}
# apply to spatiallinesdataframe 'channel' which has 'lines' slot
second_last_xy <- as.data.frame(t(sapply(channel_shp@lines, second_to_last)))
# bind the segment id to the resulting data.frame
second_last_xy <- cbind(channel_shp@data$SEGMENT_ID, second_last_xy)
second_last_xy$value <- -1
# adjust the column names
names(second_last_xy) <- c("seg_ID", "cx","cy","value")
# replace the coordinates of the bottom most segment with the upslope coordinate
# second_last_xy[second_last_xy$seg_ID == bottom_ID,c("cx","cy")] <- c(Upslope_x, Upslope_y)
# generate spatialpoint object====
tip_points <- SpatialPointsDataFrame(second_last_xy[,c("cx","cy")], second_last_xy,proj4string = crs(channel_shp))

writeOGR(tip_points, dirname(Channel_network_raster), layer = "tip_points", driver = "ESRI Shapefile", overwrite_layer = TRUE)
# gridding the 'tip_points'
tip_points <- paste0(dirname(Channel_network_raster),"/tip_points.shp")
# generate the blank raster
# tip_point_raster0 <- paste0(tes_dir,"tip_raster0.sgrd")
tip_point_raster1 <- paste0(tes_dir,"tip_raster1.sgrd")
# generate the 'tip_point_raster' file
rsaga.geoprocessor(lib="grid_tools", module = "Reclassify Grid Values",
                   param = list (INPUT=Direction_raster,
                                 RESULT= tip_point_raster1,
                                 MIN = -2, MAX = 9,
                                 METHOD = 1, RNEW = -99999,
                                 NODATA = -99999, RESULT_NODATA_CHOICE= 1
                   ), env=myenv)
# assign value to the new raster
# rsaga.geoprocessor(lib="grid_gridding", module = "Shapes to Grid", param=list(INPUT=tip_points,
#                                                                               FIELD= "value",
#                                                                               GRID_TYPE=4,
#                                                                               TARGET = 1, #TARGET=1 implies that the settings should follow the file's
#                                                                               GRID_GRID = t_segment_raster),
#                    env=myenv)

# generate the raster which holds the -1 value for junctions and final outlet 
rsaga.geoprocessor(lib="grid_gridding", module = "Shapes to Grid", param=list(INPUT=tip_points,
                                                                              FIELD= "value",
                                                                              MULTIPLE = 1,#last
                                                                              # LINE_TYPE = 0, # point
                                                                              GRID_TYPE=1,
                                                                              TARGET = 1, #TARGET=1 implies that the settings should follow the file's
                                                                              GRID_GRID = tip_point_raster1),
                   env=myenv)
# generate 'Channel_network_raster_fin' raster file
Channel_network_raster_fin <- paste0(dirname(Channel_network_raster),"/Channel_network_fin.sgrd")
rsaga.geoprocessor(lib="grid_tools", module = "Reclassify Grid Values",
                   param = list (INPUT=Direction_raster,
                                 RESULT= Channel_network_raster_fin,
                                 MIN = -2, MAX = 9,
                                 METHOD = 1, RNEW = -99999,
                                 NODATA = -99999, RESULT_NODATA_CHOICE= 1
                   ), env=myenv)
# Mosaicking to generate the final channel_network_raster
rsaga.geoprocessor(lib= "grid_tools", module = "Mosaicking", param = list(GRIDS = paste(tip_point_raster1, Channel_network_raster, sep = ";"),
                                                                          TYPE = 6,
                                                                          OVERLAP = 2,
                                                                          TARGET = 1,
                                                                          GRID_GRID = Channel_network_raster_fin),
                   env= myenv)

# WATERSHED BASINS
#Each segment from the just created channel network has an associated basin 
#(which corresponds to the catchment area of the lower point of 
#the segment minus the basins associated to other segment located upslope)
#AD corr
rsaga.geoprocessor(lib = "ta_channels",
                   module = 1, #watershed basins
                   param = list(ELEVATION = Clipped_fillsink,
                                CHANNELS = Channel_network_raster_fin,
                                BASINS = Watershed_basins_raster,
                                MINSIZE = min_size),
                   env=myenv)

# AD draft====
# rsaga.geoprocessor(lib = "ta_channels",
#                    module = 2, #watershed basins (Extended)
#                    param = list(DEM = Clipped_fillsink,
#                                 CHANNELS = Channel_network_raster,
#                                 BASINS = Watershed_basins_raster,
#                                 SUBBASINS = Watershed_sbasins_raster,
#                                 V_BASINS = Watershed_basins_v,
#                                 V_SUBBASINS = Watershed_vector,
#                                 HEADS = Watershed_heads,
#                                 MOUTHS = Watershed_mouths
#                    ),
#                    env=myenv)

# MASKING WATERSHED BASING WITH UPSLOPE AREA AD removed====
# masking_layer <- rsaga.geoprocessor(lib = "grid_tools",
#                                     module = "Grid Masking",
#                                     param = list(GRID = Watershed_basins_raster,
#                                                  MASK = Upslope_area_raster_reclass,
#                                                  MASKED = Watershed_raster.raw),
#                                     env = myenv)

#export to raster====
#1. Watershed delineation vector #AD remove cancelled
rsaga.geoprocessor(lib = "shapes_grid",
                   module = "Vectorising Grid Classes",
                   param = list(GRID = Watershed_basins_raster,
                                POLYGONS = Watershed_vector),
                   env = myenv)
#2. channel network raster
rsaga.geoprocessor(lib="io_gdal",
                   module = "GDAL: Export Raster to GeoTIFF",
                   param = list(GRIDS = Channel_network_raster_fin,
                                FILE=Channel_network_tif),
                   env=myenv)
# segmentID tif
rsaga.geoprocessor(lib="io_gdal",
                   module = "GDAL: Export Raster to GeoTIFF",
                   param = list(GRIDS = Channel_segmentID_raster,
                                FILE=Channel_segmentID_tif),
                   env=myenv)

#3. 
rsaga.geoprocessor(lib="io_gdal",
                   module = "GDAL: Export Raster to GeoTIFF",
                   param = list(GRIDS = Watershed_basins_raster,
                                FILE=Watershed_raster),
                   env=myenv)
# Channel_network_clip <- rsaga.geoprocessor(lib = "shapes_polygons",
#                                            module = "Polygon Clipping",
#                                            param = list(CLIP = Watershed_vector,
#                                                         S_INPUT = Channel_network_vector,
#                                                         S_OUTPUT = Channel_network_vector_clipped,
#                                                         M_INPUT = Channel_network_vector,
#                                                         M_OUTPUT = Channel_network_vector_clipped),
#                                            env = myenv)
#3. watershed delineation raster AD rmve
# watershed_delineation_raster <- rsaga.geoprocessor(lib="io_gdal",
#                                                    module = "GDAL: Export Raster to GeoTIFF",
#                                                    param = list(GRIDS = Upslope_area_raster_reclass,
#                                                                 FILE = Watershed_raster),
#                                                    env=myenv)

#4. Fill sink raster
rsaga.geoprocessor(lib="io_gdal",
                   module = "GDAL: Export Raster to GeoTIFF",
                   param = list(GRIDS = Clipped_fillsink,
                                FILE = Clipped_fillsink_tif),
                   env=myenv)
#AD here
#4. Slope dem raster
rsaga.geoprocessor(lib="ta_hydrology",
                   module = "Slope Length",
                   param = list(DEM = Clipped_fillsink,
                                LENGTH = slope_dem_sgrd),
                   env=myenv)

rsaga.geoprocessor(lib="io_gdal",
                   module = "GDAL: Export Raster to GeoTIFF",
                   param = list(GRIDS = slope_dem_sgrd,
                                FILE = slope_dem_tif),
                   env=myenv)

# AD rmove
# flow_accumulation_raster <- rsaga.geoprocessor(lib="io_gdal",
#                                                module = "GDAL: Export Raster to GeoTIFF",
#                                                param = list(GRIDS = Flow_direction_raster,
#                                                             FILE = Flow_direction_tif),
#                                                env=myenv)

#Save global environment AD tb corr
save(myenv, raw_path ,Watershed_raster, Watershed_vector, LU_path, SM_path,
     DEM_path, threshold_slope, threshold_soil, threshold_landuse, Channel_network_vector, 
     output_path_result_wd, output_path_result_HRU, slope_dem_tif, Channel_segmentID_tif,
     Output_HRU, channel_shp, Upslope_y, Upslope_x, file = "WD.RData", envir = .GlobalEnv)
# omitted
# 1. Channel_network_vector_clipped > covered by 'Channel_network_vector'
# 2. Unidentified data types:
#     1. slope_length_raster
#     2. slope

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#============================================================================================
#                                     END
#============================================================================================
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# graphics.off()
end<-Sys.time()
time_diff<-end-start