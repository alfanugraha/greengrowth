load("D://ZA/GGP_Fire_Jambi/fire_modeling/qb_sdm_ot_5_fin/qb_sdm_ot_6.RData")
# raster stack
comp_lyr <- stack(raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dem.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_def_log.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_hph_log.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_hti_log.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_kebun_log.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_set_log.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_trans_log.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_distkanal_log.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_distpeat_log.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_distriver_log.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_distroad_log.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_eco.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_kaw.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_LC.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_pop.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_prec_dry.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_prec_warm.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_temp_dry.tif'),
                  raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_temp_warm.tif'))




#Species Distribution Modeling with MaxEnt
#to be included within LUMENS' QUES-B Sub-Module

#Initial Data
#Type R.Data
#load("D://ZA/GGP_Fire_Jambi/fire_modeling/qb_sdm_ot_5_fin/qb_sdm_ot_6.RData")

#Input Req: data kovariat (raster) untuk detil raster bisa dicek pada script update_comp_lyr dibawah
# raster stack
#comp_lyr <- stack(raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dem.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_def_log.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_hph_log.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_hti_log.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_kebun_log.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_set_log.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_dist_trans_log.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_distkanal_log.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_distpeat_log.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_distriver_log.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_distroad_log.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_eco.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_kaw.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_LC.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_pop.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_prec_dry.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_prec_warm.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_temp_dry.tif'),
#raster('D://ZA/GGP_Fire_Jambi/Maxent_model/Sum_temp_warm.tif'))
#Output: Peta rawan kebakaran (Raster dengan nilai continous).

#Outlines:
#memory allocation####
options(java.parameters = "-Xmx8g")


#0. Setting the suitable working directory and generating output directory####
library(dismo)
library(spatial.tools) # to adjust overlayability of the predictor/covariate layers
library(rtf)
library(raster)
library(maptools)
library(ENMeval)
library(utils)


#defining a function to create a table containing the list of raster files within the .Rdata####
ls_raster <- function(){
  all_file <- as.data.frame(ls(envir = globalenv()))
  all_file$type <- NA
  colnames(all_file)[1] <- "vars"
  for(i in 1: nrow(all_file)){
    eval(parse(text=(paste("all_file[",i,", 'type'] <- class(",all_file$vars[i],")", sep = ""))))
  }
  rasterfiles <- as.data.frame(all_file[grep(pattern = "^Raster",x = all_file$type, ignore.case = F),1])
  rasterfiles$Notes <- NA
  colnames(rasterfiles)[1]<- "var_name"
  for(i in 1: nrow(rasterfiles)){
    rasterfiles[i,2] <- eval(parse(text=paste("names(",rasterfiles[i,1],")", sep = "")))
  }
  return(rasterfiles)
}

pt_correct <- function (xy_table, ekstent){
  #Removing records outside the layer extent
  xy_table <- xy_table[xy_table[,1]>ekstent@xmin & xy_table[,1] < ekstent@xmax & xy_table[,2] < ekstent@ymax & xy_table[,2] > ekstent@ymin,]
  return (xy_table)
}
#wd####
#preques_folder<-paste("PreQUES_analysis_", T1,"_",T2,"_",PreQUES.index,sep="")
#quesb_dir<-paste(dirname(proj.file),"/QUES/QUES-B/", sep="")
#quesb_dir <- "D:/Sentinel_Landscape/Process/"
quesb_dir <- choose.dir()
sdm_dir <- "SDMod"
#setwd(result_dir)
#dir.create(preques_folder)
#output directory
#result_dir<-paste(result_dir,preques_folder, sep='')
#setwd(result_dir)

#user defined variables####
#WD is the directory containing the presence data (.csv) and directory of the covariate layers.####
setwd(quesb_dir)
if(!dir.exists(sdm_dir))dir.create(sdm_dir)
setwd(sdm_dir)

#Checking QUES-B SDM logfile####
log <- "QB_SDM_log.txt"
if(file.exists(log)){
  qb_log <- read.table(log, header = T, stringsAsFactors = F)
  sdm_rep <- qb_log[nrow(qb_log),1]
  sdm_id <- sdm_rep+1
  addlog <- as.data.frame(cbind(sdm_id, as.character(Sys.time()), "NA", "NA"))
  colnames(addlog) <- colnames(qb_log)
  qb_log <- as.data.frame(rbind(qb_log, addlog))}else{sdm_id =1
  qb_log <- as.data.frame(cbind(sdm_id, as.character(Sys.time())))
  qb_log$End <- "NA"
  qb_log$odir <- "NA"
  colnames(qb_log)<- c("Repetition", "Start", "End", "out_dir")
  }
out <- paste("qb_sdm_ot_",sdm_id, sep = "")###INPUT; index showing how many runs have been made should be defined
qb_log[sdm_id,"out_dir"] <- out
write.table(qb_log, file = log, row.names = F)


#INPUT####
pres_dat <- readShapePoints(file.choose()) #nama csv koordinat nipah, #ZA10=CSV atau SHP Jambi dengan anam apa?

#[REV] pres_dat <- readShapePoints(file.choose())

cov_dir <- choose.dir()
cov_dir <- gsub("\\","/", cov_dir, fixed="TRUE")

ex_dir = ""
o_pred_filename<-"predict_map.tiff" #ZA11=dibiarkan tetap saja?
generate_map <- TRUE #binary T/F
# bg_cts <- 20000
fold <- 10
fin_eval_prop <- 30 #proportion (in %) of presence data and background points to be set aside for the final model evaluation


auc_thres <- 0.6
#extra_dir <- "indo" #for extrapolation purpose, the directory is not the same as the cov_dir


if(!dir.exists(out)) dir.create(out)

#A. Data Input####
#Import 1. Presence Data (points) & 2. Covariate Layers (raster) from source file(s)
#A1
#t_pres <- read.csv(pres_dat, header = T) #[REV] remove the command



#THE MOST IMPORTANT THING IS TO ENSURE THAT THE CSV FILE CONTAINS COORDINATES OF SPECIES'PRESENCE LOCALITIES: indirect approach by letting the user define the columns
#containing the information regarding x and y coordinates of the species localities.
#ALSO NEED TO CHECK WHETHER THE COORDINATES ARE WITHIN THE STUDIED AREA WITH THE SAME PROJECTION SYSTEM.(later after the covariate layers have been loaded)


# assigning the total presence record into a variable called 'tpres'
pres <- as.data.frame(cbind(pres_dat$coords.x1, pres_dat$coords.x2))

colnames(pres) <- c("longitude","latitude") 
#standardized the column names
#excluding the first column containing the species (object) being modeled
# AE comment 7: field yang dipilih sebagai field koordinata x dan y harus berasal input user


group <- kfold(pres, k = fold)
#Define some of the presence data as training dataset, while the 20% as test dataset

#Covariate Data Input

if(cov_dir!=""){
  av_lyr <- list.files(cov_dir,pattern = ".tif", full.names = T) # only take .tif raster files into account
  av_lyr <- grep(".tif\\z", av_lyr, perl = T, value = T) 
  #specifically extract only the file names that end with pattern ".tif"
  
  #====Selecting which layer to be excluded and which contains categorical variables
  av_data <- as.data.frame(av_lyr)
  av_data$Notes <- "Souce: External dir."

}else {
  av_data <- ls_raster()
  colnames(av_data) <- c("av_lyr", "Notes")
}


av_data$include <- 1
av_data$cat <- 0
n <- nrow(av_data)

#(290216) AD: av_data is the variable to be edited when user wants to change model parameterization after assessing the generated model performance####
#repeat{
repeat{
  av_data<-edit(av_data)
  if(sum(av_data$include)!=0){
    break
  }
}

# AE comment 9: from user perspective, inclusion is always easier than exclusion, instead of choosing which layer to exclude
# I think it is much easier for user to choose which layer to be included


av_lyr <- as.data.frame(av_data[av_data$include==1,])
av_lyr$id <- 1:nrow(av_lyr)
cat_id <- av_lyr[av_lyr$cat==1,'id']
av_lyr$call<- NA
row_ext <- grep (".tif\\z", av_lyr$av_lyr, perl = T)
for(i in 1: nrow(av_lyr)){
  if(i %in% row_ext) av_lyr$call[i] <- paste("raster('",av_lyr[i,1],"')", sep = "") else av_lyr$call[i]<- paste(av_lyr[i, 1])
}
av_lyr <- as.character(av_lyr[,'call'])
#cat_lyr <- as.character(av_data[av_data$cat==1,1])
#for(i in 1:length(cat_lyr)){
#  if (i==1){
#  cat_nm <- as.character(cat_lyr[i])
#  }else cat_nm <- as.character(paste(cat_nm, cat_lyr[i], sep = ","))
#}

#Assessing the sameness of raster extent by matrix####
identic.matrx <- data.frame()

#lyr_id <- 1:length(av_lyr)
#identic.matrx <- as.data.frame(lyr_id)
#identic.matrx$a <- 1:9

g <- data.frame()
for(a in 1: length(av_lyr)){
  for(b in 1:length(av_lyr)){
    eval(parse(text=paste("if(identical(extent(",av_lyr[a],"),extent(",av_lyr[b],")))identic.matrx[a,b] <- 'TRUE' else {identic.matrx[a,b] <- 'FALSE'
                          g <- rbind(g, c(a,b))
  }", sep = "")))
    }
}
colnames(identic.matrx)<- 1:length(av_lyr)

if(identical(dim(g), as.integer(c(0,0)))) {print("The input layers have the same spatial extent and resolution. Continuing process..")} else {
  print("At least one of the input layers has unmatched spatial extent and/or resolution. Adjusting extent...")
  print(c("Unmatched record found:\t"))
  #display the unmatched pair while also displaying the table to be edit>>after that, the edit table as input for the spatial adjustment.
  edit(identic.matrx)
  id <- 1:length(av_lyr)
  l_edit <- as.data.frame(cbind(id,av_lyr))
  l_edit$basemap <- 0
  l_edit$adj <- 0
  repeat{
    l_edit<-edit(l_edit)
    if(sum(l_edit$basemap)==1 & sum(l_edit$adj)!=0){
      break
    }
  }
  bmp <- as.integer(as.vector(l_edit[l_edit$basemap==1,1]))
  adjst <- as.integer(as.vector(l_edit[l_edit$adj==1,1]))
  for(i in 1:length(adjst)){
    eval(parse(text=paste("adj_lyr_",adjst[i],"<-", av_lyr[adjst[i]], sep = "")))
    eval(parse(text=paste("adj_lyr_",adjst[i],"<-spatial_sync_raster(unsynced = adj_lyr_",adjst[i],", reference =",av_lyr[bmp],")", sep = "")))
    #replacing the unsynced layer in the av_lyr
    av_lyr[adjst[i]] <- as.character(paste("adj_lyr_",adjst[i], sep = ""))
  }
}


#Create a rasterstack from the cov. layers. Include looping to import all of the rasters in a single command

for (i in 1: length(av_lyr)){
  if(i==1)lyr_call <- av_lyr[1] else lyr_call <- paste(lyr_call,av_lyr[i], sep =",")
  #  lyr_call <- paste(lyr_call,",raster(av_lyr[",i,"])", sep = "")
}

eval(parse(text = paste("comp_lyr <- stack(",lyr_call,")", sep = "")))

# AE comment 10: mungkin perlu spatial_sync_raster command dulu untuk memastikan seluruh layer yang digunakan memiliki 
# resolusi, koordinat system dan coverage yang sama

#viewing the layers in maps, optional?
plot(comp_lyr, legend=F)

#saving the rasterstack, necessary?
#writeRaster(comp_lyr, filename="cov_lyrs.grd")


#creating random background (bg) points within the study area (study area defined as 'mask')####
ext <- extent(comp_lyr)

#AD 30/03 : entry the function to subset the presence based on the extent here
pres <- pt_correct(pres, ext) #ZA1:ERROR NEED INPUT DATA

# AE comment 11: extent bisa diambil dari : (1) Rdata-see comment3 ; (2) user defined input ; (3) extent minimum dari data yang dimasukkan
#set.seed(0)
bg <- readShapePoints(file.choose()) #ZA2:ERROR NEED INPUT DATA
bg <- as.data.frame(cbind(bg$coords.x1, bg$coords.x2)) #ZA3:ERROR NEED INPUT DATA


#divide the bg pts. into k number of groups randomly
#set.seed(0)
bgroup <- kfold(bg, k = fold) #ZA4:ERROR NEED INPUT DATA

#====ADAPTED FROM MOLECOLOGIST:Randomly subsample the data (perhaps to address the SAC)####


rep_auc <- numeric()
comp_t_e <- list()
comp_mod <- list()


#Evaluating the suitability of the method implemented using the measure of Area Under the Curve (AUC)
m_rep_auc <- 0.7

if(m_rep_auc > auc_thres){
  #subsetting the presence and background data for calibration and validation. (Default is 70 30 for calibration and validation, respectively)
  setside <- function(x, sid = sample(1:100000, 1)){
    x <- as.data.frame(x)
    set.seed(sid)
    sampled_id <- sample(1:nrow(x), round(fin_eval_prop/100*nrow(x)))
    uns_id <- setdiff(1:nrow(x), sampled_id)
    sampled <- x[sampled_id,]
    uns <- x[uns_id,]
    res <- list(sid, sampled, uns)
    names(res) <- c("seed", "sampled", "unsampled")
    return(res)
  }
  psence <- setside(pres)
  bgraun <- setside(bg)
  fin_mod <- maxent(comp_lyr, p= psence$unsampled, factors=cat_id,
                    a= bgraun$unsampled, path = out, removeDuplicates = T, args= c("-P","maximumiterations=2100",
                                                                                   "randomseed=True",
                                                                                   "-J",
                                                                                   "writeplotdata=True"
                    )) #"replicates=5" excluded
  e_fin <- evaluate(psence$sampled, bgraun$sampled, fin_mod, comp_lyr) #ZA9= e fin masih data sumsel
  
  # AE comment 16: Again, displaying intermediate result at this point is good for user 
}else print("The modeling method couldn't fit the data properly") #ZA5:ERROR NEED INPUT DATA

#Displaying the final model results####
browseURL(paste0(getwd(),"/",fin_mod@html)) #ZA6:ERROR NEED INPUT DATA
#edit(pop)


#processing the extrapolation rasetr layers (refer to extra_input_data.R)####
if(ex_dir!=""){
  ex_lyr <- list.files(ex_dir,pattern = ".tif", full.names = T) # only take .tif raster files into account
  ex_lyr <- grep(".tif\\z", ex_lyr, perl = T, value = T) 
  #specifically extract only the file names that end with pattern ".tif"
  
  
  #====Selecting which layer to be excluded and which contains categorical variables
  ex_data <- as.data.frame(ex_lyr)
  ex_data$Notes <- "Souce: External dir."
  #adding the existing raster files inside the .Rdata
  colnames(in_dat) <- colnames(ex_data)
  ex_data <- rbind(ex_data, in_dat) #the displayed list is the raster names instead of the variable names
  
  
  
  ex_data$include <- 1
  ex_data$cat <- 0
  n <- nrow(ex_data)
  
  #(290216) AD: ex_data is the variable to be edited when user wants to change model parameterization after assessing the generated model performance####
  repeat{
    ex_data<-edit(ex_data)
    if(sum(ex_data$include)!=0){
      break
    }
  }
  
  ex_lyr <- as.data.frame(ex_data[ex_data$include==1,])
  ex_lyr$id <- 1:nrow(ex_lyr)
  cat_id <- ex_lyr[ex_lyr$cat==1,'id']
  ex_lyr$call<- NA
  row_ext <- grep (".tif\\z", ex_lyr$ex_lyr, perl = T)
  for(i in 1: nrow(ex_lyr)){
    if(i %in% row_ext) ex_lyr$call[i] <- paste("raster('",ex_lyr[i,1],"')", sep = "") else ex_lyr$call[i]<- paste(ex_lyr[i, 1])
  }
  ex_lyr <- as.character(ex_lyr[,'call'])
  
  #Assessing the sameness of raster extent by matrix####
  identic.matrx <- data.frame()
  
  g <- data.frame()
  for(a in 1: length(ex_lyr)){
    for(b in 1:length(ex_lyr)){
      eval(parse(text=paste("if(identical(extent(",ex_lyr[a],"),extent(",ex_lyr[b],")))identic.matrx[a,b] <- 'TRUE' else {identic.matrx[a,b] <- 'FALSE'
                            g <- rbind(g, c(a,b))
    }", sep = "")))
    }
  }
  colnames(identic.matrx)<- 1:length(ex_lyr)
  
  if(identical(dim(g), as.integer(c(0,0)))) {print("The input layers have the same spatial extent and resolution. Continuing process..")} else {
    print("At least one of the input layers has unmatched spatial extent and/or resolution. Adjusting extent...")
    print(c("Unmatched record found:\t"))
    #display the unmatched pair while also displaying the table to be edit>>after that, the edit table as input for the spatial adjustment.
    edit(identic.matrx)
    id <- 1:length(ex_lyr)
    l_edit <- as.data.frame(cbind(id,ex_lyr))
    l_edit$basemap <- 0
    l_edit$adj <- 0
    repeat{
      l_edit<-edit(l_edit)
      if(sum(l_edit$basemap)==1 & sum(l_edit$adj)!=0){
        break
      }
    }
    bmp <- as.integer(as.vector(l_edit[l_edit$basemap==1,1]))
    adjst <- as.integer(as.vector(l_edit[l_edit$adj==1,1]))
    for(i in 1:length(adjst)){
      eval(parse(text=paste("adj_lyr_",adjst[i],"<-", ex_lyr[adjst[i]], sep = "")))
      eval(parse(text=paste("adj_lyr_",adjst[i],"<-spatial_sync_raster(unsynced = adj_lyr_",adjst[i],", reference =",ex_lyr[bmp],")", sep = "")))
      #replacing the unsynced layer in the ex_lyr
      ex_lyr[adjst[i]] <- as.character(paste("adj_lyr_",adjst[i], sep = ""))
    }
  }
  #Create a rasterstack from the cov. layers. Include looping to import all of the rasters in a single command
  for (i in 1: length(ex_lyr)){
    if(i==1)lyr_call <- ex_lyr[1] else lyr_call <- paste(lyr_call,ex_lyr[i], sep =",")
    #  lyr_call <- paste(lyr_call,",raster(ex_lyr[",i,"])", sep = "")
  }
  
  eval(parse(text = paste("comp_ex_lyr <- stack(",lyr_call,")", sep = "")))
  
  #plot(comp_ex_lyr, legend=F)
  
  
  ex_ext <- extent(comp_ex_lyr)
  
  #creating the table to match extrapolation layer names with the calibration layer names####
  match <- as.data.frame(names(comp_ex_lyr), stringsAsFactors=F)
  for(i in 1: length(names(comp_lyr))){
    eval(parse(text = paste0("match$",names(comp_lyr)[i]," <- 0")))
  }
  colnames(match)[1]<- "extrapolate\\calibration"
  repeat{
    match <- edit(match)
    indicat <- integer()
    indic <- integer()
    for(i in 2: ncol(match)) indicat <- c(indicat, sum(match[,i]))
    for(i in 1: nrow(match)) indic <- c(indic, sum(match[i,2:ncol(match)]))
    if(unique(indicat)==1 & unique(indic)==1) break
  }
  
  c <- vector(length = nrow(match))
  for(i in 1:nrow(match)){
    names(comp_ex_lyr)[i]<- names(comp_lyr)[grep(1,match[i,2:ncol(match)])]
    c[grep(1,match[i,2:ncol(match)])] <- i
  }
  for(i in 1:nrow(match)) names(comp_ex_lyr)[i]<- names(comp_lyr)[grep(1,match[i,2:ncol(match)])] #duplicated due to some bugs within R.
  
  com_ex_lyr <- subset(comp_ex_lyr, order(c)) #cannot subset rasterstack into the same variable. hence, the var name has to be changed
    }


#Generating prediction map####

if(generate_map){if(ex_dir=="") px <- predict(comp_lyr, fin_mod, ext=ext, progress='', filename =paste0(out,"/",o_pred_filename)) else px <- predict(com_ex_lyr, fin_mod, ext=ex_ext, progress='', filename =paste0(out,"/",o_pred_filename))} # only conducted when user has been satisfied with the results
data("wrld_simpl") #end processing
par(mfrow=c(1,2))
plot(px, main='Maxent, raw values') #ZA7:ERROR NEED INPUT DATA
plot(wrld_simpl, add=TRUE, border="dark grey") #ZA8:ERROR NEED INPUT DATA

tr <- threshold(e_fin, 'spec_sens')
plot(px > tr, main='presence/absence') 

plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')


#Writing the end time of the processing at the logfile####
en <- as.character(Sys.time())
qb_log[sdm_id, "End"] <- en
write.table(qb_log, file = log, row.names = F)

#resaving necessary variables into the .lpj####
QUESB.sdm.index <-sdm_id
#resave(QUESB.sdm.index, file = proj.file)
###END

