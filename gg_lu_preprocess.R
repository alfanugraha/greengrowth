#######################################################################
## gg_lu_preprocess.R                                                 #
## ------------------                                                 #
## Date              : February, 2019                                 #
## Author            : Alfa Nugraha                                   #
## Script purpose    : List R library for GG simulation.              #                 
## All included packages are installed in R-3.5.1 version or above    #                                             
##                                                                    #
##                                                                    #
##                                                                    #
##                                                                    #
#######################################################################

#Output: Peta rawan kebakaran (Raster dengan nilai continous)

#Outlines:
#memory allocation####
options(java.parameters = "-Xmx8g")

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
out <- paste("qb_sdm_ot_",sdm_id, sep = "") ###INPUT; index showing how many runs have been made should be defined
qb_log[sdm_id,"out_dir"] <- out
write.table(qb_log, file = log, row.names = F)


ex_dir = ""
o_pred_filename<-"predict_map.tif" 
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

# assigning the total presence record into a variable called 'tpres'
pres <- as.data.frame(cbind(pres_dat$coords.x1, pres_dat$coords.x2))

colnames(pres) <- c("longitude","latitude") 
#standardized the column names
#excluding the first column containing the species (object) being modeled
# AE comment 7: field yang dipilih sebagai field koordinat x dan y harus berasal input user

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


av_lyr <- as.data.frame(av_data[av_data$include==1,])
av_lyr$id <- 1:nrow(av_lyr)
cat_id <- av_lyr[av_lyr$cat==1,'id']
av_lyr$call<- NA
row_ext <- grep (".tif\\z", av_lyr$av_lyr, perl = T)
for(i in 1: nrow(av_lyr)){
  if(i %in% row_ext) 
    av_lyr$call[i] <- paste("raster('",av_lyr[i,1],"')", sep = "") 
  else 
    av_lyr$call[i]<- paste(av_lyr[i, 1])
}
av_lyr <- as.character(av_lyr[,'call'])

#Assessing the sameness of raster extent by matrix####
identic.matrx <- data.frame()
g <- data.frame()
for(a in 1: length(av_lyr)){
  for(b in 1:length(av_lyr)){
    eval(parse(text=paste("fire_check_extent<-identical(extent(",av_lyr[a],"), extent(",av_lyr[b],"))")))
    if(fire_check_extent){
      identic.matrx[a,b] <- 'TRUE'
    } else {
      identic.matrx[a,b] <- 'FALSE'
      g <- rbind(g, c(a,b))
    }
  }
}
colnames(identic.matrx)<- 1:length(av_lyr)

# if(identical(dim(g), as.integer(c(0,0)))) {
#   print("The input layers have the same spatial extent and resolution. Continuing process..")
# } else {
#   print("At least one of the input layers has unmatched spatial extent and/or resolution. Adjusting extent...")
#   print(c("Unmatched record found:\t"))
#   #display the unmatched pair while also displaying the table to be edit>>after that, the edit table as input for the spatial adjustment.
#   edit(identic.matrx)
#   id <- 1:length(av_lyr)
#   l_edit <- as.data.frame(cbind(id,av_lyr))
#   l_edit$basemap <- 0
#   l_edit$adj <- 0
#   repeat{
#     l_edit<-edit(l_edit)
#     if(sum(l_edit$basemap)==1 & sum(l_edit$adj)!=0){
#       break
#     }
#   }
#   bmp <- as.integer(as.vector(l_edit[l_edit$basemap==1,1]))
#   adjst <- as.integer(as.vector(l_edit[l_edit$adj==1,1]))
#   for(i in 1:length(adjst)){
#     eval(parse(text=paste("adj_lyr_",adjst[i],"<-", av_lyr[adjst[i]], sep = "")))
#     eval(parse(text=paste("adj_lyr_",adjst[i],"<-spatial_sync_raster(unsynced = adj_lyr_",adjst[i],", reference =",av_lyr[bmp],")", sep = "")))
#     #replacing the unsynced layer in the av_lyr
#     av_lyr[adjst[i]] <- as.character(paste("adj_lyr_",adjst[i], sep = ""))
#   }
# }


#Create a rasterstack from the cov. layers. Include looping to import all of the rasters in a single command
for (i in 1: length(av_lyr)){
  if(i==1)
    lyr_call <- av_lyr[1] 
  else 
    lyr_call <- paste(lyr_call,av_lyr[i], sep =",")
  #  lyr_call <- paste(lyr_call,",raster(av_lyr[",i,"])", sep = "")
}
eval(parse(text = paste("comp_lyr <- stack(",lyr_call,")", sep = "")))

# AE comment 10: mungkin perlu spatial_sync_raster command dulu untuk memastikan seluruh layer yang digunakan memiliki 
# resolusi, koordinat system dan coverage yang sama

# plot(comp_lyr, legend=F)

#saving the rasterstack, necessary?
#writeRaster(comp_lyr, filename="cov_lyrs.grd")


#creating random background (bg) points within the study area (study area defined as 'mask')####
ext <- extent(comp_lyr)

#AD 30/03 : entry the function to subset the presence based on the extent here
pres <- pt_correct(pres, ext) 

# AE comment 11: extent bisa diambil dari : (1) Rdata-see comment3 ; (2) user defined input ; (3) extent minimum dari data yang dimasukkan
#set.seed(0)
bg <- as.data.frame(cbind(bg$coords.x1, bg$coords.x2)) 

#divide the bg pts. into k number of groups randomly
#set.seed(0)
bgroup <- kfold(bg, k = fold) 

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
  fin_mod <- maxent(
    comp_lyr, 
    p = psence$unsampled, 
    factors = cat_id, 
    a = bgraun$unsampled, 
    path = out, 
    removeDuplicates = T,
    args = c("-P", "maximumiterations=2100", "randomseed=True", "-J", "writeplotdata=True")
  ) #"replicates=5" excluded
  e_fin <- evaluate(psence$sampled, bgraun$sampled, fin_mod, comp_lyr)
  
  # AE comment 16: Again, displaying intermediate result at this point is good for user 
} else { 
  print("The modeling method couldn't fit the data properly") 
}
  
#Displaying the final model results####
browseURL(paste0(getwd(),"/",fin_mod@html)) 
#edit(pop)

#Generating prediction map####
if(generate_map){
  if(ex_dir=="") 
    px <- predict(comp_lyr, fin_mod, ext=ext, progress='', filename=paste0(out,"/",o_pred_filename)) 
  else
    px <- predict(com_ex_lyr, fin_mod, ext=ex_ext, progress='', filename =paste0(out,"/",o_pred_filename))  # only conducted when user has been satisfied with the results
}
data("wrld_simpl") #end processing
par(mfrow=c(1,2))
plot(px, main='Maxent, raw values') 
plot(wrld_simpl, add=TRUE, border="dark grey") 

tr <- threshold(e_fin, 'spec_sens')
plot(px > tr, main='presence/absence') 

plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres, pch='+')


#Writing the end time of the processing at the logfile####
en <- as.character(Sys.time())
qb_log[sdm_id, "End"] <- en
write.table(qb_log, file = log, row.names = F)

#resaving necessary variables into the .lpj####
# QUESB.sdm.index <-sdm_id
#resave(QUESB.sdm.index, file = proj.file)
###END

