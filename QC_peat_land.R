#library####
library(rtf)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(rgeos)
library(grid)
library(jsonlite)
library(RPostgreSQL)
library(DBI)
library(rpostgis)
library(spatial.tools)
library(rgdal)
library(plyr)
library(tiff)
library(reshape2)
library(foreign)
library(splitstackshape)
library(magick)
library(vegan)
library(RSQLite)
library(SDMTools)
library(dplyr)
library(gridExtra)
library(pracma)
library(zoo)
library(tidyverse)
library(mschart)
library(officer)

# PreQUES # QUES_C process should be refer from PreQUES result ####

## PreQUES (NOT TO BE COMPILED! FOR TESTING PURPOSE ONLY)####################################################################################################################################################

## define initial file
working_directory="F:/GGP/Jambi/Result/"
planning_unit="F:/GGP/Jambi/raster/PU/plan_unit_jambi_final.tif"
lookup_lc="F:/GGP/Jambi/table/Land_cover_legend_jambi.csv"
lookup_z="F:/GGP/Jambi/table/PU_jambi_final.csv"
##Analysis_option=selection All analysis; Perubahan dominan di tiap zona; Dinamika perubahan di tiap zona (Alpha-Beta); Analisis alur perubahan (Trajectory)

list_of_files <- list.files("F:/GGP/Jambi/raster/", pattern = ".tif$")
num_of_files <- length(list_of_files)

Analysis_option=1
raster.nodata=0

analysis.option<-Analysis_option
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

Spat_res=1

# Set working directory
pu_name<-"plan_unit"
idx_PreQUES<-1 # ADreview: idx shall be retreived from the proj.file and then added by 1 as the process runs
result_dir<-working_directory
dir.create(result_dir)

## load tabular data
# planning unit
lookup_z<-read.table(lookup_z, header = T, sep = ",")
# land cover
lookup_lc<-read.table(lookup_lc, header = T, sep = ",")
zone <- raster(planning_unit)
# landcover lookup table
lookup_l<-lookup_lc
lookup_lc<-lookup_lc
colnames(lookup_l)<-c("ID", "CLASS")
colnames(lookup_lc)<-c("ID", "CLASS")

for(d in 1:(num_of_files-1)){
  T1<-as.numeric(substr(list_of_files[d], 3, 6))
  T2<-as.numeric(substr(list_of_files[d+1], 3, 6))
  
  landuse_1 <- paste0("F:/GGP/Jambi/raster/", list_of_files[d])
  landuse_2 <- paste0("F:/GGP/Jambi/raster/", list_of_files[d+1])
  
  # landuse first time period
  landuse1<-raster(landuse_1)
  # landuse second time period
  landuse2<-raster(landuse_2)
  
  nLandCoverId<-nrow(lookup_lc)
  nPlanningUnitId<-nrow(lookup_z)
  
  dummy1<-data.frame(nPU=lookup_z$ID, divider=nLandCoverId*nLandCoverId)
  dummy1<-expandRows(dummy1, 'divider')
  
  dummy2<-data.frame(nT1=lookup_lc$ID, divider=nLandCoverId)
  dummy2<-expandRows(dummy2, 'divider')
  dummy2<-data.frame(nT1=rep(dummy2$nT1, nPlanningUnitId))
  
  dummy3<-data.frame(nT2=rep(rep(lookup_lc$ID, nLandCoverId), nPlanningUnitId))
  
  landUseChangeMapDummy<-cbind(dummy1, dummy2, dummy3)
  colnames(landUseChangeMapDummy)<-c('ZONE', 'ID_LC1', 'ID_LC2')
  
  #=Create cross-tabulation
  R<-(zone*1) + (landuse1*100^1) + (landuse2*100^2)
  lu.db<-as.data.frame(freq(R))
  lu.db<-na.omit(lu.db)
  n<-3
  k<-0
  lu.db$value_temp<-lu.db$value
  while(k < n) {
    eval(parse(text=(paste("lu.db$Var", n-k, "<-lu.db$value_temp %% 100", sep=""))))  
    lu.db$value_temp<-floor(lu.db$value_temp/100)
    k=k+1
  }
  lu.db$value_temp<-NULL
  #lu.db$value<-NULL
  colnames(lu.db)=c('ID_CHG', 'COUNT', 'ZONE', 'ID_LC1', 'ID_LC2')
  lu.db<-merge(landUseChangeMapDummy, lu.db, by=c('ZONE', 'ID_LC1', 'ID_LC2'), all=TRUE)
  lu.db$ID_CHG<-lu.db$ZONE*1 + lu.db$ID_LC1*100^1 + lu.db$ID_LC2*100^2
  lu.db<-replace(lu.db, is.na(lu.db), 0)
  
  result_dir<-paste0("F:/GGP/Jambi/Result/PreQUES_", T1, "_",  T2)
  dir.create(result_dir)
  setwd(result_dir)
  chg_map<-tolower(paste('chgmap_', pu_name, T1,'_', T2, sep=''))
  eval(parse(text=(paste("writeRaster(R, filename='", chg_map, ".tif', format='GTiff', overwrite=TRUE)", sep="")))) # ADreview: what does 'R' stand for
  
  #=Create individual table for each landuse map
  # set area and classified land use/cover for first landcover and second
  freqLanduse_1<-data.frame(freq(landuse1))
  area_lc1<-freqLanduse_1
  colnames(area_lc1) = c("ID", "COUNT_LC1") 
  area_lc1<-merge(area_lc1, lookup_l, by="ID")
  
  freqLanduse_2<-data.frame(freq(landuse2))
  area_lc2<-freqLanduse_2
  colnames(area_lc2) = c("ID", "ZONE")
  area_lc2<-merge(area_lc2,lookup_l,by="ID")
  
  # combine all data in data_merge
  colnames(lookup_l)[1]="ID_LC1"
  colnames(lookup_l)[2]="LC_t1"
  data_merge <- merge(lu.db,lookup_l,by="ID_LC1")
  colnames(lookup_l)[1]="ID_LC2"
  colnames(lookup_l)[2]="LC_t2"
  data_merge <- as.data.frame(merge(data_merge,lookup_l,by="ID_LC2"))
  colnames(lookup_z)[1]="ZONE"
  # colnames(lookup_z)[2]="COUNT_ZONE"
  colnames(lookup_z)[2]="Z_NAME"
  data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
  data_merge$COUNT<-data_merge$COUNT*Spat_res
  colnames(lookup_l)<-c("ID", "CLASS")
  colnames(lookup_z)<-c("ID", "ZONE")
  area_zone<-lookup_z
  #area<-min(sum(area_zone[,2]), sum(area_lc1[,2]), sum(area_lc2[,2]))
  data_merge_sel <- data_merge[ which(data_merge$COUNT > 0),]
  data_merge_sel$LU_CHG <- do.call(paste, c(data_merge_sel[c("LC_t1", "LC_t2")], sep = " to "))
  
  lg_chg <- data_merge_sel
  lg_chg$ID1<-as.numeric(as.character((lg_chg$ID_LC1)))
  lg_chg$ID2<-as.numeric(as.character((lg_chg$ID_LC2)))
  lg_chg$IDC<-lg_chg$ID1-lg_chg$ID2
  lg_chg<-lg_chg[ which(lg_chg$IDC!=0),]
  lg_chg <- as.data.frame(lg_chg[order(-lg_chg$COUNT),])
  lg_chg$CHG_CODE<-as.factor(toupper(abbreviate(lg_chg$LU_CHG, minlength=5, strict=FALSE, method="both")))
  lg_chg$ID1<-lg_chg$ID2<-lg_chg$IDC<-NULL
  # top ten changes
  lg_chg_top<-head(lg_chg, n=10)
  lg_chg_top$LC_t1<-lg_chg_top$LC_t2<-NULL
  # summary of landuse dominant change
  chg_only<-aggregate(COUNT~LU_CHG,data=lg_chg,FUN=sum)
  chg_only$CHG_CODE<-as.factor(toupper(abbreviate(chg_only$LU_CHG, minlength=5, strict=FALSE, method="both")))
  chg_only<-chg_only[order(-chg_only$COUNT),]
  chg_only<-chg_only[c(1,3,2)]
  # top ten dominant changes based on landuse/cover change
  chg_only_top<-head(chg_only, n=10)
  
  # summary of zonal dominant change
  lg_chg_zonal<-as.data.frame(NULL)
  for(l in 1:length(area_zone$ID)){
    tryCatch({
      a<-(area_zone$ID)[l]
      lg_chg_z<-lg_chg
      lg_chg_z<-as.data.frame(lg_chg_z[which(lg_chg_z$ZONE == a),])
      lg_chg_z<-aggregate(COUNT~ZONE+LU_CHG,data=lg_chg_z,FUN=sum)
      lg_chg_z$CHG_CODE<-as.factor(toupper(abbreviate(lg_chg_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
      lg_chg_z<-lg_chg_z[order(-lg_chg_z$COUNT),]
      lg_chg_z<-lg_chg_z[c(1,2,4,3)]
      #top ten dominant changes based on planning unit
      lg_chg_z_10<-head(lg_chg_z,n=10)
      lg_chg_zonal<-rbind(lg_chg_zonal,lg_chg_z_10)
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  # produce chart of largest source of changes in landuse
  location="Jambi"
  colnames(chg_only_top)[3]<-"COUNT"
  Largest.chg<- ggplot(data=chg_only_top, aes(x=reorder(CHG_CODE, -COUNT),y=COUNT, fill=CHG_CODE))+geom_bar(stat='identity',position='dodge')+
    geom_text(data=chg_only_top, aes(x=CHG_CODE, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
    ggtitle(paste("10 Perubahan Tutupan Lahan Dominan di", location, T1,"-",T2 )) +
    labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)') + guides(fill=FALSE)+
    theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  write.table(data_merge, file = paste0('chgtbl_', T1,'_', T2,'.csv'), sep = ",", row.names = F)
  setwd(working_directory)
}

preques_ppt <- read_pptx()
tes <- ms_barchart(data = chg_only_top, x = "CHG_CODE", y = "COUNT")
preques_ppt <- add_slide(preques_ppt, layout = "Title and Content", master = "Office Theme") 
ph_with_chart(preques_ppt, tes)

print(preques_ppt, target=paste0(working_directory, "/preques_ppt_chart2.pptx"))

#save project file
proj.file<-paste(working_directory, "PreQUES.lpj", sep="")
save.image(file = proj.file)


# QUES_C ####

# Setting up basic database parameters ####
# select the .lpj based on which the project shall be executed
proj.file <- "F:/GGP/Jambi/Result/PreQUES.lpj"
load(proj.file)
idx_QUES_C<-1 # # ADreview: idx shall be retreived from the proj.file and then added by 1 as the process runs

## Define initial file
cstock="F:/GGP/jambi/table/c_stock.csv"
em_peat="F:/GGP/jambi/table/faktor_emisi_peat.csv"

# Set result directory
result_dir<-paste0("F:/GGP/Jambi/Result/QUESC/")
dir.create(result_dir)
setwd(result_dir)

# To retrieve ref data
Adm="F:/GGP/jambi/raster/Adm/Jambi_kab.tif"
lut_ref="F:/GGP/jambi/table/Jambi_adm.csv"
lut_ref<-read.table(lut_ref, header = T, sep = ",")
nrefId<-nrow(lut_ref)
ref<-raster(Adm)

if (pu_name=="ref") {
  names(lookup_z) <- c("ID", "ZONE")
} else {
  names(lookup_z) <- c("ID", "ZONE")
  lookup_z <- lookup_z[ , c(1,2)]
}

# extract peat planning unit IDs ####
peat_keyword <- "eat"
peat_puID <- lookup_z[grep(peat_keyword, lookup_z[,2]) , 1]
cstock<-read.table(cstock, header = T, sep = ",")
em_peat<-read.table(em_peat, header = T, sep = ",")

# lut_ref has been available in the proj.file
# setting the combined raster layer
ref <- ref*10^6

# chgmap list
list_of_chgmap <- list.files("F:/GGP/Jambi/raster/Chgmap/", pattern = ".tif$")
num_of_chgmap <- length(list_of_chgmap)

# LOOPING FRAMEWORK FOR EACH TIMESTEP====
for(ts in 1: (num_of_chgmap)){
  
  # Setting periode for QUES_C
  T1<-as.numeric(substr(list_of_files[ts], 3, 6))
  T2<-as.numeric(substr(list_of_files[ts+1], 3, 6))

  # Eval(parse(text=(paste(list_of_chgmap, overwrite=TRUE, sep=""))))
  ti1<-as.numeric(substr(list_of_files[ts], 3, 6))
  ti2<-as.numeric(substr(list_of_files[ts+1], 3, 6))
  d_ti <- ti2-ti1 # delta year
  
  # Combine with admin data
  chg_map<- paste0("F:/GGP/Jambi/raster/Chgmap/", list_of_chgmap[ts])
  chg_map<- raster(chg_map)
  ch_map <- ref+chg_map
  
  # Retrieve freq
  fr_chMap <- data.frame(freq(ch_map, useNA = "no"), stringsAsFactors= FALSE)
  
  # Disaggregate using substr # note that the result will be presented as character. Do convert into numeric
  fr_chMap$IDADM <- floor(fr_chMap$value / 10^6)
  fr_chMap$ID_Z <- fr_chMap$value%%10^2
  fr_chMap$ID_T1 <- floor(fr_chMap$value%%10^4/10^2)
  fr_chMap$ID_T2 <- floor(fr_chMap$value%%10^6/10^4)
  
  # Define new 'hectare' column
  fr_chMap <- fr_chMap %>% mutate(hectares = count* res(ref)[1]^2/10000)
  
  # Carbon merge t1-t2
  for(w in 1:2){
    if(w ==1) orNames <- names(cstock)
    names(cstock)[1] <-paste0("ID_T", w)
    names(cstock)[2] <-paste0("LC_", w)
    names(cstock)[3] <- paste0("c_", w)
    fr_chMap <- merge(fr_chMap, cstock, by= paste0("ID_T", w), all.x = TRUE)
    if(w ==2) names(cstock) <- orNames
  }
  
  # Calculate emission
  fr_chMap$Em_co2Eq <- (fr_chMap$c_1 - fr_chMap$c_2)*fr_chMap$count*res(ref)[1]^2/10000 * 3.67
  fr_chMap$Seq <- 0
  fr_chMap[fr_chMap$Em_co2Eq < 0, "Seq"] <- -1* fr_chMap[fr_chMap$Em_co2Eq < 0, "Em_co2Eq"]
  fr_chMap[fr_chMap$Seq > 0, "Em_co2Eq"] <- 0 # correcting negative emission
  
  # Peat_em merge t1-t2
  rec_table <- fr_chMap[, c("value", "Em_co2Eq", "Seq", "count")]
  rec_table$Em_px <- rec_table$Em_co2Eq/rec_table$count
  rec_table$Seq_px <- rec_table$Seq/rec_table$count
  
  # QUES_C dir
  result_dir<-paste0("F:/GGP/Jambi/Result/QUESC/QUESC_", ti1, "_",  ti2)
  dir.create(result_dir)
  setwd(result_dir)
  
  # Peat_puID contains numbers which stand for id of pu containing peat
  for(w in 1:2){
    em_peatMod <- em_peat[, c(1, 3)]
    em_peatMod[, 2] <- em_peatMod[, 2]*d_ti/2
    names(em_peatMod)[1] <-paste0("ID_T", w)
    names(em_peatMod)[2] <- paste0("EmPeat_", w)
    fr_chMap <- merge(fr_chMap, em_peatMod, by= paste0("ID_T", w), all.x = TRUE)
  }
  
  # Correction for peat_em which falls in non-peat planning unit (*0)
  fr_chMap[which(!fr_chMap$ID_Z %in% peat_puID), c("EmPeat_1", "EmPeat_2")] <- fr_chMap[which(!fr_chMap$ID_Z %in% peat_puID), c("EmPeat_1", "EmPeat_2")]*0
  
  # Calculate total peat_em
  fr_chMap[, c("EmPeat_1", "EmPeat_2")] <- fr_chMap[, c("EmPeat_1", "EmPeat_2")]*fr_chMap[, "count"]* res(ref)[1]^2/10000 # conversion factor to hectare
  fr_chMap$EmPeatTot <- fr_chMap$EmPeat_1 + fr_chMap$EmPeat_2
  
  # Legend merge using lookup tables: pu, admin
  fr_chMap <- merge(fr_chMap, lut_ref, by = "IDADM", all.x = TRUE)
  colnames(lookup_z) = c("ID_Z", "ZONE")
  fr_chMap <- merge(fr_chMap, lookup_z, by = "ID_Z", all.x =TRUE)
  
  # Add period annotation
  fr_chMap$PERIOD <- paste0(ti1, "-", ti2)
  
  # Store at master table, in .csv, rbind with previous runs
  if(ts == 1) carb_compile <- fr_chMap else carb_compile <- data.frame(rbind(carb_compile, fr_chMap), stringsAsFactors = FALSE)
 
  # Extract carbon compile per period
  carb_compile_period <- data.frame(fr_chMap, stringsAsFactors = FALSE)
  
  # Calculate Total_Em & net_em
  carb_compile_period$Tot_Em <- carb_compile_period$Em_co2Eq + carb_compile_period$EmPeatTot
  carb_compile_period$Nett_Em <- carb_compile_period$Tot_Em - carb_compile_period$Seq
  write.table(carb_compile_period, file = paste0("emission_LCdynamics_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  
  # Sort preparation process
  carb_compile_period_sel <- carb_compile_period[ which(carb_compile_period$count > 0),]
  carb_compile_period_sel$LU_CHG <- do.call(paste, c(carb_compile_period_sel[c("LC_1", "LC_2")], sep = " to "))
  Cb_chg <- carb_compile_period_sel
  Cb_chg$ID1<-as.numeric(as.character((Cb_chg$ID_T1)))
  Cb_chg$ID2<-as.numeric(as.character((Cb_chg$ID_T2)))
  Cb_chg$IDC<-Cb_chg$ID_T1-Cb_chg$ID_T2
  Cb_chg<-Cb_chg[ which(Cb_chg$IDC!=0),]
  
  # Summary top ten seq
  Sq_chg <- as.data.frame(Cb_chg[order(-Cb_chg$Seq),])
  Sq_chg$CHG_CODE<-as.factor(toupper(abbreviate(Sq_chg$LU_CHG, minlength=5, strict=FALSE, method="both")))
  Sq_chg$ID1<-Sq_chg$ID2<-Sq_chg$IDC<-NULL
  Sq_chg_top<-head(Sq_chg, n=10)
  Sq_chg_top$LC_t1<-Sq_chg_top$LC_t2<-NULL
  
  # Summary top ten em mineral  
  Em_chg <- as.data.frame(Cb_chg[order(-Cb_chg$Em_co2Eq),])
  Em_chg$CHG_CODE<-as.factor(toupper(abbreviate(Em_chg$LU_CHG, minlength=5, strict=FALSE, method="both")))
  Em_chg$ID1<-Em_chg$ID2<-Em_chg$IDC<-NULL
  Em_chg_top<-head(Em_chg, n=10)
  Em_chg_top$LC_t1<-Em_chg_top$LC_t2<-NULL  
  
  # Summary top ten em peat
  Empeat_chg <- as.data.frame(Cb_chg[order(-Cb_chg$EmPeatTot),])
  Empeat_chg$CHG_CODE<-as.factor(toupper(abbreviate(Empeat_chg$LU_CHG, minlength=5, strict=FALSE, method="both")))
  Empeat_chg$ID1<-Empeat_chg$ID2<-Empeat_chg$IDC<-NULL
  Empeat_chg_top<-head(Empeat_chg, n=10)
  Empeat_chg_top$LC_t1<-Empeat_chg_top$LC_t2<-NULL
  
  # Summary top ten Tot_Em
  Tot_Em_chg <- as.data.frame(Cb_chg[order(-Cb_chg$Tot_Em),])
  Tot_Em_chg$CHG_CODE<-as.factor(toupper(abbreviate(Tot_Em_chg$LU_CHG, minlength=5, strict=FALSE, method="both")))
  Tot_Em_chg$ID1<-Tot_Em_chg$ID2<-Tot_Em_chg$IDC<-NULL
  Tot_Em_chg_top<-head(Tot_Em_chg, n=10)
  Tot_Em_chg_top$LC_t1<-Tot_Em_chg_top$LC_t2<-NULL
  
  # Summary top ten Nett_Em
  Nett_Em_chg <- as.data.frame(Cb_chg[order(-Cb_chg$Nett_Em),])
  Nett_Em_chg$CHG_CODE<-as.factor(toupper(abbreviate(Nett_Em_chg$LU_CHG, minlength=5, strict=FALSE, method="both")))
  Nett_Em_chg$ID1<-Nett_Em_chg$ID2<-Nett_Em_chg$IDC<-NULL
  Nett_Em_chg_top<-head(Nett_Em_chg, n=10)
  Nett_Em_chg_top$LC_t1<-Nett_Em_chg_top$LC_t2<-NULL
  
  # Summary top ten Nett_Em_Seq
  Nett_Em_Seq_chg <- as.data.frame(Cb_chg[order(Cb_chg$Nett_Em),])
  Nett_Em_Seq_chg$CHG_CODE<-as.factor(toupper(abbreviate(Nett_Em_Seq_chg$LU_CHG, minlength=5, strict=FALSE, method="both")))
  Nett_Em_Seq_chg$ID1<-Nett_Em_Seq_chg$ID2<-Nett_Em_Seq_chg$IDC<-NULL
  Nett_Em_Seq_chg_top<-head(Nett_Em_Seq_chg, n=10)
  Nett_Em_Seq_chg_top$LC_t1<-Nett_Em_Seq_chg_top$LC_t2<-NULL
  
  write.table(Sq_chg_top, file = paste0("Perubahan_Seq_Tertinggi_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Em_chg_top, file = paste0("Perubahan_Em_Mineral_Tertinggi_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Empeat_chg_top, file = paste0("Perubahan_Em_Peat_Tertinggi_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Tot_Em_chg_top, file = paste0("Perubahan_Em_Total_Tertinggi_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Nett_Em_chg_top, file = paste0("Perubahan_Nett_Em_Total_Tertinggi_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Nett_Em_Seq_chg_top, file = paste0("Perubahan_Nett_Em_Seq_Total_Tertinggi_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  
  # Summary top per planning unit
  Seq_chg_zonal<-as.data.frame(NULL)
  Tot_Em_chg_zonal<-as.data.frame(NULL)
  Nett_Em_chg_zonal<-as.data.frame(NULL)
  Nett_Em_Seq_chg_zonal<-as.data.frame(NULL)
  
  for(ui in 1:length(lookup_z$ID_Z)){
    tryCatch({
      xf<-(lookup_z$ID_Z)[ui]
      #top ten Seq per planning unit
      Seq_chg_z<-Cb_chg
      Seq_chg_z<-as.data.frame(Seq_chg_z[which(Seq_chg_z$ID_Z == xf),])
      Seq_chg_z<-aggregate(Seq~ZONE+LU_CHG+ID_Z,data=Seq_chg_z,FUN=sum)
      Seq_chg_z$CHG_CODE<-as.factor(toupper(abbreviate(Seq_chg_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
      Seq_chg_z<-Seq_chg_z[order(-Seq_chg_z$Seq),]
      Seq_chg_z<-Seq_chg_z[c(1,2,4,3)]
      Seq_chg_z_10<-head(Seq_chg_z,n=10)
      Seq_chg_zonal<-rbind(Seq_chg_zonal,Seq_chg_z_10)
      #top ten Tot_Em per planning unit
      Tot_Em_chg_z<-Cb_chg
      Tot_Em_chg_z<-as.data.frame(Tot_Em_chg_z[which(Tot_Em_chg_z$ID_Z == xf),])
      Tot_Em_chg_z<-aggregate(Tot_Em~ZONE+LU_CHG+ID_Z,data=Tot_Em_chg_z,FUN=sum)
      Tot_Em_chg_z$CHG_CODE<-as.factor(toupper(abbreviate(Tot_Em_chg_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
      Tot_Em_chg_z<-Tot_Em_chg_z[order(-Tot_Em_chg_z$Tot_Em),]
      Tot_Em_chg_z<-Tot_Em_chg_z[c(1,2,4,3)]
      Tot_Em_chg_z_10<-head(Tot_Em_chg_z,n=10)
      Tot_Em_chg_zonal<-rbind(Tot_Em_chg_zonal,Tot_Em_chg_z_10)
      #top ten Nett_Em per planning unit
      Nett_Em_chg_z<-Cb_chg
      Nett_Em_chg_z<-as.data.frame(Nett_Em_chg_z[which(Nett_Em_chg_z$ID_Z == xf),])
      Nett_Em_chg_z<-aggregate(Nett_Em~ZONE+LU_CHG+ID_Z,data=Nett_Em_chg_z,FUN=sum)
      Nett_Em_chg_z$CHG_CODE<-as.factor(toupper(abbreviate(Nett_Em_chg_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
      Nett_Em_chg_z<-Nett_Em_chg_z[order(-Nett_Em_chg_z$Nett_Em),]
      Nett_Em_chg_z<-Nett_Em_chg_z[c(1,2,4,3)]
      Nett_Em_chg_z_10<-head(Nett_Em_chg_z,n=10)
      Nett_Em_chg_zonal<-rbind(Nett_Em_chg_zonal,Nett_Em_chg_z_10)
      #top ten Nett_Em_Seq per planning unit
      Nett_Em_Seq_chg_z<-Cb_chg
      Nett_Em_Seq_chg_z<-as.data.frame(Nett_Em_Seq_chg_z[which(Nett_Em_Seq_chg_z$ID_Z == xf),])
      Nett_Em_Seq_chg_z<-aggregate(Nett_Em~ZONE+LU_CHG+ID_Z,data=Nett_Em_Seq_chg_z,FUN=sum)
      Nett_Em_Seq_chg_z$CHG_CODE<-as.factor(toupper(abbreviate(Nett_Em_Seq_chg_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
      Nett_Em_Seq_chg_z<-Nett_Em_Seq_chg_z[order(Nett_Em_Seq_chg_z$Nett_Em),]
      Nett_Em_Seq_chg_z<-Nett_Em_Seq_chg_z[c(1,2,4,3)]
      Nett_Em_Seq_chg_z_10<-head(Nett_Em_Seq_chg_z,n=10)
      Nett_Em_Seq_chg_zonal<-rbind(Nett_Em_Seq_chg_zonal,Nett_Em_Seq_chg_z_10)
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  write.table(Seq_chg_zonal, file = paste0("Seq_Zone_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Tot_Em_chg_zonal, file = paste0("Tot_Em_Zone_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Nett_Em_chg_zonal, file = paste0("Nett_Em_Zone_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Nett_Em_Seq_chg_zonal, file = paste0("Nett_Em_Seq_Zone_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)

  # Summary top per admin
  Seq_chg_admin<-as.data.frame(NULL)
  Tot_Em_chg_admin<-as.data.frame(NULL)
  Nett_Em_chg_admin<-as.data.frame(NULL)
  Nett_Em_Seq_chg_admin<-as.data.frame(NULL)
  
  for(uj in 1:length(lut_ref$IDADM)){
    tryCatch({
      xg<-(lut_ref$IDADM)[uj]
      #top ten Seq per admin
      Seq_chg_a<-Cb_chg
      Seq_chg_a<-as.data.frame(Seq_chg_a[which(Seq_chg_a$IDADM == xg),])
      Seq_chg_a<-aggregate(Seq~ZONE+LU_CHG+IDADM,data=Seq_chg_a,FUN=sum)
      Seq_chg_a$CHG_CODE<-as.factor(toupper(abbreviate(Seq_chg_a$LU_CHG, minlength=5, strict=FALSE, method="both")))
      Seq_chg_a<-Seq_chg_a[order(-Seq_chg_a$Seq),]
      Seq_chg_a<-Seq_chg_a[c(1,2,4,3)]
      Seq_chg_a_10<-head(Seq_chg_a,n=10)
      Seq_chg_admin<-rbind(Seq_chg_admin,Seq_chg_a_10)
      #top ten Tot_Em per admin
      Tot_Em_chg_a<-Cb_chg
      Tot_Em_chg_a<-as.data.frame(Tot_Em_chg_a[which(Tot_Em_chg_a$IDADM == xg),])
      Tot_Em_chg_a<-aggregate(Tot_Em~ZONE+LU_CHG+IDADM,data=Tot_Em_chg_a,FUN=sum)
      Tot_Em_chg_a$CHG_CODE<-as.factor(toupper(abbreviate(Tot_Em_chg_a$LU_CHG, minlength=5, strict=FALSE, method="both")))
      Tot_Em_chg_a<-Tot_Em_chg_a[order(-Tot_Em_chg_a$Tot_Em),]
      Tot_Em_chg_a<-Tot_Em_chg_a[c(1,2,4,3)]
      Tot_Em_chg_a_10<-head(Tot_Em_chg_a,n=10)
      Tot_Em_chg_admin<-rbind(Tot_Em_chg_admin,Tot_Em_chg_a_10)
      #top ten Nett_Em per admin
      Nett_Em_chg_a<-Cb_chg
      Nett_Em_chg_a<-as.data.frame(Nett_Em_chg_a[which(Nett_Em_chg_a$IDADM == xg),])
      Nett_Em_chg_a<-aggregate(Nett_Em~ZONE+LU_CHG+IDADM,data=Nett_Em_chg_a,FUN=sum)
      Nett_Em_chg_a$CHG_CODE<-as.factor(toupper(abbreviate(Nett_Em_chg_a$LU_CHG, minlength=5, strict=FALSE, method="both")))
      Nett_Em_chg_a<-Nett_Em_chg_a[order(-Nett_Em_chg_a$Nett_Em),]
      Nett_Em_chg_a<-Nett_Em_chg_a[c(1,2,4,3)]
      Nett_Em_chg_a_10<-head(Nett_Em_chg_a,n=10)
      Nett_Em_chg_admin<-rbind(Nett_Em_chg_admin,Nett_Em_chg_a_10)
      #top ten Nett_Em_Seq per admin
      Nett_Em_Seq_chg_a<-Cb_chg
      Nett_Em_Seq_chg_a<-as.data.frame(Nett_Em_Seq_chg_a[which(Nett_Em_Seq_chg_a$IDADM == xg),])
      Nett_Em_Seq_chg_a<-aggregate(Nett_Em~ZONE+LU_CHG+IDADM,data=Nett_Em_Seq_chg_a,FUN=sum)
      Nett_Em_Seq_chg_a$CHG_CODE<-as.factor(toupper(abbreviate(Nett_Em_Seq_chg_a$LU_CHG, minlength=5, strict=FALSE, method="both")))
      Nett_Em_Seq_chg_a<-Nett_Em_Seq_chg_a[order(Nett_Em_Seq_chg_a$Nett_Em),]
      Nett_Em_Seq_chg_a<-Nett_Em_Seq_chg_a[c(1,2,4,3)]
      Nett_Em_Seq_chg_a_10<-head(Nett_Em_Seq_chg_a,n=10)
      Nett_Em_Seq_chg_admin<-rbind(Nett_Em_Seq_chg_admin,Nett_Em_Seq_chg_a_10)
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  # Generate thematics table
  write.table(Seq_chg_admin, file = paste0("Seq_admin_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Tot_Em_chg_admin, file = paste0("Tot_Em_admin_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Nett_Em_chg_admin, file = paste0("Nett_Em_admin_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  write.table(Nett_Em_Seq_chg_admin, file = paste0("Nett_Em_Seq_admin_", ti1, "_", ti2, ".csv"), sep = ",", row.names = FALSE)
  
  # Summarize calculation result following the template: Period; Gross Em; Seq; Nett abg em; peat em; Total em. Store as .csv
  if(ts == 1) {
    summary_table <- data.frame(PERIOD = paste0(ti1, "-", ti2), Count_area = sum(fr_chMap$count), G_Em = sum(fr_chMap$Em_co2Eq), Seq = sum(fr_chMap$Seq), P_Em = sum(fr_chMap$EmPeatTot), stringsAsFactors = FALSE)
    summary_table$NettAll <- summary_table$G_Em - summary_table$Seq + summary_table$P_Em
    summary_table$G_Em_per_Ha_yr <- summary_table$G_Em / d_ti / summary_table$Count_area
    summary_table$Seq_per_Ha_yr <- summary_table$Seq / d_ti / summary_table$Count_area
    summary_table$P_Em_per_Ha_yr <- summary_table$P_Em / d_ti / summary_table$Count_area
    summary_table$NettAll_per_Ha_yr <- summary_table$NettAll / d_ti / summary_table$Count_area
  } else {
    summary_add <- data.frame(PERIOD = paste0(ti1, "-", ti2), Count_area = sum(fr_chMap$count), G_Em = sum(fr_chMap$Em_co2Eq), Seq = sum(fr_chMap$Seq), P_Em = sum(fr_chMap$EmPeatTot), stringsAsFactors = FALSE)
    summary_add$NettAll <- summary_add$G_Em - summary_add$Seq + summary_add$P_Em
    summary_add$G_Em_per_Ha_yr <- summary_add$G_Em / d_ti / summary_add$Count_area
    summary_add$Seq_per_Ha_yr <- summary_add$Seq / d_ti / summary_add$Count_area
    summary_add$P_Em_per_Ha_yr <- summary_add$P_Em / d_ti / summary_add$Count_area
    summary_add$NettAll_per_Ha_yr <- summary_add$NettAll / d_ti / summary_add$Count_area
    summary_table <- data.frame(rbind(summary_table, summary_add), stringsAsFactors = FALSE)
  }
  
  # Generate QUES_C map
  Em_Mineral_map <- reclassify(ch_map, carb_compile_period_sel[, c("value", "Em_co2Eq")])
  writeRaster(Em_Mineral_map, paste0("Em_Mineral_map_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
  Seq_map <- reclassify(ch_map, carb_compile_period_sel[, c("value", "Seq")])
  writeRaster(Seq_map, paste0("Seq_map_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
  Em_Peat_map <- reclassify(ch_map, carb_compile_period_sel[, c("value", "EmPeatTot")])
  writeRaster(Em_Peat_map, paste0("Em_Peat_map_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
  Total_Em_map <- reclassify(ch_map, carb_compile_period_sel[, c("value", "Tot_Em")])
  writeRaster(Total_Em_map, paste0("Total_Em_map_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
  Nett_Em_map <- reclassify(ch_map, carb_compile_period_sel[, c("value", "Nett_Em")])
  writeRaster(Nett_Em_map, paste0("Nett_Em_map_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
  
  # SumTab \ends----
  if(ts == (length(num_of_chgmap))){
    summary_table$NettAll <- summary_table$G_Em - summary_table$Seq + summary_table$P_Em
  }
  
  # Produce figure Nett Emission
  location="Kota Sungai Penuh"
  colnames(Nett_Em_chg_a_10)[3]<-"Nett_Em"
  Largest_Nett_Em.chg_a<- ggplot(Nett_Em_chg_a_10, aes(x=reorder(LU_CHG, -Nett_Em),y=Nett_Em, fill=LU_CHG))+geom_bar(stat='identity',position='dodge')+
    geom_text(data=Nett_Em_chg_a_10, aes(x=LU_CHG, y=Nett_Em, label=round(Nett_Em, 1)),size=3, vjust=0.1) +
    ggtitle(paste("10 emisi bersih tertinggi", location, ti1,"-",ti2 )) +
    labs(x = 'Emisi bersih', y='Ton_CO2eq') + guides(fill=FALSE)+
    theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())

  working_directory<-paste0("F:/GGP/Jambi/Result/QUESC/")
  setwd(working_directory)
}

# Generate summary table
write.table(summary_table, file = paste0('summary_emission.csv'), sep = ",", row.names = FALSE)
write.table(carb_compile, file = paste0('emission_LCdynamics.csv'), sep = ",", row.names = FALSE)

# Print ppt
# QUES_C_ppt <- read_pptx()
# em_test <- ms_barchart(data = fr_chMap, x = "ZONE", y = "COUNT")
# QUES_C_ppt <- add_slide(QUES_C_ppt, layout = "Title and Content", master = "Office Theme")
# ph_with_chart(QUES_C_ppt, em_test)
# print(QUES_C_ppt, target=paste0(result_dir, "/QUES_C_ppt_chart1.pptx"))

# Save project file
# proj.file<-paste(working_directory, "QUES_C.lpj", sep="")
# save.image(file = proj.file)
