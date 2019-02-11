# library-----
# libraries with no duplicates====
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

## QUES_C ####################################################################################################################################################

# Setting up basic database parameters====
# select the .lpj based on which the project shall be executed
proj.file <- "F:/GGP/Jambi/Result/PreQUES.lpj"
load(proj.file)
idx_QUES_C<-1 # # ADreview: idx shall be retreived from the proj.file and then added by 1 as the process runs

## define initial file
cstock="F:/GGP/jambi/table/c_stock.csv"
em_peat="F:/GGP/jambi/table/faktor_emisi_peat.csv"

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

# standardized column names
# extract peat planning unit IDs
peat_keyword <- "eat"
peat_puID <- lookup_z[grep(peat_keyword, lookup_z[,2]) , 1]
cstock<-read.table(cstock, header = T, sep = ",")
em_peat<-read.table(em_peat, header = T, sep = ",")

# lut_ref has been available in the proj.file
# setting the combined raster layer
ref <- ref*10^6

# chgmap list
list_of_chgmap <- list.files(result_dir, pattern = ".tif$")
num_of_chgmap <- length(list_of_chgmap)

# LOOPING FRAMEWORK FOR EACH TIMESTEP====
for(ts in 1: (length(num_of_chgmap))){
  
  # eval(parse(text=(paste(list_of_chgmap, overwrite=TRUE, sep=""))))
  ti1<-as.numeric(substr(list_of_files[+5], 3, 6))
  ti2<-as.numeric(substr(list_of_files[+6], 3, 6))
  d_ti <- ti2-ti1 # delta year
  
  # combine with admin data
  chg_map<- paste0(result_dir, "/", list_of_chgmap)
  chg_map<- raster(chg_map)
  ch_map <- ref+chg_map
  
  # retrieve freq
  fr_chMap <- data.frame(freq(ch_map, useNA = "no"), stringsAsFactors= FALSE)
  
  # disaggregate using substr # note that the result will be presented as character. Do convert into numeric
  fr_chMap$IDADM <- floor(fr_chMap$value / 10^6)
  fr_chMap$ID_Z <- fr_chMap$value%%10^2
  fr_chMap$ID_T1 <- floor(fr_chMap$value%%10^4/10^2)
  fr_chMap$ID_T2 <- floor(fr_chMap$value%%10^6/10^4)
  
  # Define new 'hectare' column
  fr_chMap <- fr_chMap %>% mutate(hectares = count* res(ref)[1]^2/10000)
  
  # carbon merge t1-t2
  for(w in 1:2){
    if(w ==1) orNames <- names(cstock)
    names(cstock)[1] <-paste0("ID_T", w)
    names(cstock)[2] <-paste0("LC_", w)
    names(cstock)[3] <- paste0("c_", w)
    fr_chMap <- merge(fr_chMap, cstock, by= paste0("ID_T", w), all.x = TRUE)
    if(w ==2) names(cstock) <- orNames
  }
  
  # calculate emission
  fr_chMap$Em_co2Eq <- (fr_chMap$c_1 - fr_chMap$c_2)*fr_chMap$count*res(ref)[1]^2/10000 * 3.67
  fr_chMap$Seq <- 0
  fr_chMap[fr_chMap$Em_co2Eq < 0, "Seq"] <- -1* fr_chMap[fr_chMap$Em_co2Eq < 0, "Em_co2Eq"]
  fr_chMap[fr_chMap$Seq > 0, "Em_co2Eq"] <- 0 # correcting negative emission
  
  # peat_em merge t1-t2
  rec_table <- fr_chMap[, c("value", "Em_co2Eq", "Seq", "count")]
  rec_table$Em_px <- rec_table$Em_co2Eq/rec_table$count
  rec_table$Seq_px <- rec_table$Seq/rec_table$count
  
  # QUES_C dir
  result_dir<-paste0("F:/GGP/Jambi/Result/QUESC_", ti1, "_",  ti2)
  dir.create(result_dir)
  setwd(result_dir)
  
  # generate emission and sequestration map
  em_map <- reclassify(ch_map, rec_table[, c("value", "Em_px")])
  writeRaster(em_map, paste0("Em_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
  seq_map <- reclassify(ch_map, rec_table[, c("value", "Seq_px")])
  writeRaster(seq_map, paste0("Seq_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
  
  # peat_puID contains numbers which stand for id of pu containing peat
  for(w in 1:2){
    em_peatMod <- em_peat[, c(1, 3)]
    em_peatMod[, 2] <- em_peatMod[, 2]*d_ti/2
    names(em_peatMod)[1] <-paste0("ID_T", w)
    names(em_peatMod)[2] <- paste0("EmPeat_", w)
    fr_chMap <- merge(fr_chMap, em_peatMod, by= paste0("ID_T", w), all.x = TRUE)
  }
  
  # correction for peat_em which falls in non-peat planning unit (*0)
  fr_chMap[which(!fr_chMap$ID_Z %in% peat_puID), c("EmPeat_1", "EmPeat_2")] <- fr_chMap[which(!fr_chMap$ID_Z %in% peat_puID), c("EmPeat_1", "EmPeat_2")]*0
  
  # calculate total peat_em
  fr_chMap[, c("EmPeat_1", "EmPeat_2")] <- fr_chMap[, c("EmPeat_1", "EmPeat_2")]*fr_chMap[, "count"]* res(ref)[1]^2/10000 # conversion factor to hectare
  fr_chMap$EmPeatTot <- fr_chMap$EmPeat_1 + fr_chMap$EmPeat_2
  
  # legend merge using lookup tables: pu, admin
  fr_chMap <- merge(fr_chMap, lut_ref, by = "IDADM", all.x = TRUE)
  colnames(lookup_z) = c("ID_Z", "ZONE")
  fr_chMap <- merge(fr_chMap, lookup_z, by = "ID_Z", all.x =TRUE)
  
  # add period annotation
  fr_chMap$PERIOD <- paste0(ti1, "-", ti2)
  
  # store at master table, in .csv, rbind with previous runs
  if(ts == 1) carb_compile <- fr_chMap else carb_compile <- data.frame(rbind(carb_compile, fr_chMap), stringsAsFactors = FALSE)
  
  # Summarize calculation result following the template: Period; Gross Em; Seq; Nett abg em; peat em; Total em. Store as .csv
  if(ts == 1) {
    summary_table <- data.frame(PERIOD = paste0(ti1, "-", ti2), G_Em = sum(fr_chMap$Em_co2Eq), Seq = sum(fr_chMap$Seq), P_Em = sum(fr_chMap$EmPeatTot), stringsAsFactors = FALSE)
  } else {
    summary_add <- data.frame(PERIOD = paste0(ti1, "-", ti2), G_Em = sum(fr_chMap$Em_co2Eq), Seq = sum(fr_chMap$Seq), P_Em = sum(fr_chMap$EmPeatTot), stringsAsFactors = FALSE)
    summary_table <- data.frame(rbind(summary_table, summary_add), stringsAsFactors = FALSE)
  }
  
  # SumTab \ends----
  if(ts == (length(num_of_chgmap))){
    summary_table$NettAll <- summary_table$G_Em - summary_table$Seq + summary_table$P_Em
    write.csv(summary_table, "summary_emission.csv", row.names = FALSE)
    write.csv(carb_compile, "emission_LCdynamics.csv", row.names = FALSE)
  }
  # QUES_C_ppt <- read_pptx()
  # tes <- ms_barchart(data = fr_chMap, x = "CHG_CODE", y = "COUNT")
  # QUES_C_ppt <- add_slide(QUES_C_ppt, layout = "Title and Content", master = "Office Theme") 
  # ph_with_chart(QUES_C_ppt, tes)
  # print(QUES_C_ppt, target=paste0(result_dir, "/QUES_C_ppt_chart1.pptx"))
  setwd(working_directory)
}

#save project file
proj.file<-paste(working_directory, "QUES_C.lpj", sep="")
save.image(file = proj.file)
