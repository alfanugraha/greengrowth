###SCRIPT#FOR#NPV#MAPS###############################################


###library###############
library(raster)
library(foreign)

###INPUT#DATA########################################################
setwd("D:/GGP/NPV/Results/GG/")
LC_NPV_rec<-read.table("D:/GGP/NPV/npv_fin_rec.csv", header = T, sep = ";", stringsAsFactors = FALSE)
colnames(LC_NPV_rec)[2]="id_rec"

# read all tiffs
nOfTiffs<-nrow(as.data.frame(as.character(list.files(pattern='*.tif$'))))

# read all dbfs
listOfdbfFiles<-list.files(pattern='*.dbf$')
nOfDbfs<-nrow(as.data.frame(as.character(listOfdbfFiles)))

###Algorithms#######
# count EuclDist_t column with selected ID
# n2<-nrow(comb_lc[which(comb_lc$EuclDist_t==2),])

# looping from 1 until number of dbfs
# i = 1 until the end of loop
# i = i+1
for(i in 1:nOfDbfs){
  comb_lc<-read.dbf(listOfdbfFiles[i])
  # create combinations
  
  comb_lc_rec<-comb_lc
  comb_lc_rec$id_rec<-0
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3] %in% c(2:7) & EuclDist_t%in% c(1), 2, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3] %in% c(2:7) & EuclDist_t%in% c(2), 3, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3] %in% c(2:7) & EuclDist_t%in% c(3), 4, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==8 & EuclDist_t ==1, 5, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==8 & EuclDist_t ==2, 6, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==8 & EuclDist_t ==3, 7, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==9 & EuclDist_t ==1, 8, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==9 & EuclDist_t ==2, 9, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==9 & EuclDist_t ==3, 10, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==10 & EuclDist_t ==1, 11, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==10 & EuclDist_t ==2, 12, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==10 & EuclDist_t ==3, 13, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==11 & EuclDist_t ==1, 14, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==11 & EuclDist_t ==2, 15, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==11 & EuclDist_t ==3, 16, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==12 & EuclDist_t ==1, 17, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==12 & EuclDist_t ==2, 18, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==12 & EuclDist_t ==3, 19, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==13 & EuclDist_t ==1, 20, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==13 & EuclDist_t ==2, 21, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==13 & EuclDist_t ==3, 22, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==14 & EuclDist_t ==1, 23, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==14 & EuclDist_t ==2, 24, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==14 & EuclDist_t ==3, 25, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==15 & EuclDist_t ==1, 26, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==15 & EuclDist_t ==2, 27, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==15 & EuclDist_t ==3, 28, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==16 & EuclDist_t ==1, 29, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==16 & EuclDist_t ==2, 30, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==16 & EuclDist_t ==3, 31, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==17 & EuclDist_t ==1, 32, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==17 & EuclDist_t ==2, 33, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==17 & EuclDist_t ==3, 34, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==18 & EuclDist_t ==1, 35, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==18 & EuclDist_t ==2, 36, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==18 & EuclDist_t ==3, 37, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==19 & EuclDist_t ==1, 38, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==19 & EuclDist_t ==2, 39, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==19 & EuclDist_t ==3, 40, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==20 & EuclDist_t ==1, 41, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==20 & EuclDist_t ==2, 42, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==20 & EuclDist_t ==3, 43, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==24 & EuclDist_t ==1, 44, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==24 & EuclDist_t ==2, 45, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==24 & EuclDist_t ==3, 46, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==25 & EuclDist_t ==1, 47, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==25 & EuclDist_t ==2, 48, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3]==25 & EuclDist_t ==3, 49, id_rec)})
  comb_lc_rec<-within(comb_lc_rec, {id_rec<-ifelse(comb_lc_rec[,3] %in% c(0,1,21:23,26:28) & EuclDist_t%in% c(1:3), 50, id_rec)})
  
  # combine with NPV lookup table
  comb_lc_rec<- merge(comb_lc_rec,LC_NPV_rec, by="id_rec")
  
  # NPVxHectar
  comb_lc_rec$npv<-scan(text=comb_lc_rec$npv, dec=",", sep=".")
  comb_lc_rec$npvxha<-comb_lc_rec$Count*comb_lc_rec$npv
  
  # Export DBF
  write.dbf(comb_lc_rec,listOfdbfFiles[i])
  
  val <- 21 + (i-1) * 3
  eval(parse(text=(paste("lc_gg_", val, " <- comb_lc_rec", sep=""))))
}

###ANALYSIS RESULT########################################################
colnames(lc_gg_21)[8]="p2021"
colnames(lc_gg_24)[8]="p2024"
colnames(lc_gg_27)[8]="p2027"
colnames(lc_gg_30)[8]="p2030"
colnames(lc_gg_33)[8]="p2033"
colnames(lc_gg_36)[8]="p2036"
colnames(lc_gg_39)[8]="p2039"
colnames(lc_gg_42)[8]="p2042"
colnames(lc_gg_45)[8]="p2045"

colnames(lc_gg_21)[4]="LC"
colnames(lc_gg_24)[4]="LC"
colnames(lc_gg_27)[4]="LC"
colnames(lc_gg_30)[4]="LC"
colnames(lc_gg_33)[4]="LC"
colnames(lc_gg_36)[4]="LC"
colnames(lc_gg_39)[4]="LC"
colnames(lc_gg_42)[4]="LC"
colnames(lc_gg_45)[4]="LC"

lc_gg_21_<-subset(lc_gg_21, select=c(FinRec, EuclDist_t, LC, p2021))
lc_gg_24_<-subset(lc_gg_24, select=c(FinRec, EuclDist_t, LC, p2024))
lc_gg_27_<-subset(lc_gg_27, select=c(FinRec, EuclDist_t, LC, p2027))
lc_gg_30_<-subset(lc_gg_30, select=c(FinRec, EuclDist_t, LC, p2030))
lc_gg_33_<-subset(lc_gg_33, select=c(FinRec, EuclDist_t, LC, p2033))
lc_gg_36_<-subset(lc_gg_36, select=c(FinRec, EuclDist_t, LC, p2036))
lc_gg_39_<-subset(lc_gg_39, select=c(FinRec, EuclDist_t, LC, p2039))
lc_gg_42_<-subset(lc_gg_42, select=c(FinRec, EuclDist_t, LC, p2042))
lc_gg_45_<-subset(lc_gg_45, select=c(FinRec, EuclDist_t, LC, p2045))

all_data<-merge(lc_gg_21_, lc_gg_24_, by=c("FinRec", "EuclDist_t", "LC"))
all_data<-merge(all_data, lc_gg_27_, by=c("FinRec", "EuclDist_t", "LC"))
all_data<-merge(all_data, lc_gg_30_, by=c("FinRec", "EuclDist_t", "LC"))
all_data<-merge(all_data, lc_gg_33_, by=c("FinRec", "EuclDist_t", "LC"))
all_data<-merge(all_data, lc_gg_36_, by=c("FinRec", "EuclDist_t", "LC"))
all_data<-merge(all_data, lc_gg_39_, by=c("FinRec", "EuclDist_t", "LC"))
all_data<-merge(all_data, lc_gg_42_, by=c("FinRec", "EuclDist_t", "LC"))
all_data<-merge(all_data, lc_gg_45_, by=c("FinRec", "EuclDist_t", "LC"))

all_data<-edit(all_data)
write.table(all_data, "../gg_table_analysis.csv", row.names=F, col.names=T, sep=';', dec = ",")




#BAUH
lc_bauh_21_<-read.dbf("../../Dbf_Results/Final/lc21.dbf")
lc_bauh_24_<-read.dbf("../../Dbf_Results/Final/lc24.dbf")
lc_bauh_27_<-read.dbf("../../Dbf_Results/Final/lc27.dbf")
lc_bauh_30_<-read.dbf("../../Dbf_Results/Final/lc30.dbf")
lc_bauh_33_<-read.dbf("../../Dbf_Results/Final/lc33.dbf")
lc_bauh_36_<-read.dbf("../../Dbf_Results/Final/lc36.dbf")
lc_bauh_39_<-read.dbf("../../Dbf_Results/Final/lc39.dbf")
lc_bauh_42_<-read.dbf("../../Dbf_Results/Final/lc42.dbf")
lc_bauh_45_<-read.dbf("../../Dbf_Results/Final/lc45.dbf")

colnames(lc_bauh_21_)[8]="p2021"
colnames(lc_bauh_24_)[8]="p2024"
colnames(lc_bauh_27_)[8]="p2027"
colnames(lc_bauh_30_)[8]="p2030"
colnames(lc_bauh_33_)[8]="p2033"
colnames(lc_bauh_36_)[8]="p2036"
colnames(lc_bauh_39_)[8]="p2039"
colnames(lc_bauh_42_)[8]="p2042"
colnames(lc_bauh_45_)[8]="p2045"

colnames(lc_bauh_21_)[5]="LC"
colnames(lc_bauh_24_)[5]="LC"
colnames(lc_bauh_27_)[5]="LC"
colnames(lc_bauh_30_)[5]="LC"
colnames(lc_bauh_33_)[5]="LC"
colnames(lc_bauh_36_)[5]="LC"
colnames(lc_bauh_39_)[5]="LC"
colnames(lc_bauh_42_)[5]="LC"
colnames(lc_bauh_45_)[5]="LC"

lc_bauh_21<-subset(lc_bauh_21_, select=c(FinRec, EUCLDIST_T, LC, p2021))
lc_bauh_24<-subset(lc_bauh_24_, select=c(FinRec, EUCLDIST_T, LC, p2024))
lc_bauh_27<-subset(lc_bauh_27_, select=c(FinRec, EUCLDIST_T, LC, p2027))
lc_bauh_30<-subset(lc_bauh_30_, select=c(FinRec, EUCLDIST_T, LC, p2030))
lc_bauh_33<-subset(lc_bauh_33_, select=c(FinRec, EUCLDIST_T, LC, p2033))
lc_bauh_36<-subset(lc_bauh_36_, select=c(FinRec, EUCLDIST_T, LC, p2036))
lc_bauh_39<-subset(lc_bauh_39_, select=c(FinRec, EUCLDIST_T, LC, p2039))
lc_bauh_42<-subset(lc_bauh_42_, select=c(FinRec, EUCLDIST_T, LC, p2042))
lc_bauh_45<-subset(lc_bauh_45_, select=c(FinRec, EUCLDIST_T, LC, p2045))

all_data<-merge(lc_bauh_21, lc_bauh_24, by=c("FinRec", "EUCLDIST_T", "LC"))
all_data<-merge(all_data, lc_bauh_27, by=c("FinRec", "EUCLDIST_T", "LC"))
all_data<-merge(all_data, lc_bauh_30, by=c("FinRec", "EUCLDIST_T", "LC"))
all_data<-merge(all_data, lc_bauh_33, by=c("FinRec", "EUCLDIST_T", "LC"))
all_data<-merge(all_data, lc_bauh_36, by=c("FinRec", "EUCLDIST_T", "LC"))
all_data<-merge(all_data, lc_bauh_39, by=c("FinRec", "EUCLDIST_T", "LC"))
all_data<-merge(all_data, lc_bauh_42, by=c("FinRec", "EUCLDIST_T", "LC"))
all_data<-merge(all_data, lc_bauh_45, by=c("FinRec", "EUCLDIST_T", "LC"))

all_data<-edit(all_data)
write.table(all_data, "../bauh_table_analysis.csv", row.names=F, col.names=T, sep=';', dec = ",")
