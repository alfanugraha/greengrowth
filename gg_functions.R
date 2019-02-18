#######################################################################
## gg_functions.R                                                     #
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

