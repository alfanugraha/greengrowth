#######################################################################
## gg_libraries.R                                                     #
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


# GENERAL LIBRARY
library(raster)
library(rgdal)
library(spatial.tools)
library(tcltk)
library(splitstackshape)
library(stringr)
library(plyr)
library(ggplot2)
library(grid)
library(tiff)
library(RColorBrewer)
library(rasterVis)
library(reshape2)
library(maptools)
library(foreign)
library(lattice)
library(latticeExtra)
library(magick)
library(officer)
library(XML)

# FIRE 
# please download and put maxent.jar first to R_LIBS_USER before running 
# http://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download
library(dismo) 
library(rJava)
library(ENMeval) 
library(utils)
# devtools::install_github("juoe/spatialtoolbox")
library(spatialtoolbox)


# BIODIVERSITY
library(rgeos)
library(DBI)
library(vegan)
library(RSQLite)
library(SDMTools)
library(dplyr)
library(gridExtra)
library(pracma)
library(zoo)



