#
# ISSplott.r
#
#


# install package dependencies if necessary
listOfPackages <- c("ggplot2", "data.table", "staplr")
new.packages <- listOfPackages[!(listOfPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)}

# project folder
mainFolder <- getwd()
# file with plotter functionality
filePlotter <- paste0(mainFolder, "/plotInfra.R")

# folder where all csv input files are located
folderName <- paste0(mainFolder, "/WEBSERVICE")
#folderName where temp pdf output will be located
tempFolder <- paste0(mainFolder, "/WEBSERVICE/TEMP")
# folder where all pdf output files should be located
targetFolder <- paste0(mainFolder, "/WEBSERVICE/PLOTS/")

# files with input data for spurplanknoten and betriebsstellenfahrwege
spkFile <- paste0(mainFolder, "/D2013_M15_46_v02.csv")
btsfwFile <- paste0(mainFolder, "/BTSFW-M15-2013_46_DW_v01.csv")

# import plotter file
source(filePlotter)

# function call
plotAllRoutes(folderName, tempFolder, targetFolder)
