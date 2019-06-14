
### Adrina, Dani ###
### Step 1: search and replace all the species names (for example "Hyperolius_adspersus"). You might replace with any of the other species in the excel list I sent you. Always use the Genus and species name seperated by "_". 
### There are two places in this script were you need to manually change the species name because it is NOT seperated by an underscore. These are at line 17 and line 32. 
### You will need to make folders for each species you make a model for. In this folder you need to add the file "cropAlt.aci". You will also need to download the distribition shape (.shp) file from IUCN redlist. 


library(raster); #For raster-based loading, calculations, and mapping
library(rgdal); #For reading the M polygon
library(caret)
library(rgbif);
library(spocc); #For getting georefernces
library(raster); #For loading and manipulating rasters
library(scrubr); #Package introduced for data cleaning
library(spatstat); #Spatial statistics package with method for calculating nearest neighbor distance
library(maptools)
library(rgeos)
library(sp)


setwd("/Users/gjongsma/Documents/ENM/Hyperolius_adspersus")
getwd()

#SECTION 1
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#MINE THE OCCURRENCE DATA. 

#RGBIF
#This package works specifically for querying the GBIF database.
rGBIFHyperolius_adspersus <- occ_search(scientificName = "Hyperolius adspersus", hasCoordinate = T);
#Searches all GBIF records for squirrels, only returns occurrences with coordinates
head(rGBIFHyperolius_adspersus$data);
#Shows you all 101 columns in the GBIF record table. That is probably more than you want, so we'll simplify this to the bare essentials.
tablerGBIFHyperolius_adspersus <- cbind(rGBIFHyperolius_adspersus$data$name, rGBIFHyperolius_adspersus$data$decimalLongitude, rGBIFHyperolius_adspersus$data$decimalLatitude, rGBIFHyperolius_adspersus$data$institutionCode); 
dim(tablerGBIFHyperolius_adspersus)
#This puts species name, longitude, latitude, and source institution into a table. There are TONS (101) of options for data fields, including dates, if you want to try making time structured models for your project.
tablerGBIFHyperolius_adspersusClean <- tablerGBIFHyperolius_adspersus[complete.cases(tablerGBIFHyperolius_adspersus),];
dim(tablerGBIFHyperolius_adspersusClean)
#Removes incomplete records
colnames(tablerGBIFHyperolius_adspersusClean) <- c("Species", "Latitude", "Longitude", "Institution");
#Names columns in table
write.csv(tablerGBIFHyperolius_adspersusClean, "Hyperolius_adspersusRGBIF.csv", row.names = F);
#Writes the resulting table to your working directory.

#SPOCC
#This package queries ten different biodiversity databases.
spoccHyperolius_adspersus <- occ(query = "Hyperolius adspersus", from = c('gbif', 'ecoengine', 'vertnet', 'idigbio'), has_coords = T);
#spoccHyperolius_adspersus <- occ(query = "Astylosternus batesi", from = c('gbif'), has_coords = T);
#spoccHyperolius_adspersus <- occ(query = "Scotobleps gabonicus", from = c('idigbio', 'gbif', 'ecoengine', 'vertnet',), has_coords = T);
#Searches all appropriate databases for records of frogs, only returns occurrences with coordinates. Note: if you have marine species, ants, birds, etc, there are other databases available through spocc. Check the tutorial for a complete list.
head(spoccHyperolius_adspersus);
#you will note the structure of the object for spocc is much different than that of rgbif, with separate slots for each of the databases that were queried, and not all the databases may return results.
spoccHyperolius_adspersusDF <- occ2df(spoccHyperolius_adspersus);
dim(spoccHyperolius_adspersusDF)
#This function merges results from all of the databases you queries in a simplified format.
head(spoccHyperolius_adspersusDF)
#Species name, longitude, latitude, and source database are now in the same data table. Note that the institution is not included. That is, for example, GBIF is an aggregator of other data sources.
spoccHyperolius_adspersusDFClean <- spoccHyperolius_adspersusDF[complete.cases(spoccHyperolius_adspersusDF),];
#Removes incomplete records
colnames(spoccHyperolius_adspersusDFClean) <- c("Species", "Longitude", "Latitude", "Database");
#Names columns in table
write.csv(spoccHyperolius_adspersusDFClean, "Hyperolius_adspersusSPOCC.csv", row.names = F);
#Writes the resulting table to your working directory.

#Merging data into a single file.
allHyperolius_adspersus <- rbind(as.matrix(tablerGBIFHyperolius_adspersusClean[,1:3]), as.matrix(spoccHyperolius_adspersusDFClean[,1:3]))
#Puts results of rGBIF and spocc into a single table
colnames(allHyperolius_adspersus) <- c("Species", "Longitude", "Latitude");
allHyperolius_adspersus
write.csv(allHyperolius_adspersus, "All_Hyperolius_adspersus.csv", row.names = F);
#Writes the resulting table to your working directory. Note that there will likely be duplicates between rGBIF and spocc. We will work on cleaning out duplicate occurrences in a future lab.
dim(allHyperolius_adspersus)

#SECTION 2
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#CLEAN THE OCCURRENCE DATA. 

#Get your data from a .csv
rawHyperolius_adspersus <- read.csv("Hyperolius_adspersusRGBIF.csv");
#rawHyperolius_adspersus <- read.csv("All_Hyperolius_adspersus.csv");
dfHyperolius_adspersus <- dframe(rawHyperolius_adspersus);

#Step 1: Using scrubr
#scrubbedHyperolius_adspersus <- dfHyperolius_adspersus[,-6]; #Removes unique, database-specific keys
scrubbedHyperolius_adspersus <- coord_incomplete(dfHyperolius_adspersus); #Removes data with incomplete coordinates
scrubbedHyperolius_adspersus <- coord_unlikely(scrubbedHyperolius_adspersus); #Removes data with unlikely coordinates (i.e. 0,0)
paste("Step 1: Post scrubr: ", nrow(scrubbedHyperolius_adspersus), " points left.", sep = "");
write.csv(scrubbedHyperolius_adspersus, "Scrubbed_Hyperolius_adspersus.csv", row.names = F);

#Step 2: Removing duplicate dates and localities
uniqueHyperolius_adspersus <- unique(scrubbedHyperolius_adspersus); #Removes duplicate points (not considering state)
paste("Step 2: Removing duplicate dates and localities: ", nrow(uniqueHyperolius_adspersus), " points left.", sep = "");
colnames(uniqueHyperolius_adspersus) <- c("Species", "Longitude", "Latitude");
write.csv(uniqueHyperolius_adspersus, "Unique_Hyperolius_adspersus.csv", row.names = F);
UniquePoints <- "Unique_Hyperolius_adspersus.csv"
dim(uniqueHyperolius_adspersus)


##***********************CHECK THAT YOU HAVE ADDED ANY NON-DATABASED DATA NOW (EXAMPLE GJ FIELD 2018)***********************##


#SECTION 3A
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#CREATE M (WITH BUFFER). THAT IS THE LAYER WE WILL EXPLORE POTENTIAL DISTRIBUTION ACROSS

#Load the libraries you'll need.


#RETRIEVE CROPPED RASTER MAP, ALREADY IN YOUR WORKING DIRECTORY. 
CropAfrica <- raster(x = "cropAlt.asc")
crs(CropAfrica) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(CropAfrica)

#Read points
Hyperolius_adspersus <- read.csv(file = "Unique_Hyperolius_adspersus.csv", header = T);
head(Hyperolius_adspersus); #Check out what the datapoints look like
points(Hyperolius_adspersus[,2:3], pch = 16, cex = 1.5)

#Reading shapefiles
LcalShape1 <- readOGR("data_0.shp"); #Automatically reads .prj file
LcalShape1; #Check out what the metadata for the file look like
plot(LcalShape1)

#Reprojecting shapefile
training2 <- spTransform(LcalShape1, CRSobj = crs(CropAfrica));

#Plotting shapefiles
plot(CropAfrica)
plot(training2, add = TRUE); #Layers shapefile on top of raster.
points(Hyperolius_adspersus[,2:3], pch = 16, cex = 1, add = TRUE); #Layers points on top.

#ADD BUFFER TO SHAPEFILE (> GREATER THAN 1 M ASL). 
############### If no buffer jump to line 156 ###################
#IF NO BUFFER SKIP TO NEXT SECTION. 
trainingBuff <- readOGR("data_0.shp");
crs(trainingBuff) <-CRS("+proj=utm +zone=10 +datum=WGS84")
b.r <- buffer(trainingBuff, width = 1, dissolve = T)
crs(b.r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#gb.r <- gBuffer(trainingBuff, width = 0.3, byid = T)
plot(b.r)
b.r
#TRYING TO EXPORT AS SHAPE! 

###############  NO BUFFER OPTIONS  ##############
#b.r <- (LcalShape1)
#plot(b.r)


#CROP IUCN RANGE TO STUDY AREA
CropFrog <- crop(b.r, CropAfrica)
plot(CropFrog)


#BRING IT ALL TOGETHER NOW
plot(CropAfrica)
plot(LcalShape1, add = TRUE)
plot(CropFrog, add = TRUE)
points(Hyperolius_adspersus[,2:3], pch = 16, cex = 1.5)
dim(Hyperolius_adspersus)

#############  REACCESS ANY POINTS OUTSIDE OF NEW BUFFERED AREA!  ##############

#CROP POINTS OUTSIDE OF BUFFERED AREA
LGFpoints <- extract(CropFrog, Hyperolius_adspersus[2:3], pch = 16, cex = 1.5); #Extract raster values to locality points.
points(Hyperolius_adspersus[,2:3], pch = 16, cex = 1.2)
IUCNunique <- cbind(Hyperolius_adspersus, LGFpoints); #Merges extracts with locality points.
head(IUCNunique); #See? Now there's a new column with the altitude at each locality point.
LGFonly <- IUCNunique[complete.cases(IUCNunique),]; #Removes points with no altitude data (i.e. outside raster).
dim(LGFonly)
plot(CropAfrica)
points(LGFonly[,2:3], pch = 16, cex = 1.5)
write.csv(LGFonly, file = "zLGF.csv", row.names = F);

#Making a map
plot(CropAfrica); #Plot raster
plot(CropFrog, add = TRUE); #Layer shapefile on top of raster.
points(LGFonly[,2:3], pch = 16, cex = 1); #Layer points onto map.

#check scale. 
#GABON <- readOGR("GAB_adm0.shp"); #Automatically reads .prj file
#plot(GABON, add = TRUE)
#getwd()

#SECTION 4
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#TRAINNING OUR BIOCLIM LAYERS

setwd("/Users/gjongsma/Documents/ENM/uncorrelated/reduced")
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
#Navigate to folder containing environmental data

#Loading a raster stack
#envtListGJ <- list.files(pattern = ".asc"); #Gets a list of .asc files
#envtStack <- stack(envtListGJ); #Reads in .asc files as a raster stack
#crs(envtStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
#plot(envtStack); #Plots all the layers of the raster stack object

#Loading a raster stack
envtListGJ <- list.files(pattern = ".asc"); #Gets a list of .asc files
envtStack <- stack(envtListGJ); #Reads in .asc files as a raster stack
crs(envtStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
plot(envtStack); #Plots all the layers of the raster stack object

###Get M shapefile ##GJ Note: I am not using the buffered IUCN layer created above (obj = b.r). 
setwd("/Users/gjongsma/Documents/ENM/Hyperolius_adspersus") #Changing working directory to the folder where the shapefile of your M from created in Part One of the lab.
getwd()
dir.create("zMAXENT")
setwd("/Users/gjongsma/Documents/ENM/Hyperolius_adspersus/zMAXENT")
getwd()
#LcalShape1 <- readOGR("species_56272.shp"); #Reads in your shapefile ************************************DISABLE IF USING BUFFER
#crs(LcalShape1) <- crs(envtStack); #Defines Copper Pheasant M projection as identical to envtStack ******DISABLE IF USING BUFFER

###Masking your environmental variables to M 
envtStack <- crop(envtStack, CropFrog) #Crops raster stack to the extent of the shapefile you're using to mask your data
LcalTraining <- mask(envtStack, CropFrog) #Masking the raster stack
plot(LcalTraining)
setwd("/Users/gjongsma/Documents/ENM/Hyperolius_adspersus/zMAXENT")
writeRaster(LcalTraining, filename = "Hyperolius_adspersus", format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T); 
#Saves all the layers in the stack, with "SyrmaticusSoemmerringii" as a prefix



#SECTION 5
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#CLIP BIAS LAYER TO M LAYER. 

setwd("/Users/gjongsma/Documents/ENM/Hyperolius_adspersus/zMAXENT")
Test2 <- raster(x = "./Hyperolius_adspersus_reduced_bio1.asc")
finalBias <- raster("/Users/gjongsma/Documents/ENM/biasLayerTRUE.asc")

CropX <- crop(finalBias, CropFrog) #Crops raster stack to the extent of the shapefile you're using to mask your data
MaskX <- mask(CropX, CropFrog) #Masking the raster stack
plot(MaskX)
MaskX
Test2
getwd()
writeRaster(MaskX, "Hyperolius_adspersusBIAS.asc", format = "ascii", NAFlag = "-9999", overwrite = T)

#RESTRICT DIMENSIONS TO 6 DECIMALS. 


#SECTION 5
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#THRESHOLDS

setwd("/Users/gjongsma/Documents/ENM/Hyperolius_adspersus/")
getwd()

#First you will get the necessary data. You will need the occurrence point .csv file you used to train the model and an ascii file produced by Maxent (it will have the same name as the .png of your model results you submitted for lab last week).
LboulRaster <- raster("./Hyperolius_adspersus_avg.asc"); #This is an ascii produced by Maxent with your model results
LboulPoints <- read.csv("/Users/gjongsma/Documents/ENM/Hyperolius_adspersus/zLGF.csv"); #These are the points you used to train the model
points(LboulPoints[,2:3], pch = 16, cex = 2)

#First you will calculate the thresholds you will use. In this case, you'll find thresholds based on score quantiles from your results raster.
threshold <- quantile(LboulRaster);

#Generates plots of thresholds and statistics to illustrate sensitivity and specificity tradeoffs.
proportionPresent <- vector(mode = "list", length(threshold))
truePresences <- vector(mode = "list", length(threshold))
count <- 1;
while (count <= length(threshold)){
  m <- c(0, threshold[count], 0,  threshold[count], 1, 1); #This vector tells R that from 0 to the threshold, reclassify the raster cells as 0s, and from the threshold to 1, reclassify the cells as 1s.
  rclmat <- matrix(m, ncol=3, byrow=TRUE); #This turns the vector into a matrix
  threshed <- reclassify(LboulRaster, rcl = rclmat); #This reclassifies your raster.
  plot(threshed, main = paste("Threshold: ", threshold[count], sep = "")); #Plot the resulting raster, with the occurrence points.
  points(LboulPoints[,2:3], pch = ".") #Plots your occurrence points
  proportionPresent[[count]] <- table(values(threshed))[2] / (table(values(threshed))[1] + table(values(threshed))[2]) #Calculates the proportion of the training region over which presences are predicted
  truePresences[[count]] <- sum(na.omit(extract(threshed, LboulPoints[,2:3]))) #Counts the number of true presences predicted by the model
  count <- count + 1;
}

m <- c(0, threshold[3], 0,  threshold[3], 1, 1); #This vector tells R that from 0 to the threshold, reclassify the raster cells as 0s, and from the threshold to 1, reclassify the cells as 1s.
rclmat <- matrix(m, ncol=3, byrow=TRUE); #This turns the vector into a matrix
threshed <- reclassify(LboulRaster, rcl = rclmat); #This reclassifies your raster.
plot(threshed)
#points(LboulPoints[,2:3], pch = 16, cex = .8)


#Making and saving the threshold table
thresholdTable <- cbind(threshold, truePresences, proportionPresent) #Puts the results of your loop into a table
write.csv(thresholdTable, "ThresholdTable.csv", row.names = F) #Writes the results into a table
thresholdTable


#GJ <- extend(threshed, CropAfrica)
#crs(GJ) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
r <- raster(ncol=312, nrow=528) 
bb <- extent(7, 20, -10, 12) 
extent(r) <- bb 
r <- setExtent(r, bb)
GJ2 <- extend(threshed, r)
plot(GJ2)
GJ2

GJ3 <- crop(GJ2, CropAfrica)
crs(GJ3) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(GJ3)
GJ3

newFrogRaster <- reclassify(GJ3, cbind(NA, 0))



setwd("/Users/gjongsma/Documents/ENM/BIODIVERSE") #for biodiverse program
writeRaster(newFrogRaster, filename="Hyperolius_adspersus_BIAS.ascii", format="ascii", overwrite=TRUE)


