####################################################

## Project: land use changes in the maldives

## Date: 3rd February, 2022

## Author: Bernard Iorver

##################################################


# 1. set working directory ------------------------------------------------------

setwd("C://Benion//Benion Programmings//R//EDX")

# install packages ----------------------------------------------------------------

# for creating, reading and manipulating and writing raster data
install.packages("raster") 

# classes for importing, manipulating and exporting spatial data
install.packages ("sp")  

# for creating, reading and manipulating raster data
install.packages ("rgdal") 


# load libraries ------------------------------------------------------------------

library(raster)
library (sp)
library (rgdal)

# Loading and visualizing the land use in Malé -------------------------------------------

# to open the shapefile use the function readOGR from package rgdal
samp <- readOGR("./downloaded-files/land_use_maldives_2019_08_08/shapefiles/Male/male.shp")

# get the limits for the Malé shapefile
extent(samp)

# Skip NA data (NA stands for not available and means empty data)
samp <- samp[!is.na(samp@data[,'Legend_S2']),]

# The land-use classification is stored in the attribute @data@Legend_S2. You can see the different land uses by calling:
samp@data$Legend_S2


# Define a colour vector (col_vect2) for the visualisation of the shapefile
col_vect2 <- vector()
dat_col2 <- cbind(as.character(na.omit(samp@data[, 'Legend_S2'])))

# now assign colours to the specific land-cover types using the just defined colour vector (e.g. to later display urban areas in grey):
for(j in 1: dim(dat_col2)[1]){
  if(dat_col2[j, 1] == 'Urban'){ col_vect2[j] <- 'grey'}
  if(dat_col2[j, 1] == 'Beach'){ col_vect2[j] <- 'yellow'}
  if(dat_col2[j, 1] == 'Airport'){ col_vect2[j] <- 'orange'}
  if(dat_col2[j, 1] == 'Shrubland'){ col_vect2[j] <- 'green'}
  if(dat_col2[j, 1] == 'Transitional'){ col_vect2[j] <- 'brown'}
  if(dat_col2[j, 1] == 'Deep Water'){ col_vect2[j] <- 'darkblue'}
  if(dat_col2[j, 1] == 'Shallow Water'){ col_vect2[j] <- 'lightblue'}
  if(dat_col2[j, 1] == 'Vegetation'){ col_vect2[j] <- 'lightgreen'}
  if(dat_col2[j, 1] == 'Harbour'){ col_vect2[j] <- 'pink'}
}

# plot the land use data
layout(matrix(1,1))
plot(samp, col=col_vect2,main="Land use classification of Malé-Hulhumale",cex.main=0.9)
legend('bottomleft', legend=unique(dat_col2), fill = unique(col_vect2), cex=0.8, lty=1, lwd=3, bty='n')


#####  2ND LESSON   #######

# visualise the mean spectra of the land cover data -----------------------------------

# create an empty data frame (think a table) called ms to store the mean spectra
ms <- as.data.frame(matrix(0, nrow = length(unique(samp$Legend_S2)), ncol = nlayers(male_crop)))

#the row names are taken from the samp object
rownames(ms) <- as.character(unique(samp$Legend_S2))

#the column names are taken from the df object
colnames(ms) <- colnames(df) 

# store the mean spectra for each classification in the data frame ms
for (i in unique(samp$Legend_S2)){
  x <- df[samp$Legend_S2==i,]
  # here the mean values are calculated
  ms[i,] <- colMeans(x)
}   

# create a colour vector for the different land-use categories
mycolor <- c('lightgrey', 'darkgrey', 'orange', 'pink', 'red', 'gold', 'lightblue', 'darkblue', 'darkgreen')

# visualize the spectral profile
for (i in 1:nrow(ms)){
  if(i == 1){ 
    # plot the first spectra
    plot(factor(colnames(ms), levels=colnames(ms)), as.vector(as.character(ms[i,])), type = "l", lwd = 3, lty = 1, col = mycolor[i], ylim=c(5000, 15000), xlim=c(1, 6), xlab='Bands', ylab='Reflectance', xaxt='n',main='spectral profile from Landsat 8', font.main =2)    
    axis(1, at=1:6, lab=colnames(ms))
  }else{
    # add the other spectra using the function 'lines'
    lines(factor(colnames(ms), levels=colnames(ms)), as.vector(as.character(ms[i,])), type = "o", lwd = 3, lty = 1, col = mycolor[i]) 
    if(i == nrow(ms)){
      legend('bottomleft', rownames(ms), cex=0.8, col=mycolor, lty=1, lwd=3, bty='n')
    }
  }
}


######    3RD LESSON      ##########

# Land use changes over time -------------------------------------------------------

# Read in the raster files for the different years
male_hulhumale_2018 <- brick("./downloaded-files/land_use_maldives_2019_08_08/rasterfiles/landsat8_2018march_male_hulhumale.tif")
male_hulhumale_2013 <- brick("./downloaded-files/land_use_maldives_2019_08_08/rasterfiles/landsat8_2013may_male_hulhumale.tif")

# Name the bands of the raster files
names(male_hulhumale_2018) <- c('blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')
names(male_hulhumale_2013) <- c('blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')

# select one band of interest, in this case select band NIR:
rr2018 <- male_hulhumale_2018[['NIR']]
rr2013 <- male_hulhumale_2013[['NIR']]


# Convert the raster into dataframes. The parameter 'xy=T' means that the coordinates of the pixels will be part of the dataframe
nr2018.df <- as.data.frame(rr2018, xy=T)
nr2013.df <- as.data.frame(rr2013, xy=T)

#Check whether there are zero values in the data frames
pos_zeros_2018 <- which(nr2018.df==0,arr.ind=TRUE)
pos_zeros_2013 <- which(nr2013.df==0,arr.ind=TRUE)
nrow(pos_zeros_2018) # result: 0
nrow(pos_zeros_2013) # result: 0

#check for missing values
nr2018.df <- nr2018.df[complete.cases(nr2018.df$NIR),]  
nr2013.df <- nr2013.df[complete.cases(nr2013.df$NIR),]  

## run k-means algorithm for 2013
set.seed(1)
nr2013.km <- kmeans(nr2013.df$NIR, centers = 9, iter.max = 500, nstart = 3, algorithm="Lloyd")

# add the cluster ids (identification numbers) of each pixel to the data frame
nr2013.df$cluster <- nr2013.km$cluster

# convert the data frame containing the clusters into a raster
nr2013.raster <- rasterFromXYZ(cbind(nr2013.df[, c('x', 'y', 'cluster')]), res=res(rr2013), crs=crs(rr2013))

## run k-means algorithm for 2018
set.seed(1)
nr2018.km <- kmeans(nr2018.df$NIR, centers = 9, iter.max = 500, nstart = 3, algorithm="Lloyd")

# add the cluster ids of each pixel to the dataframe
nr2018.df$cluster <- nr2018.km$cluster

# convert the dataframe containing the clusters into a raster
nr2018.raster <- rasterFromXYZ(cbind(nr2018.df[, c('x', 'y', 'cluster')]), res=res(rr2018), crs=crs(rr2018))

# adjusting the plot layout to show figures of both years side by side
par(mfrow=c(1,2))

# plotting the classification for 2013 and 2018
plot(nr2013.raster, col= c('red', 'gold', 'green', 'darkblue', 'orange', 'lightblue', 'grey','pink','brown'), main = '2013', line=0.5, cex.main=0.8)
plot(nr2018.raster, col= c('red', 'gold', 'green', 'darkblue', 'orange', 'lightblue', 'grey','pink','brown'), main = '2018', line=0.5, cex.main=0.8)
mtext("Land use classification of Malé-Hulhumale (NIR)", cex=1.25, line=2, adj=1.25)
