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

# Load Landsat data from the Maldives ----------------------------------------------------

male_hulhumale <- brick("./downloaded-files/land_use_maldives_2019_08_08/rasterfiles/landsat8_2013may_male_hulhumale.tif")

# Show information about raster file
male_hulhumale

# set the names of the bands
names(male_hulhumale) <- c('blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')
names(male_hulhumale)

# access the number of bands of the raster
nlayers(male_hulhumale)

# Get the spatial resolution of the raster 
res(male_hulhumale) # resolution: 30m

# Get the number of cells of the raster 
ncell(male_hulhumale)

# Visualize the bands of the satellite image  -----------------------------------------
plot(male_hulhumale)

# adjusting the layout of the plot to show figures side by side
nf <- layout(matrix(c(1,0,2), 1, 3, byrow = TRUE), width = c(1,0.2,1), respect = TRUE)
plotRGB(male_hulhumale, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", main = "Landsat True Color - Malé and Hulhumale")
plotRGB(male_hulhumale, r = 2, g = 5, b = 4, axes = TRUE, stretch = "lin", main = "Landsat False Colour - Malé and Hulhumale")


# Select the Malé island only (crop) ---------------------------------------------------

# extent(xmin, xmax, ymin, ymax) -> defining a square with coordinates of its four corners that fits the Malé island only 
e_male <- extent(333358, 335815, 460500, 462449)

# Cropping the bigger area(male_hulhumale object) with the square stored in the extent object (e_male)
male_crop <- crop(male_hulhumale, e_male)

# rename the layers of the new raster object male_crop
names(male_crop) <- c("blue","green","red","NIR","SWIR1","SWIR2")

# plot the new cropped spatial object
plot(male_crop)

# save the new file named male_crop in a raster format
writeRaster(male_crop, file="./saved-files/rasterfiles/male_crop.tif",overwrite=TRUE)


######    2ND LESSON     ########

# extract the values of each raster layer (i.e. each band) of male_crop which fall inside samp polygons and store them in the variable df:
df <- extract(male_crop, samp)

# explore the new variable df
str(df)

# use the function 'rbind' to combine all dataframes (of all polygons)'
df <- as.data.frame(Reduce(rbind, df))

# now explore the structure again
str(df)
head(df)

###########      3RD LESSON    ##########

# Unsupervised classification -------------------------------------------------------

# we use the original Male-Hulhumale raster data and not the cropped area
# Load Landsat data from the Maldives ----------------------------------------------------

male_hulhumale <- brick("./downloaded-files/land_use_maldives_2019_08_08/rasterfiles/landsat8_2013may_male_hulhumale.tif")

names(male_hulhumale) <- c('blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')

# extract the NIR band
rr <- male_hulhumale[['NIR']] 
nr.df <- as.data.frame(rr, xy=T)

# eliminate empty data
nr.df <- nr.df[complete.cases(nr.df$NIR),]  

# after the preparations above we can now run the k means algorithm:
set.seed(1)
nr.km <- kmeans(nr.df$NIR, centers = 9, #here we define that we want 9 centers / categories
                iter.max = 500, nstart = 3, algorithm="Lloyd")

# add the clusters identified by the kmeans algorithm to the new column 'cluster' of your dataframe
nr.df$cluster <- nr.km$cluster

# create a new raster identifying each pixel by the assigned clusters
nr.raster <- rasterFromXYZ(cbind(nr.df[, c('x', 'y', 'cluster')]), res=res(rr), crs=crs(rr))

# visualise the new raster with the clusters identified by 9 different colors
plot(nr.raster, col= c('red', 'pink', 'yellow', 'lightblue', 'darkblue', 'grey', 'green','orange','gold'), main = 'Unsupervised Classification - NIR')


