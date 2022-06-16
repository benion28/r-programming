####################################################

## Project: Landscape Ecology MOOC, Case Study 2.2 Energy Extraction and Rare Plants

## Script purpose: map land use conflicts between energy extraction and plant conservation

## Date: 23rd January, 2022

## Author: Bernard Iorver

##################################################


# 1. set working directory ------------------------------------------------------

setwd("C://Benion//Benion Programmings//R//EDX")


# 2. Install packages if they are not on your system yet --------------------------------

install.packages("raster") #to process rasters
install.packages ("rgeos") 
install.packages ("rgdal") #to process shapefiles


# 3. load libraries ------------------------------------------------------------------

library(raster)
library (rgdal)
library (rgeos)


# 4. Import and Examine Spatial Data ---------------------------------------------

# import polygon shapefile of Colorado Plateau without attributes
cp.poly <- readOGR(dsn = "./downloaded-files/data_unit_2.2/data", layer = "COP_boundpoly_aea") 

# plot the polygon: how does it look like?
plot(cp.poly, col = "red")


# import shapefile with attributes of American states
states <- readOGR(dsn = "./downloaded-files/data_unit_2.2/data", layer = "na_states_aea")

# extract the polygon for Utah from shapefile of all states
state.UT <- states[states$CODE == "UT" ,] 


# Examine the location of Utah (red) and the Colorado Plateau (green) in the U.S.

plot(states)
plot(state.UT, add=T, col="red")
plot(cp.poly, col=NA, border="green", add=T)


# import point shapefile: cities in the Colorado Plateau
cp.cities <- readOGR("./downloaded-files/data_unit_2.2/data", layer = "COP_Major_Cities_pt")

# extract city names with population>40000 in the year 2000
cp.cities$NAME[cp.cities$POP2000>40000]  

# import lines shapefile: streams and rivers
cp.hohflow <- readOGR(dsn = "./downloaded-files/data_unit_2.2/data", layer = "COP_NHD_Flowlines_line")


# create list of unique names and use to subset rivers

hohflow.nm <- unique(cp.hohflow$GNIS_Name)  # list of values in column GNIS_Name
hohflow.rv <- hohflow.nm[grep("River", hohflow.nm)]  # select GNIS_Name 'River' only
cp.rivers <- cp.hohflow[cp.hohflow$GNIS_Name %in% hohflow.rv ,] # only condition 'River' kept

# add rivers and cities to background colorado plateau plot

plot(cp.poly, col = "grey")  # background polygon: Colorado Plateau
plot(cp.rivers, add = T, col = "blue")  # add rivers
plot(cp.cities, pch = 20, cex = 2, col = "red", add = T)  # add cities


# load raster with elevation at 1 km resolution

elev1km <- raster("./downloaded-files/data_unit_2.2/data/elev_1k_aea.img") 

# plot the raster

plot(elev1km)

# add the borders of the colorado plateau (polygon from before)

plot(cp.poly, col = NA, border="red", add = T) 


# import the slope (slp) and aspect (asp) at 1km resolution 

slp1km <- raster("./downloaded-files/data_unit_2.2/data/slp_1k_aea.img")  
asp1km <- raster("./downloaded-files/data_unit_2.2/data/asp_1k_aea.img")  

# compare attributes for all 3 rasters; are they identical?

elev1km
slp1km
asp1km


# stack rasters with identical attributes

topo.stack <- stack(elev1km, slp1km, asp1km)


# rectangle: crop raster=elev1km with the polygon=cp.poly

cp.elevR <- crop(elev1km, cp.poly)  # crop elev1km by cp.poly extent
plot(cp.elevR)  
plot(cp.poly, add = T)  # add Colorado Plateau boundary

# or by using the borders of the colorado plateau as a "mask" for cropping the elevation data:

# crop & mask raster=elev1km with polygon=cp.poly

cp.elev <- mask(cp.elevR, cp.poly)  # 2nd mask values outside of cp.poly polygon
plot(cp.elev)
plot(cp.poly, add = T)  # add polygon boundary

# plot distribution of elevation values in Colorado Plateau

hist(
  cp.elev, 
  xlab = "Elevation in Meters above sea level", 
  ylab = "Frequency", main ="Elevation Histogram")




# 5. Land Ownership in the Colorado Plateau --------------------------------------

d1 <- get(load("./downloaded-files/data_unit_2.2/data/spp.owner.RData"))

# what are ownership codes?

unique(d1$own.code)  

# what are plant species codes?

unique(d1$spp.code) 

# Extract this species from the data:

d2 <- subset(d1, spp.code=="pegr6")

# create a cross-tabulation of the data

d3 <- xtabs(count ~ own.code, d2)  

# create the owners-vector

owners <- c("Federal Public", "Tribal PR", "State PR", "Special PR", "Private")

# plot the result

barplot(d3, xlab = "Land Ownership", ylab  ="Frequency", names.arg = owners)


sum(d3)  # total number of observations

sum(d3[2:5])  # total observations in private

d3[1]/sum(d3)*100  # percentage in public

EM_data <- subset(d1, spp.code == "asden")
CMF_data <- subset(d1, spp.code == "phth6")

EM_d3 <- xtabs(count ~ own.code, EM_data) 
CMF_d3 <- xtabs(count ~ own.code, CMF_data) 

# Total Sum
sum(EM_d3)
sum(CMF_d3)

# Percentage In Public
EM_d3[1]/sum(EM_d3) * 100
CMF_d3[1]/sum(CMF_d3) * 100


# 6. Oil and Gas Wells in the Colorado Plateau ----------------------------------

g1 <- get(load("./downloaded-files/data_unit_2.2/data/spp.oilgas.RData")) #import the data

# what are the oil-gas codes?

unique(g1$oilgas.pres)  

# what are the species codes?

unique(g1$spp.code)

# select garhams beardtongue

g2 <- subset(g1, spp.code=="pegr6") 

# calculate counts by present:absent

g3 <- xtabs(count ~ oilgas.pres, g2) 

#plot the result

barplot(g3, xlab = "Oil--Gas Well--Head Presence/Absence", ylab = "Frequency", names.arg = c("Absent", "Present"))


