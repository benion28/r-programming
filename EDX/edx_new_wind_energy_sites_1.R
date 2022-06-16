####################################################
## Project: Landscape Ecology MOOC, Case Study 3.2 Renewable Energy and Landscape Conflicts
## Script purpose: Find and map suitable sites for wind energy in Switzerland
## Date:
## Author:
##################################################


# 1. set working directory ------------------------------------------------------
setwd("C:/Benion/Benion Programmings/R/EDX")


# 2. Install packages ------------------------------------------------------------------
install.packages("rgeos")
install.packages("raster")
install.packages ("sp")
install.packages ("rgdal")


# 3. load libraries -------------------------------------------------------------------- 
library(rgeos)
library(raster)
library (sp)
library (rgdal)


# 4. Map wind potential, touristic areas and areas of conservation interest --------------

# 4.1 load shapefiles containing data on windpotential, nature conservation areas and touristic areas
windpotential <-shapefile("./downloaded-files/Shapefiles_Case_Study_Wind/windpotential_high.shp")
nature <-shapefile("./downloaded-files/Shapefiles_Case_Study_Wind/BLN.shp")
touristic <-shapefile("./downloaded-files/Shapefiles_Case_Study_Wind/OvernightStays_50000.shp")

# 4.2 load basemap layers
basemap <-shapefile("./downloaded-files/Shapefiles_Case_Study_Wind/Switzerland.shp")
lakes <-shapefile("./downloaded-files/Shapefiles_Case_Study_Wind/Lakes.shp")

# 4.3 Plot a map showing basemap and areas with high wind potential
plot(basemap, col="ivory3", border=FALSE, main='Wind Energy Potential Switzerland', cex.main=0.9)
plot(lakes, col="lightblue", add=TRUE, border=FALSE)
plot(windpotential, col="red", add=TRUE, border=FALSE)
legend("topright", legend=c("high wind potential"), fill=c("red"), cex=0.8, bty="n")

# 4.4 Plot areas important for nature protection and tourism ----------------------------
plot(basemap, col="ivory3", border=FALSE, main='Wind Energy Potential Switzerland', cex.main=0.9)
plot(lakes, col="lightblue", add=TRUE, border=FALSE)
plot(windpotential, col="red", add=TRUE, border=FALSE)
plot(nature, col="darkgreen", add=TRUE, border=FALSE)
plot(touristic, col="purple", add=TRUE, border=FALSE)
legend("topright", legend=c("high wind potential", "important natural areas", "touristic areas"), fill=c("red", "darkgreen", "purple"), cex=0.8, bty="n")


# 5. Map conflict free wind energy potential

# 5.1 Calculate difference between the layers
wind_no_protect <- gDifference(windpotential, nature)
wind_no_tourists <- gDifference(windpotential, touristic) 

# 5.2 Plot areas with low nature conflict potential
plot(basemap, col="ivory3", border=FALSE, main='Wind Map Switzerland')
plot(lakes, col="lightblue", add=TRUE, border=FALSE)
plot(windpotential, col="red", add=TRUE, border=FALSE)
plot(wind_no_protect, col="green3", add=TRUE, border=FALSE)
legend("topright", legend=c("high windpotential & conflicts with nature protection", "high windpotential & no nature protection conflicts"), fill=c("red", "green3"), cex=0.8, bty="n")

# 5.3 Plot areas with low tourism conflict potential
plot(basemap, col="ivory3", border=FALSE, main='Wind Map Switzerland')
plot(lakes, col="lightblue", add=TRUE, border=FALSE)
plot(windpotential, col="red", add=TRUE, border=FALSE)
plot(wind_no_tourists, col="green3", add=TRUE, border=FALSE)
legend("topright", legend=c("high windpotential & conflicts with tourism", "high windpotential & no tourism conflicts"), fill=c("red", "green3"), cex=0.8, bty="n")

# 5.4 Intersect no tourism conflicts layer with no nature conflicts layer
wind_no_tourist_no_nature <- intersect (wind_no_tourists,wind_no_protect)

# 5.5 Subtract Open Space Areas
construction <-shapefile("./downloaded-files/Shapefiles_Case_Study_Wind/AreasLowConstruction.shp")
wind_no_conflict <- gDifference(wind_no_tourist_no_nature,construction)

# 5.6 Plot map showing high wind potential with no tourism, nature and open space conflicts
plot(basemap, col="ivory3", border=FALSE, main='Wind Map Switzerland')
plot(lakes, col="lightblue", add=TRUE, border=FALSE)
plot(windpotential, col="red", add=TRUE, border=FALSE)
plot(wind_no_conflict, col="green3", add=TRUE, border=FALSE)
legend("topright", legend=c("high windpotential & potential conflicts", "high windpotential & no conflicts"), fill=c("red", "green3"), cex=0.8, bty="n")


# 6. Key Numbers

# 6.1 Calculate percentages of conflict areas 
p_tourist<-round(gArea(wind_no_tourists)/gArea(windpotential)*100,1)
p_tourist_nature<-round(gArea(wind_no_tourist_no_nature)/gArea(windpotential)*100,1)
p_all<-round(gArea(wind_no_conflict)/gArea(windpotential)*100,1)
percentage<-c(p_tourist,p_tourist_nature,p_all)

# 6.2 draw chart
par(mar=c(5, 5, 5, 5))
bp<-barplot(percentage, main="Percentage of remaining Wind Potential Area", horiz=FALSE,
            names.arg=c("tour. confl. excl.","tour.&nat. conflicts excl.","tour.&nat.&open space confl. excl."), 
            col=c("yellow", "greenyellow", "green"), border=NA, 
            ylab="Percentage %" ,ylim=c(0,100), width = c(18, 18, 18), xlim=c(0,80))
text(bp,percentage-0.3,labels=percentage)
