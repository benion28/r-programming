#---- Ring Ouzel: Data Preparation ----------#

# Set Working Directory
setwd("C:\\Benion\\Benion Programmings\\R\\Species Distribution Modelling")

avi_dat <- read.table('C://Benion//Benion Programmings//R//Species Distribution Modelling//data//Data_SwissBreedingBirds.csv', header=T, sep=',')
summary(avi_dat)

library(psych)
describe(avi_dat)

avi_cols <- c('Turdus_torquatus', 'bio_5', 'bio_2', 'bio_14', 'std', 'rad', 'blockCV_tile')
avi_df <- data.frame(avi_dat)[avi_cols]
summary(avi_df)



library(raster)

# Please note that you have to set download=T if you haven't downloaded the data before:
# bio_curr <- getData('worldclim', var='bio', res=0.5, lon=5.5, lat=45.5, path='C://Benion//Benion Programmings//R//Species Distribution Modelling//data', download=T)[[c(2,5,14)]]
bio_curr <- getData('worldclim', var='bio', res=0.5, lon=5.5, lat=45.5, path='C://Benion//Benion Programmings//R//Species Distribution Modelling//data', download=F)[[c(2,5,14)]]

# Please note that you have to set download=T if you haven't downloaded the data before:
# bio_fut <- getData('CMIP5', var='bio', res=0.5, lon=5.5, lat=45.5, rcp=45, model='NO', year=50, path='data', download=F)[[c(2,5,14)]]
bio_fut <- getData('CMIP5', var='bio', res=0.5, lon=5.5, lat=45.5, rcp=45, model='NO', year=50, path='C://Benion//Benion Programmings//R//Species Distribution Modelling//data', download=T)[[c(2,5,14)]]


# A spatial mask of Switzerland in Swiss coordinates
# bg <- raster('/vsicurl/https://damariszurell.github.io/SDM-Intro/CH_mask.tif')
# writeRaster(bg, "C://Benion//Benion Programmings//R//Species Distribution Modelling//data//CH_mask.tif")
bg <- raster("C://Benion//Benion Programmings//R//Species Distribution Modelling//data//CH_mask.tif")

# The spatial extent of Switzerland in Lon/Lat coordinates is roughly:
ch_ext <- c(5, 11, 45, 48)

# Crop the climate layers to the extent of Switzerland
bio_curr <- crop(bio_curr, ch_ext)

# Re-project to Swiss coordinate system and clip to Swiss political bounday
bio_curr <- projectRaster(bio_curr, bg)
bio_curr <- resample(bio_curr, bg)
bio_curr <- mask(bio_curr, bg)
names(bio_curr) <- c('bio_2', 'bio_5', 'bio_14')

# For storage reasons the temperature values in worldclim are multiplied by 10. For easier interpretability, we change it back to �C.
bio_curr[[1]] <- bio_curr[[1]]/10
bio_curr[[2]] <- bio_curr[[2]]/10

# Repeat above steps for future climate layers
bio_fut <- crop(bio_fut, ch_ext)
bio_fut <- projectRaster(bio_fut, bg)
bio_fut <- resample(bio_fut, bg)
bio_fut <- mask(bio_fut, bg)
names(bio_fut) <- c('bio_2', 'bio_5', 'bio_14')
bio_fut[[1]] <- bio_fut[[1]]/10
bio_fut[[2]] <- bio_fut[[2]]/10

plot(bio_curr)


#---- Ring Ouzel: Data Preparation ----------#

# Fit GLM
m_glm <- glm( Turdus_torquatus ~ bio_2 + I(bio_2^2) + bio_5 + I(bio_5^2) + bio_14 + I(bio_14^2), family='binomial', data=avi_df)
summary(m_glm)


# Install the mecofun package
# library(devtools)
# devtools::install_git("https://gitup.uni-potsdam.de/macroecology/mecofun.git")


# Load the mecofun package
library(mecofun)

# Names of our variables:
pred <- c('bio_2', 'bio_5', 'bio_14')

# We want three panels next to each other:
par(mfrow=c(1,3))

# Plot the partial responses
partial_response(m_glm, predictors = avi_df[,pred])


library(RColorBrewer)
library(lattice)

# We prepare the response surface by making a dummy data set where two predictor variables range from their minimum to maximum value, and the remaining predictor is kept constant at its mean:
xyz <- data.frame(
        expand.grid(seq(min(avi_df[,pred[1]]),
        max(avi_df[,pred[1]]),
        length=50), 
        seq(min(avi_df[,pred[2]]),
        max(avi_df[,pred[2]]),
        length=50)), 
        mean(avi_df[,pred[3]])
      )
names(xyz) <- pred

# Make predictions
xyz$z <- predict(m_glm, xyz, type='response')
summary(xyz)


# Make a colour scale
cls <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))(100)

# plot 3D-surface
wireframe(
  z ~ bio_2 + bio_5, data = xyz, zlab = list("Occurrence prob.", rot=90),
  drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), zlim = c(0, 1),
    main='GLM', xlab='bio_2', ylab='bio_5', screen=list(z = 120, x = -70, y = 3)
)


# Plot inflated response curves:
par(mfrow=c(1,3))
inflated_response(m_glm, predictors = avi_df[,pred], method = "stat6", lwd = 3, main='GLM')


#----- Random forest (RF) ----------#

# Fit RF
(m_rf <- randomForest( x=avi_df[,2:4], y=avi_df[,1], ntree=1000, nodesize=10, importance =T))

# Variable importance:
importance(m_rf,type=1)

varImpPlot(m_rf)

# Look at single trees:
head(getTree(m_rf,1,T))

# Now, we plot response curves in the same way as we did for GLMs above:
par(mfrow=c(1,3))
partial_response(m_rf, predictors = avi_df[,pred], main='Random Forest')


# Plot the response surface:
xyz$z <- predict(m_rf, xyz) # Note that we created the xyz data.frame in the GLM example above
wireframe(z ~ bio_2 + bio_5, data = xyz, zlab = list("Occurrence prob.", rot=90),
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), zlim = c(0, 1),
          main='RF', xlab='bio_2', ylab='bio_5', screen=list(z = 120, x = -70, y = 3))


# Plot inflated response curves:
par(mfrow=c(1,3))
inflated_response(m_rf, predictors = avi_df[,pred], method = "stat6", lwd = 3, main='RF')



#----- Ring Ouzel: Model assessment ----------#

# Make cross-validated predictions for GLM:
crosspred_glm <- mecofun::crossvalSDM(m_glm, traindat= avi_df[!is.na(avi_df$blockCV_tile),], colname_pred=pred, colname_species = "Turdus_torquatus", kfold= avi_df[!is.na(avi_df$blockCV_tile),'blockCV_tile'])

# Make cross-validated predictions for RF:
crosspred_rf <- mecofun::crossvalSDM(m_rf, traindat= avi_df[!is.na(avi_df$blockCV_tile),], colname_pred=pred, colname_species = "Turdus_torquatus", kfold= avi_df[!is.na(avi_df$blockCV_tile),'blockCV_tile'])

# Look at correlation between GLM and RF predictions:
plot(crosspred_glm, crosspred_rf, pch=19, col='grey35')

(eval_glm <- mecofun::evalSDM(observation = avi_df[!is.na(avi_df$blockCV_tile),1], predictions = crosspred_glm))

(eval_rf <- mecofun::evalSDM(observation = avi_df[!is.na(avi_df$blockCV_tile),1], predictions = crosspred_rf))

# Derive median predictions:
crosspred_ens <- apply(data.frame(crosspred_glm, crosspred_rf),1,median)

# Evaluate ensemble predictions
(eval_ens <- mecofun::evalSDM(observation = avi_df[!is.na(avi_df$blockCV_tile),1], predictions = crosspred_ens))


#----- Ring Ouzel: Model assessment ----------#

# Make predictions to current climate:
bio_curr_df <- data.frame(rasterToPoints(bio_curr))
bio_curr_df$pred_glm <- mecofun::predictSDM(m_glm, bio_curr_df)
bio_curr_df$pred_rf <- mecofun::predictSDM(m_rf, bio_curr_df)
bio_curr_df$pred_ens <- apply(bio_curr_df[,-c(1:5)],1,median)

# Make binary predictions:
bio_curr_df$bin_glm <- ifelse(bio_curr_df$pred_glm > eval_glm$thresh, 1, 0)
bio_curr_df$bin_rf <- ifelse(bio_curr_df$pred_rf > eval_rf$thresh, 1, 0)
bio_curr_df$bin_ens <- ifelse(bio_curr_df$pred_ens > eval_ens$thresh, 1, 0)

# Make raster stack of predictions:
r_pred_curr <- rasterFromXYZ(bio_curr_df[,-c(3:5)])
plot(r_pred_curr)

# Assess novel environments in future climate layer:
bio_fut_df <- data.frame(rasterToPoints(bio_fut))

# Values of 1 in the eo.mask will indicate novel environmental conditions
bio_fut_df$eo_mask <- mecofun::eo_mask(avi_df[,pred], bio_fut_df[,pred])
plot(rasterFromXYZ(bio_fut_df[,-c(3:5)]), main='Environmental novelty')


# Make predictions to future climate:
bio_fut_df$pred_glm <- mecofun::predictSDM(m_glm, bio_fut_df)
bio_fut_df$pred_rf <- mecofun::predictSDM(m_rf, bio_fut_df)
bio_fut_df$pred_ens <- apply(bio_fut_df[,-c(1:5)],1,median)

# Make binary predictions:
bio_fut_df$bin_glm <- ifelse(bio_fut_df$pred_glm > eval_glm$thresh, 1, 0)
bio_fut_df$bin_rf <- ifelse(bio_fut_df$pred_rf > eval_rf$thresh, 1, 0)
bio_fut_df$bin_ens <- ifelse(bio_fut_df$pred_ens > eval_ens$thresh, 1, 0)

# Make raster stack of predictions:
r_pred_fut <- rasterFromXYZ(bio_fut_df[,-c(3:5)])
plot(r_pred_fut[[-1]])


# Predictions to analogous climates:
bio_analog_df <- bio_fut_df[,c('x','y','pred_glm','pred_rf')]
bio_analog_df[bio_fut_df$eo_mask>0,c('pred_glm','pred_rf')] <- NA
plot(rasterFromXYZ(bio_analog_df))

# Predictions to novel climates:
bio_novel_df <- bio_fut_df[,c('x','y','pred_glm','pred_rf')]
bio_novel_df[bio_fut_df$eo_mask==0,c('pred_glm','pred_rf')] <- NA
plot(rasterFromXYZ(bio_novel_df))



# Unload Packages
p_unload(all)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L