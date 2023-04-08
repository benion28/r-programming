

#install.packages('BiodiversityR')

###species diversity_ student project 2021


rm(list=ls())
setwd("C:/Benion/Benion Programmings/R/analysis/species diversity")
data <- read.csv("ceci.csv", sep=",", head=TRUE)

#View(data)
head(data)
summary(data)
summary(data$SPECIES)
spp<-as.factor(data$SPECIES)
levels(spp)
summary(spp)

spps <- as.data.frame(summary(spp))
spps
View(spps)
#write.csv(spps,"cecisp.csv")

names(data)
FID <- spp<-as.factor(data$FID)
levels(FID)
F1 <- data[ data$FID == "Agan", ]
F2 <- data[ data$FID == "Atson", ]
F3 <- data[ data$FID == "Dei", ]

summary(F1)
summary(F2)
summary(F3)


names(data)
#crops
CROPS <- spp<-as.factor(data$CROP)
levels(CROPS)
summary(CROPS)

F1sp <- as.factor(F1$SPECIES)
F2sp <- as.factor(F2$SPECIES)
F3sp <- as.factor(F3$SPECIES)

summary(F1sp)

F1spp <- as.data.frame(summary(F1sp))
F2spp <- as.data.frame(summary(F2sp))
F3spp <- as.data.frame(summary(F3sp))

colnames(F1spp) <- c('count')
colnames(F2spp) <- c('count')
colnames(F3spp) <- c('count')
colnames(spps) <- c('count')


library(abdiv)
alpha_diversities

Diversity_F1<- rbind(berger_parker = berger_parker_d(F1spp$count),
                         brillouin_d = brillouin_d(F1spp$count),
                         dominance = dominance(F1spp$count),
                         heip_e = heip_e(F1spp$count),
                         invsimpson = invsimpson(F1spp$count),
                         kempton_taylor_q = kempton_taylor_q(F1spp$count),
                         margalef = margalef(F1spp$count),
                         mcintosh_d = mcintosh_d(F1spp$count),
                         mcintosh_e = mcintosh_e(F1spp$count),
                         menhinick = menhinick(F1spp$count),
                         pielou_e = pielou_e(F1spp$count),
                         richness = richness(F1spp$count),
                         shannon = shannon(F1spp$count),
                         simpson = simpson(F1spp$count),
                         simpson_e = simpson_e(F1spp$count),
                         strong = strong(F1spp$count))

Diversity_F2<- rbind(berger_parker = berger_parker_d(F2spp$count),
                     brillouin_d = brillouin_d(F2spp$count),
                     dominance = dominance(F2spp$count),
                     heip_e = heip_e(F2spp$count),
                     invsimpson = invsimpson(F2spp$count),
                     kempton_taylor_q = kempton_taylor_q(F2spp$count),
                     margalef = margalef(F2spp$count),
                     mcintosh_d = mcintosh_d(F2spp$count),
                     mcintosh_e = mcintosh_e(F2spp$count),
                     menhinick = menhinick(F2spp$count),
                     pielou_e = pielou_e(F2spp$count),
                     richness = richness(F2spp$count),
                     shannon = shannon(F2spp$count),
                     simpson = simpson(F2spp$count),
                     simpson_e = simpson_e(F2spp$count),
                     strong = strong(F2spp$count))

Diversity_F3<- rbind(berger_parker = berger_parker_d(F3spp$count),
                     brillouin_d = brillouin_d(F3spp$count),
                     dominance = dominance(F3spp$count),
                     heip_e = heip_e(F3spp$count),
                     invsimpson = invsimpson(F3spp$count),
                     kempton_taylor_q = kempton_taylor_q(F3spp$count),
                     margalef = margalef(F3spp$count),
                     mcintosh_d = mcintosh_d(F3spp$count),
                     mcintosh_e = mcintosh_e(F3spp$count),
                     menhinick = menhinick(F3spp$count),
                     pielou_e = pielou_e(F3spp$count),
                     richness = richness(F3spp$count),
                     shannon = shannon(F3spp$count),
                     simpson = simpson(F3spp$count),
                     simpson_e = simpson_e(F3spp$count),
                     strong = strong(F3spp$count))


Diversity_spps<- rbind(berger_parker = berger_parker_d(spps$count),
                     brillouin_d = brillouin_d(spps$count),
                     dominance = dominance(spps$count),
                     heip_e = heip_e(spps$count),
                     invsimpson = invsimpson(spps$count),
                     kempton_taylor_q = kempton_taylor_q(spps$count),
                     margalef = margalef(spps$count),
                     mcintosh_d = mcintosh_d(spps$count),
                     mcintosh_e = mcintosh_e(spps$count),
                     menhinick = menhinick(spps$count),
                     pielou_e = pielou_e(spps$count),
                     richness = richness(spps$count),
                     shannon = shannon(spps$count),
                     simpson = simpson(spps$count),
                     simpson_e = simpson_e(spps$count),
                     strong = strong(spps$count))


library('BiodiversityR')
data(ifri)
View(ifri)

#important value index
x <- data
head(x)

# importancevalue(x, site="Plot.ID", species="Species",count="Trees.ha", basal="BA.ha",
#                 factor="forest", level="") 

IVI <-importancevalue.comp(x, site="PID", species="SPECIES",count="count", basal="BA",
                     factor="forest") 
IVI  
write.csv(IVI, "C:/Benion/Benion Programmings/R/analysis/species diversity/ceciIVI.csv")




################################################
#mercy's analysis
rm(list=ls())
setwd("C:/Documents/Species Diversity")
data <- read.csv("Mercy.csv", sep=",", head=TRUE)

#View(data)
head(data)
summary(data)
summary(data$SPECIES)
spp<-as.factor(data$SPECIES)
levels(spp)
summary(spp)

spps <- as.data.frame(summary(spp))
spps
View(spps)
#write.csv(spps,"C:/Documents/Species Diversity/mercysp.csv")

names(data)
FID <- spp<-as.factor(data$FID)
levels(FID)
F1 <- data[ data$FID == "Iorkume", ]
F2 <- data[ data$FID == "Tyodugh", ]
F3 <- data[ data$FID == "Pilla", ]

summary(F1)
summary(F2)
summary(F3)


names(data)
#crops
CROPS <- spp<-as.factor(data$CROP)
levels(CROPS)
summary(CROPS)

F1sp <- as.factor(F1$SPECIES)
F2sp <- as.factor(F2$SPECIES)
F3sp <- as.factor(F3$SPECIES)

F1spp <- as.data.frame(summary(F1sp))
F2spp <- as.data.frame(summary(F2sp))
F3spp <- as.data.frame(summary(F3sp))

F1spp
F2spp
F3spp

colnames(F1spp) <- c('count')
colnames(F2spp) <- c('count')
colnames(F3spp) <- c('count')
colnames(spps) <- c('count')


library(abdiv)
alpha_diversities
beta_diversities

Diversity_F1<- rbind(berger_parker = berger_parker_d(F1spp$count),
                     brillouin_d = brillouin_d(F1spp$count),
                     dominance = dominance(F1spp$count),
                     heip_e = heip_e(F1spp$count),
                     invsimpson = invsimpson(F1spp$count),
                     kempton_taylor_q = kempton_taylor_q(F1spp$count),
                     margalef = margalef(F1spp$count),
                     mcintosh_d = mcintosh_d(F1spp$count),
                     mcintosh_e = mcintosh_e(F1spp$count),
                     menhinick = menhinick(F1spp$count),
                     pielou_e = pielou_e(F1spp$count),
                     richness = richness(F1spp$count),
                     shannon = shannon(F1spp$count),
                     simpson = simpson(F1spp$count),
                     simpson_e = simpson_e(F1spp$count),
                     strong = strong(F1spp$count))

Diversity_F2<- rbind(berger_parker = berger_parker_d(F2spp$count),
                     brillouin_d = brillouin_d(F2spp$count),
                     dominance = dominance(F2spp$count),
                     heip_e = heip_e(F2spp$count),
                     invsimpson = invsimpson(F2spp$count),
                     kempton_taylor_q = kempton_taylor_q(F2spp$count),
                     margalef = margalef(F2spp$count),
                     mcintosh_d = mcintosh_d(F2spp$count),
                     mcintosh_e = mcintosh_e(F2spp$count),
                     menhinick = menhinick(F2spp$count),
                     pielou_e = pielou_e(F2spp$count),
                     richness = richness(F2spp$count),
                     shannon = shannon(F2spp$count),
                     simpson = simpson(F2spp$count),
                     simpson_e = simpson_e(F2spp$count),
                     strong = strong(F2spp$count))

Diversity_F3<- rbind(berger_parker = berger_parker_d(F3spp$count),
                     brillouin_d = brillouin_d(F3spp$count),
                     dominance = dominance(F3spp$count),
                     heip_e = heip_e(F3spp$count),
                     invsimpson = invsimpson(F3spp$count),
                     kempton_taylor_q = kempton_taylor_q(F3spp$count),
                     margalef = margalef(F3spp$count),
                     mcintosh_d = mcintosh_d(F3spp$count),
                     mcintosh_e = mcintosh_e(F3spp$count),
                     menhinick = menhinick(F3spp$count),
                     pielou_e = pielou_e(F3spp$count),
                     richness = richness(F3spp$count),
                     shannon = shannon(F3spp$count),
                     simpson = simpson(F3spp$count),
                     simpson_e = simpson_e(F3spp$count),
                     strong = strong(F3spp$count))


Diversity_spps<- rbind(berger_parker = berger_parker_d(spps$count),
                       brillouin_d = brillouin_d(spps$count),
                       dominance = dominance(spps$count),
                       heip_e = heip_e(spps$count),
                       invsimpson = invsimpson(spps$count),
                       kempton_taylor_q = kempton_taylor_q(spps$count),
                       margalef = margalef(spps$count),
                       mcintosh_d = mcintosh_d(spps$count),
                       mcintosh_e = mcintosh_e(spps$count),
                       menhinick = menhinick(spps$count),
                       pielou_e = pielou_e(spps$count),
                       richness = richness(spps$count),
                       shannon = shannon(spps$count),
                       simpson = simpson(spps$count),
                       simpson_e = simpson_e(spps$count),
                       strong = strong(spps$count))

Diversity_F1
Diversity_F2
Diversity_F3
Diversity_spps


library('BiodiversityR')
#data(ifri)
#View(ifri)

#important value index
x <- data
head(x)

IVI <-importancevalue.comp(x, site="PID", species="SPECIES",count="count", basal="BA",
                           factor="forest") 
IVI  
write.csv(IVI, "C:/Documents/Species Diversity/mercyIVI.csv")





############################################
#Pilla moses analysis


rm(list=ls())
setwd("C:/Documents/Species Diversity")
data <- read.csv("Pilla.csv", sep=",", head=TRUE)

#View(data)
head(data)
summary(data)
summary(data$SPECIES)
spp<-as.factor(data$SPECIES)
levels(spp)
summary(spp)

spps <- as.data.frame(summary(spp))
spps
View(spps)

names(data)
FID <- spp<-as.factor(data$FID)
levels(FID)
F1 <- data[ data$FID == "Ugondu", ]
F2 <- data[ data$FID == "Vambe", ]

summary(F1)
summary(F2)

F1sp <- as.factor(F1$SPECIES)
F2sp <- as.factor(F2$SPECIES)

summary(F1sp)

F1spp <- as.data.frame(summary(F1sp))
F2spp <- as.data.frame(summary(F2sp))

colnames(F1spp) <- c('count')
colnames(F2spp) <- c('count')
colnames(spps) <- c('count')


library(abdiv)
alpha_diversities
beta_diversities

Diversity_F1<- rbind(berger_parker = berger_parker_d(F1spp$count),
                     brillouin_d = brillouin_d(F1spp$count),
                     dominance = dominance(F1spp$count),
                     heip_e = heip_e(F1spp$count),
                     invsimpson = invsimpson(F1spp$count),
                     kempton_taylor_q = kempton_taylor_q(F1spp$count),
                     margalef = margalef(F1spp$count),
                     mcintosh_d = mcintosh_d(F1spp$count),
                     mcintosh_e = mcintosh_e(F1spp$count),
                     menhinick = menhinick(F1spp$count),
                     pielou_e = pielou_e(F1spp$count),
                     richness = richness(F1spp$count),
                     shannon = shannon(F1spp$count),
                     simpson = simpson(F1spp$count),
                     simpson_e = simpson_e(F1spp$count),
                     strong = strong(F1spp$count))

Diversity_F2<- rbind(berger_parker = berger_parker_d(F2spp$count),
                     brillouin_d = brillouin_d(F2spp$count),
                     dominance = dominance(F2spp$count),
                     heip_e = heip_e(F2spp$count),
                     invsimpson = invsimpson(F2spp$count),
                     kempton_taylor_q = kempton_taylor_q(F2spp$count),
                     margalef = margalef(F2spp$count),
                     mcintosh_d = mcintosh_d(F2spp$count),
                     mcintosh_e = mcintosh_e(F2spp$count),
                     menhinick = menhinick(F2spp$count),
                     pielou_e = pielou_e(F2spp$count),
                     richness = richness(F2spp$count),
                     shannon = shannon(F2spp$count),
                     simpson = simpson(F2spp$count),
                     simpson_e = simpson_e(F2spp$count),
                     strong = strong(F2spp$count))

Diversity_spps<- rbind(berger_parker = berger_parker_d(spps$count),
                       brillouin_d = brillouin_d(spps$count),
                       dominance = dominance(spps$count),
                       heip_e = heip_e(spps$count),
                       invsimpson = invsimpson(spps$count),
                       kempton_taylor_q = kempton_taylor_q(spps$count),
                       margalef = margalef(spps$count),
                       mcintosh_d = mcintosh_d(spps$count),
                       mcintosh_e = mcintosh_e(spps$count),
                       menhinick = menhinick(spps$count),
                       pielou_e = pielou_e(spps$count),
                       richness = richness(spps$count),
                       shannon = shannon(spps$count),
                       simpson = simpson(spps$count),
                       simpson_e = simpson_e(spps$count),
                       strong = strong(spps$count))

Diversity_F1
Diversity_F2
Diversity_spps

library('BiodiversityR')
data(ifri)
View(ifri)

#important value index
x <- data
head(x)

IVI <-importancevalue.comp(x, site="PID", species="SPECIES",count="count", basal="BA",
                           factor="forest") 
IVI  
write.csv(IVI, "C:/Documents/Species Diversity/1pillaIVI.csv")

IVI2 <-importancevalue.comp(x, site="PID", species="SPECIES",count="count", basal="BA",
                           factor="FID") 
IVI2$values

write.csv(IVI2$Ugondu, "C:/Documents/Species Diversity/2pillaIVI.csv")
write.csv(IVI2$Vambe, "C:/Documents/Species Diversity/3pillaIVI.csv")
