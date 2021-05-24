## Validation of SMA using PLanet imagery
## Validation occurred using NWI and updated NWI (hand drawn by Lainie)
##


## Set working directory
options(scipen = 999)
setwd("D:/Meghan/Projects/Yellowstone/GEE_outputs/Updated_11_17_2020/")
list.files()

## Libraries
library("dplyr")
library("reshape2")
library(broom)
library(ggplot2)

## To do
#1.Set up Github repo
#1a What to do with zeroes and how do I get a res for every wetland.
#2.Figure out how many validation data points we have and if they match the planet imagery
#3.Get stats on how many we are missing due to clouds, no imagery, etc..
#4.Figure out cutoff point
#5.Map residuals - look at hydrographs
#6.Look at field data validation
#7. Look at cliamte data


# # Load data 
Wetland_SMA<- read.csv("SMA_Wetlandsworking.csv")#[-7]
val_Aug<- read.csv("Planet_validation_Aug.csv")
val_June<- read.csv("Planet_validation_June.csv")
veg<- read.csv("surroundingvegetation.csv")
head(veg)
head(val_June)

# Add dates
Wetland_SMA$JulianDate<- substring(Wetland_SMA$system.index, 15, 22)
Wetland_SMA$Date<- as.Date(strptime(Wetland_SMA$JulianDate, "%Y %m %d"))

#Find unique dates in 2017 SMA to match with planet dates within 15 days.
#Wetland_SMA <- Wetland_SMA[Wetland_SMA$JulianDate>20170501,]
#Wetland_SMA <- Wetland_SMA[Wetland_SMA$JulianDate<20170831,]
#unique(Wetland_SMA$JulianDate)

#Wetland_SMA <- Wetland_SMA[Wetland_SMA$JulianDate==20170604|20170620 |20170807,]
#"2017-06-06" "2017-06-04" "2017-08-03" "2017-06-03"
#Select matching dates of Landsat imagery and reduce columns
Wetland_SMA_June <- Wetland_SMA[Wetland_SMA$JulianDate==20170604, c(12,21,7,18, 15,26,28,9)]
Wetland_SMA_Aug <- Wetland_SMA[Wetland_SMA$JulianDate== 20170807,c(12,21,7,18, 15,26,28,9)]
#Wetland_SMA_All <- rbind(Wetland_SMA_June,Wetland_SMA_Aug )
tail(Wetland_SMA_June)
colnames(Wetland_SMA)
#Area<- Wetland_SMA[c(9,12, 28), Wetland_SMA$JulianDate="20170807"]
#Area<- na.omit(Area)
#Area$TotalSqm<- (Area$Hectares *10000)
#Area<
#head(Area)'

#Wetland_SMA <- merge(Wetland_SMA, veg, by="OBJECTID")

unique(Wetland_SMA$PolyTyp)
New_SMA_June<- Wetland_SMA_June[Wetland_SMA_June$PolyTyp=="NWI",]
New_SMA_Aug<- Wetland_SMA_Aug[Wetland_SMA_Aug$PolyTyp=="NWI",]

NWI_SMA_June<- Wetland_SMA_June[Wetland_SMA_June$PolyTyp=="Original",]
NWI_SMA_Aug<- Wetland_SMA_Aug[Wetland_SMA_Aug$PolyTyp=="Original",]


length(unique(New_SMA_June$OBJECTID))
length(unique(New_SMA_Aug$OBJECTID))

val_Aug$PlanetDate2<- substring(val_Aug$PlanetDate, 1, 8)
val_Aug$PlanetDate2<- as.Date(val_Aug$PlanetDate2,"%m/%d/%Y")

val_June$PlanetDate2<- substring(val_June$PlanetDate, 1, 8)
val_June$PlanetDate2<- as.Date(val_June$PlanetDate2,"%m/%d/%Y")

## Merge with validation dataset
val_NWI_June<- merge(val_June, NWI_SMA_June, by="OBJECTID")
val_NWI_Aug<- merge(val_Aug, NWI_SMA_Aug, by="OBJECTID")
val_NWI <- rbind(val_NWI_June, val_NWI_Aug)
val_NWI<- val_NWI[!val_NWI$water==0,] 

val_New_June<- merge(val_June, New_SMA_June, by="OBJECTID")
val_New_Aug<- merge(val_Aug, New_SMA_Aug, by="OBJECTID")
val_New<- rbind(val_New_June, val_New_Aug)
val_New<- val_New[!val_New$water==0,] 

head(val_NWI)


head(NWI_SMA)
val_New<- val_New[, c(1,8,14,5,7,13,15)]
val_NWI<- val_NWI[, c(1,8,14,5,7,13,15)]
colnames(val_NWI)<- c("OBJECTID", "PlanetDate", "LandsatDate", "PlanetQual", "Sqm_val", "SMAwater_est","Hectares")
colnames(val_New)<- c("OBJECTID", "PlanetDate", "LandsatDate", "PlanetQual", "Sqm_val", "SMAwater_est","Hectares")

head(val_New)
head(val_NWI)

#Add area to val_NWI
val_New<- merge(val_New, val_NWI[,c(1,7)], by="OBJECTID")
head(val_New)
val_New<-val_New[,c(1:6,8)]
##Save datasets

#write.csv(val_NWI, "val_NWI.csv")
#write.csv(val_New, "val_New.csv")