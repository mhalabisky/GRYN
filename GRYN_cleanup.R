## GRYN Clean up    
## 5/24/2021
## Meghan Halabisky

##libraries

#### Upload GRYN files and extract data
options(scipen = 999)
setwd("D:/Meghan/Projects/Yellowstone/GEE_outputs/Updated_11_17_2020/")
list.files()
library("dplyr")
library("reshape2")


# Load data for Lakes
#water_em_timeseries<- read.csv("water_em_timeseriesLakes3.csv")
#Wetland_indices<- read.csv("Indices_Wetlandsworking.csv")[-7]

#Wetland_SMA<- read.csv("SMA_Wetlandsworking.csv")#[-7]
#pixelcount <- read.csv("pixelcount.csv")

# # Load data For all wetlands - all size class 
water_em_timeseries<- read.csv("water_em_timeseries.csv")
Wetland_indices<- read.csv("Indices_Wetlandsworking.csv")[-7]
#Wetland_indices<- read.csv("Indices_Wetlandsworking_2021addons.csv")[-7]
#Wetland_SMA<- read.csv("SMA_Wetlandsworking_2021addons_v2.csv")
Wetland_SMA<- read.csv("SMA_Wetlandsworking.csv")#[-7]
#pixelcount <- read.csv("pixelcount_2021addons.csv")  
pixelcount <- read.csv("pixelcount.csv")
#groundtruth <- read.csv("Wetland_WorkingLayer_table.csv")

### Select only the New NWI polygons
Wetland_indices<- Wetland_indices[Wetland_indices$PolyTyp=="NWI",]
Wetland_SMA<- Wetland_SMA[Wetland_SMA$PolyTyp=="NWI",]
pixelcount<- pixelcount[pixelcount$PolyTyp=="NWI", c(1,6,12)]
# Wetland example data

head(water_em_timeseries)
head(Wetland_indices)
head(Wetland_SMA)
tail(pixelcount)

# Add dates
colnames(Wetland_SMA)
#Wetland_SMA$OBJECTID[Wetland_SMA$OBJECTID==146 & Wetland_SMA$Comment =="NWI is good"]<- 147 ## Fix error

water_em_timeseries$JulianDate<- substring(water_em_timeseries$system.index, 15, 22)
Wetland_SMA$JulianDate<- substring(Wetland_SMA$system.index, 15, 22)
Wetland_indices$JulianDate<- substring(Wetland_indices$system.index, 15, 22)
pixelcount$JulianDate<- substring(pixelcount$system.index, 15, 22)

head(water_em_timeseries)
head(Wetland_indices)
head(Wetland_SMA)
tail(pixelcount)
#pixelcount<- pixelcount[,c(2,3,4)]
pixelcount<- pixelcount[,c(7,13,15)]
class(Wetland_indices$JulianDate)
class(Wetland_SMA$JulianDate)
## Remove NAs
Wetland_indices <- Wetland_indices[!is.na(Wetland_indices$NDVI),]

## Cloudy days when extracting water endmember. Remove cloudy days by locating endmembers that are not "pure" - Should be 100% water. 
q100<- max(water_em_timeseries$water)
quantile(water_em_timeseries$water)

q85<-q100-(q100*.15)
#q85<- 352635

water_em_cloudfree<- water_em_timeseries[water_em_timeseries$water > q85,]
plot(water_em_timeseries$JulianDate, water_em_timeseries$water, ylim=c(0,q100))## Noisy water_em
plot(water_em_cloudfree$JulianDate, water_em_cloudfree$water, ylim=c(0,q100)) ## Cleaned up water_em. Identify noisy dates 

water.cloudfree.dates<- water_em_cloudfree$JulianDate
indices.removeNAs<- Wetland_indices$JulianDate
length(indices.removeNAs)

##remove cloudy dates that were missed. Remove dates less than 0

#cloudfree.dates<- merge(grass.cloudfree.dates, water.cloudfree.dates)
Wetland_SMA<- Wetland_SMA[!Wetland_SMA$water<0, ]
#Wetland_SMA<- Wetland_SMA[!Wetland_SMA$water==0, ]
Wetland_SMA<- Wetland_SMA[Wetland_SMA$JulianDate %in% water.cloudfree.dates, ]  ## Removed noisy dates from time series. 
Wetland_SMA<- Wetland_SMA[Wetland_SMA$JulianDate %in% indices.removeNAs, ] # just in case there are NAs

Wetland_SMA$Date<- as.Date(strptime(Wetland_SMA$JulianDate, "%Y %m %d"))
Wetland_indices$Date<- as.Date(strptime(Wetland_indices$JulianDate, "%Y %m %d"))
pixelcount$Date<- as.Date(strptime(pixelcount$JulianDate, "%Y %m %d"))

## Test
tail(Wetland_SMA, 199)
test<- Wetland_SMA[Wetland_SMA$OBJECTID==48,  ]
#test<- Wetland_SMA[Wetland_SMA$Name=="Y955-8_Polygon",  ]
test.yr<- test[test$JulianDate>2007000,]
test.yr<- test.yr[test.yr$JulianDate<2008000,]
plot(test.yr$JulianDate,test.yr$water )
lines(test.yr$JulianDate,test.yr$water )
plot(test$JulianDate,test$water )
lines(test$JulianDate,test$water )

head(Wetland_SMA)
max(test$water)
quantile(test$water)  

Wetland_SMA$Unique_ID<- Wetland_SMA$Name # Convert OBJECTID to unique id 
pixelcount$Unique_ID<- pixelcount$Name # Convert OBJECTID to unique id 
Wetland_indices$Unique_ID<- Wetland_indices$OBJECTID # Convert OBJECTID to unique id 

## Select only the new NWI polygons


#all<- merge(Wetland_indices, Wetland_SMA, by=c("Unique_ID","Date"))
head(Wetland_SMA)
head(pixelcount)
all<- merge(Wetland_SMA, pixelcount, by=c("Unique_ID","Date"))
head(all)
#all2<- as.data.frame(all %>% group_by(label) %>% mutate(prct_water = water/max(water))) 
clean<- as.data.frame(all %>% group_by(Unique_ID) %>% mutate(pixelcount = max(count))) ## find piuxel count of polygon. Needs to match max pixel count
clean1<- as.data.frame(clean %>% group_by(Unique_ID) %>% mutate(maxwater = max(water))) ## Find maximum surface water inundation
clean2<- as.data.frame(clean1 %>% group_by(Unique_ID) %>% mutate(prctwater = water/max(water))) ## Find Percent water
clean3<- as.data.frame(clean2 %>% group_by(Unique_ID) %>% mutate(quantile = quantile(water,.999)))## Find 99.9% to remove outliers
head(clean3)

clean3<- clean3[clean3$pixelcount==clean3$count,]
clean4<- clean3[clean3$water<clean3$quantile,] # Remove 99.9% values to remove high values
head(clean4)
clean5<- as.data.frame(clean4 %>% group_by(Unique_ID) %>% mutate(prctwater2 = water/max(water)))
colnames(clean5)


#Test out 
test<-clean5[clean5$OBJECTID.x==39, ]
plot(test$Date,test$water)
lines(test$Date,test$water)
test.lm<- lm(test$water~test$Date)
max(test$water)
head(test)  
summary(test.lm)

clean5$JulianDate.x<- as.numeric(clean5$JulianDate.x)
colnames(clean5)

# convert from long to wide format
clean6<- clean5[, c(1,2,20,28,30)] # Remove extra columns
#clean6<-as.numeric(clean6$Unique_ID)
head(clean6)
New_wide <- dcast(clean5, Unique_ID~ Date, value.var= "water",fun.aggregate= mean)
New_wide <- dcast(clean5, Unique_ID~ Date, value.var= "prctwater2",fun.aggregate= mean)
head(New_wide)

write.csv(clean6, "Wetlands_2021_addons.csv")
write.csv(New_wide, "Wetlands_2021addons_wideprctwtr.csv")
#write.csv(New_wide, "Wetlands_ALL_wide_v05.csv")
#write.csv(clean6, "Wetlands_ALL_long_v05.csv")
tail(clean6)
clean6<-clean6[c(2,3,6),]
plot

#write.csv(clean6_wide, "Lakes_wide_v01.csv")
#write.csv(clean6, "Lakes_ALL_long_v01.csv")
