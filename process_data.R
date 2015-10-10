#### Load or Save Workspace ####
# save.image("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/process_data.RData")
# load("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/process_data.RData")

#### Libraries ####
library(ggplot2)
library(spatial)
library(rgeos)
library(rgdal)
library(gpclib)
library(sp)
library(maptools)
require(chron)
library(spdep)
library(maps)
library(geosphere)
library(RColorBrewer)
library(reshape2)
library(RCurl)
# library(plotly)

#### Tasks ####
# Look at number of trips during quarantine days compared to non
  # Before and after for Tonkolili (July 24)
  # Three saturdays (April 4, 11, 18) nationally. Result: Strong signal
  # "three days stay at home" March 27-29 nationally. Result: signal is not very strong except on first day
  # before and after schools reopen (April 14). Result: no national signal.

#### Read Data ####
# Create a dataset for the Admin 2 Trips
setwd("/Users/peakcm/Documents/SLE_Mobility/sle_ericson")
data_admin2 <- read.csv(file = "IndivMvtBtwnAdmin2DaySep1Agg.csv")

date.start <- as.numeric(as.Date(c("03/20/2015"), format = "%m/%d/%Y"))
dates <- seq(from = date.start, to = date.start + ncol(data_admin2) - 3)
names(data_admin2) <- c("Dis_From", "Dis_To",  dates)
data_admin2$cum_trips <- apply(data_admin2[,3:ncol(data_admin2)], 1, sum)
data_admin2$cum_trips_16518to16520 <- apply(data_admin2[,7:9], 1, sum)
data_admin2$cum_trips_16521to16523 <- apply(data_admin2[,10:12], 1, sum)
data_admin2$cum_trips_16524to16526 <- apply(data_admin2[,13:15], 1, sum)

# Create a dataset for the Admin 3 Trips
setwd("/Users/peakcm/Documents/SLE_Mobility/sle_ericson")
data_admin3 <- read.csv(file = "IndivMvtBtwnAdmin3DaySep1Agg.csv")

date.start <- as.numeric(as.Date(c("03/20/2015"), format = "%m/%d/%Y"))
dates <- seq(from = date.start, to = date.start + ncol(data_admin3) - 3)
names(data_admin3) <- c("Chief_From", "Chief_To",  dates)
data_admin3$cum_trips <- apply(data_admin3[,3:ncol(data_admin3)], 1, sum)
data_admin3$cum_trips_16518to16520 <- apply(data_admin3[,7:9], 1, sum)
data_admin3$cum_trips_16521to16523 <- apply(data_admin3[,10:12], 1, sum)
data_admin3$cum_trips_16524to16526 <- apply(data_admin3[,13:15], 1, sum)

# Create a dataset for the tower locations provided to us by Ericsson
setwd("/Users/peakcm/Documents/SLE_Mobility/data")
towers <- read.csv(file = "Tower_Locations.csv")
towers$CHCODE <- towers$admin3
towers.fort <- fortify(towers, region = admin3)

# Add Buffer to tower locations
work.dir <- "/Users/peakcm/Documents/SLE_Mobility/Arc GIS"
towers_buffer <- readOGR(work.dir, layer = 'towers_buffer')
# plot(towers_buffer)
towers_buffer.fort <- fortify(towers_buffer)

# Add city CHCODES to the encompassing polygon
data_admin3[data_admin3$Chief_From == 1291, "Chief_From"] <- 1212
data_admin3[data_admin3$Chief_To == 1291, "Chief_To"] <- 1212
towers[towers$CHCODE == 1291,"CHCODE"] <- 1212

data_admin3[data_admin3$Chief_From == 1391, "Chief_From"] <- 1313
data_admin3[data_admin3$Chief_To == 1391, "Chief_To"] <- 1313
towers[towers$CHCODE == 1391,"CHCODE"] <- 1313

data_admin3[data_admin3$Chief_To == 2191, "Chief_To"] <- 2102
data_admin3[data_admin3$Chief_From == 2191, "Chief_From"] <- 2102
towers[towers$CHCODE == 2191,"CHCODE"] <- 2102

data_admin3[data_admin3$Chief_From == 3191, "Chief_From"] <- 3108
data_admin3[data_admin3$Chief_To == 3191, "Chief_To"] <- 3108
towers[towers$CHCODE == 3191,"CHCODE"] <- 3108

data_admin3[data_admin3$Chief_From == 3291, "Chief_From"] <- 3209
data_admin3[data_admin3$Chief_To == 3291, "Chief_To"] <- 3209
towers[towers$CHCODE == 3291,"CHCODE"] <- 3209

# Combine some Freetown towers into the encompassing polygon
data_admin3[data_admin3$Chief_From == 4201, "Chief_From"] <- 4208
data_admin3[data_admin3$Chief_To == 4201, "Chief_To"] <- 4208
towers[towers$CHCODE == 4201,"CHCODE"] <- 4208

data_admin3[data_admin3$Chief_From == 4202, "Chief_From"] <- 4208
data_admin3[data_admin3$Chief_To == 4202, "Chief_To"] <- 4208
towers[towers$CHCODE == 4202,"CHCODE"] <- 4208

data_admin3[data_admin3$Chief_From == 4206, "Chief_From"] <- 4208
data_admin3[data_admin3$Chief_To == 4206, "Chief_To"] <- 4208

data_admin3[data_admin3$Chief_From == 4207, "Chief_From"] <- 4208
data_admin3[data_admin3$Chief_To == 4207, "Chief_To"] <- 4208
towers[towers$CHCODE == 4207,"CHCODE"] <- 4208

# Create a shapefile for Sierra Leone Admin 3
work.dir <- "/Users/peakcm/Documents/SLE_Mobility/Arc GIS/Open Humanitarian Data Repository/Sierra Leone Chiefdoms SLGov Admin_3 2012"
admin3.sp <- readOGR(work.dir, layer = 'Sierra_Leone_Chiefdoms_SLGov_Admin_3_2012')
admin3.sp <- admin3.sp[admin3.sp$CHCODE > 0,]
centroids <- data.frame(getSpPPolygonsLabptSlots(admin3.sp))
names(centroids) <- c("Long", "Lat")
admin3.sp <- spCbind(admin3.sp, centroids$Long)
admin3.sp <- spCbind(admin3.sp, centroids$Lat)
admin3.sp.fort <- fortify(admin3.sp, region = "CHCODE", )
admin3.sp.fort$CHCODE <- as.numeric(admin3.sp.fort$id)

# Create a shapefile for Sierra Leone Admin 2
work.dir <- "/Users/peakcm/Documents/SLE_Mobility/Arc GIS/Open Humanitarian Data Repository/Sierra_Leone_Districts_Admin_2_2012"
admin2.sp <- readOGR(work.dir, layer = 'Sierra_Leone_Districts_Admin_2_2012')
centroids <- data.frame(getSpPPolygonsLabptSlots(admin2.sp))
names(centroids) <- c("Long", "Lat")
admin2.sp <- spCbind(admin2.sp, centroids$Long)
admin2.sp <- spCbind(admin2.sp, centroids$Lat)
admin2.sp.fort <- fortify(admin2.sp, region = "DISCODE", )
admin2.sp.fort$DISCODE <- as.numeric(admin2.sp.fort$id)

# Create a shapefile for Sierra Leone Admin 1
work.dir <- "/Users/peakcm/Documents/SLE_Mobility/Arc GIS/Open Humanitarian Data Repository/Sierra_Leone_Provinces_-_Admin_1_2012"
admin1.sp <- readOGR(work.dir, layer = 'Sierra_Leone_Provinces_-_Admin_1_2012')
centroids <- data.frame(getSpPPolygonsLabptSlots(admin1.sp))
names(centroids) <- c("Long", "Lat")
admin1.sp <- spCbind(admin1.sp, centroids$Long)
admin1.sp <- spCbind(admin1.sp, centroids$Lat)
admin1.sp.fort <- fortify(admin1.sp, region = "DISCODE", )
admin1.sp.fort$RCODE <- as.numeric(admin1.sp.fort$id)

# Create a dataset with estimated travel times between districts from WHO Mr. Yarabah Conteh
names <- c("Bo", "Kaliahun", "Freetown", "Bombali", "Port Loko", "Bonthe", "Kambia", "Kenema", "Koindagu", "Kono", "Moyamba", "Pujehun", "Tonkolili", "Freetown_Rural")
DISCODE <- c(31, 11, 42, 21, 24, 32, 22, 12, 23, 13, 33, 34, 25, 41)
travel.times <- data.frame(rep(DISCODE, each=length(DISCODE)))
names(travel.times) <- c("Dis_From")
travel.times$Dis_To <- rep(DISCODE, times = length(DISCODE))
travel.times$Hours <- NA
travel.times[travel.times$Dis_From==31,"Hours"] <- c(0, 3, 3.5, 3, 2.5, 2, 3, 1, 5, 6, 2, 3, 3.5, 3.5)
travel.times[travel.times$Dis_From==11,"Hours"] <- c(NA, 0, 7, 5.5, 6, 4, 6.5, 2, 7, 4, 5, 5.5, 7, 7)
travel.times[travel.times$Dis_From==42,"Hours"] <- c(NA, NA, 0, 3, 3, 5, 4, 4.5, 5, 6, 4, 5, 4, 0)
travel.times[travel.times$Dis_From==21,"Hours"] <- c(NA, NA, NA, 0, 2, 4, 3, 4, 2, 4, 4, 5, .75, NA)
travel.times[travel.times$Dis_From==24,"Hours"] <- c(NA, NA, NA, NA, 0, 4, .75, 5, 4, 5, 3, 5, 3, NA)
travel.times[travel.times$Dis_From==32,"Hours"] <- c(NA, NA, NA, NA, NA, 0, 4, 2, 6, 7, 3, 3, 5, NA)
travel.times[travel.times$Dis_From==22,"Hours"] <- c(NA, NA, NA, NA, NA, NA, 0, 5, 5, 4, 4, 5, 3, NA)
travel.times[travel.times$Dis_From==12,"Hours"] <- c(NA, NA, NA, NA, NA, NA, NA, 0, 7, 4, 3, 2.5, 5, NA)
travel.times[travel.times$Dis_From==23,"Hours"] <- c(NA, NA, NA, NA, NA, NA, NA, NA, 0, 5, 6, 6, 2.5, NA)
travel.times[travel.times$Dis_From==13,"Hours"] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 7, 8, 3.5, NA)
travel.times[travel.times$Dis_From==33,"Hours"] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 3.5, 5, NA)
travel.times[travel.times$Dis_From==34,"Hours"] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 5, NA)
travel.times[travel.times$Dis_From==25,"Hours"] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, NA)
travel.times[travel.times$Dis_From==41,"Hours"] <- travel.times[travel.times$Dis_From==42,"Hours"]

for (i in 1:nrow(travel.times)){
  if (is.na(travel.times[i, "Hours"]) == 1){
    Dis_From <- travel.times[i, "Dis_From"]
    Dis_To <- travel.times[i, "Dis_To"]
    travel.times[i,"Hours"] <- travel.times[travel.times$Dis_From == Dis_To & travel.times$Dis_To == Dis_From, "Hours"]
  }
}

# Import population data
setwd("/Users/peakcm/Documents/2014 Cholera OCV/Data - Raw/Population")
data_chief_pop <- read.csv(file = "SL_Chiefdom_Codes_Pop.csv")

#### Tonkolili Plots ####
# Focus on Kholifa Rowala Chiefdom (2505) in Tonkolili District
hist(log(data_admin3[data_admin3$Chief_From == 2505, "cum_trips"]), xlab = "log(cumulative trips)", main = "Trips from Kholifa Rowala, Tonkolili")
data_admin3 <- data_admin3[order(-data_admin3$cum_trips),]
plot(data_admin3[data_admin3$Chief_From == 2505, "cum_trips"],
     main = "Trips from Kholifa Rowala, Tonkolili", xlab = "Chiefdom Rank", ylab = "Cumulative trips")

# Pie chart (Chiefdoms)
lbls <- paste(data_admin3[data_admin3$Chief_From == 2505,"Chief_To"], "\n", round(data_admin3[data_admin3$Chief_From == 2505,"cum_trips"]/sum(data_admin3[data_admin3$Chief_From == 2505,"cum_trips"])*100, 0),"%", sep="")
pie(x = data_admin3[data_admin3$Chief_From == 2505,"cum_trips"], labels = lbls, main="Chiefdom Destinations from\nKholifa Rowala, Tonkolili (2505)", )

lbls <- paste(data_admin3[data_admin3$Chief_To == 2505,"Chief_From"], "\n", round(data_admin3[data_admin3$Chief_To == 2505,"cum_trips"]/sum(data_admin3[data_admin3$Chief_To == 2505,"cum_trips"])*100, 0),"%", sep="")
pie(x = data_admin3[data_admin3$Chief_To == 2505,"cum_trips"], labels = lbls, main="Chiefdom Departures to\nKholifa Rowala, Tonkolili (2505)", )

# List of chiefdom destinations
data_admin3.From2505 <- data_admin3[data_admin3$Chief_From == 2505,c("Chief_From","Chief_To", "cum_trips")]
# View(data_admin3.From2505[order(-data_admin3.From2505$cum_trips),])

sum(data_admin3.From2505[data_admin3.From2505$Chief_To >= 2000 & data_admin3.From2505$Chief_To < 3000,"cum_trips" ]) / sum(data_admin3.From2505[, "cum_trips"]) # 83.6% of all travel is to the North region

sum(data_admin3.From2505[data_admin3.From2505$Chief_To > 4000 ,"cum_trips" ]) / sum(data_admin3.From2505[data_admin3.From2505$Chief_To < 2000 | data_admin3.From2505$Chief_To >= 3000, "cum_trips"]) # 73.8% of all travel outside of the North region is to Western Area

sum(data_admin3.From2505[data_admin3.From2505$Chief_To == 2102,"cum_trips" ]) / sum(data_admin3.From2505[, "cum_trips"]) # 33.6% of all trips are to Makeni Town

sum(data_admin3.From2505[data_admin3.From2505$Chief_To == 2102,"cum_trips" ]) / sum(data_admin3.From2505[data_admin3.From2505$Chief_To >= 2000 & data_admin3.From2505$Chief_To < 3000, "cum_trips"]) # 40.2% of travel to the North region is to Makeni Town

sum(data_admin3.From2505[data_admin3.From2505$Chief_To == 2102,"cum_trips" ]) / sum(data_admin3.From2505[data_admin3.From2505$Chief_To >= 2100 & data_admin3.From2505$Chief_To < 2200, "cum_trips"]) # 94.3% of travel to Bombali District is to Makeni Town

sum(data_admin3.From2505[data_admin3.From2505$Chief_To >= 2100 & data_admin3.From2505$Chief_To < 2200,"cum_trips" ]) / sum(data_admin3.From2505[, "cum_trips"]) # 35.7% of all travel is to Bombali District

sum(data_admin3.From2505[data_admin3.From2505$Chief_To >= 2500 & data_admin3.From2505$Chief_To < 2600,"cum_trips" ]) / sum(data_admin3.From2505[, "cum_trips"]) # 44.0% of all travel is to Tonkolili District

sum(data_admin3.From2505[data_admin3.From2505$Chief_To == 2510 ,"cum_trips" ]) / sum(data_admin3.From2505[data_admin3.From2505$Chief_To >= 2500 & data_admin3.From2505$Chief_To < 2600, "cum_trips"]) # 47.1% of all travel to Tonkolili District is to Tane chiefdom

sum(data_admin3.From2505[data_admin3.From2505$Chief_To == 2510 ,"cum_trips" ]) / sum(data_admin3.From2505[data_admin3.From2505$Chief_To >= 2500 & data_admin3.From2505$Chief_To < 2600, "cum_trips"]) # 47.1% of all travel to Tonkolili District is to Tane chiefdom

sum(data_admin3.From2505[data_admin3.From2505$Chief_To == 2102 | data_admin3.From2505$Chief_To == 2102,"cum_trips" ]) / sum(data_admin3.From2505[data_admin3.From2505$Chief_To < 2000, "cum_trips"])
sum(data_admin3.From2505[data_admin3.From2505$Chief_To == 3108, "cum_trips"]) / sum(data_admin3.From2505[data_admin3.From2505$Chief_To > 3000 & data_admin3.From2505$Chief_To < 4000, "cum_trips"])

# Pie chart (Districts)
data_Tonk_districts <- data.frame(sort(unique(data_admin2[,1])))
names(data_Tonk_districts) <- c("district")
data_Tonk_districts$from <- NA
data_Tonk_districts$to <- NA
for (dist in data_Tonk_districts$district){
  cat(dist, "\n")
  data_Tonk_districts[data_Tonk_districts$district == dist, "from"] <- 
    sum(data_admin3[data_admin3$Chief_From == 2505 & floor(data_admin3$Chief_To/100) == dist, "cum_trips"])
  data_Tonk_districts[data_Tonk_districts$district == dist, "to"] <- 
    sum(data_admin3[data_admin3$Chief_To == 2505 & floor(data_admin3$Chief_From/100) == dist, "cum_trips"])
}
data_Tonk_districts$region <- floor(data_Tonk_districts$district/10)
data_Tonk_districts$region <- factor(data_Tonk_districts$region, 
                                        levels = c(2, 4, 1, 3),
                                        labels = c("North", "West", "East", "South"))

data_Tonk_districts$district_name <- c("Kailahun", "Kenema", "Kono", "Bombali", "Kambia", "Koinadugu", "Port Loko", "Tonkolili", "Bo", "Bonthe", "Moyamba", "Pujehun", "Western Area Rural", "Western Area Urban")

data_Tonk_districts <- data_Tonk_districts[order(data_Tonk_districts$to),] # Sort data so for horizontal bar chart

data_Tonk_districts$district <- factor(data_Tonk_districts$district, levels = data_Tonk_districts$district, 
                                          labels = data_Tonk_districts$district_name)

ggplot(data=data_Tonk_districts) +
  geom_bar(aes(x=district, y=from, fill = region), stat="identity") +
  #   xlab("Destination District Number") +
  ylab("Number of trips") +
  ggtitle("Trips from Kholifa Rowala, Tonkolili") +
  scale_fill_discrete(name = "Region") +
  theme_bw() + coord_flip() +
  theme(text = element_text(size=18)) +
  theme(axis.title.y = element_blank())

# ggplot(data=data_Tonk_districts) +
#   geom_bar(aes(x=factor(district), y=to, fill = factor(region)), stat="identity") +
#   xlab("Destination District Number") +
#   ylab("Number of trips") +
#   ggtitle("Trips to Kholifa Rowala, Tonkolili") +
#   scale_fill_discrete(name = "Region", labels=c("East","North","South", "West")) +
#   theme_bw()
# 
# ggplot(data=data_Tonk_districts) +
#   geom_bar(aes(x=factor(district), y=from, fill = factor(region)), stat="identity") +
#   xlab("Source District Number") +
#   ylab("Number of trips") +
#   ggtitle("Trips from Kholifa Rowala, Tonkolili") +
#   scale_fill_discrete(name = "Region",labels=c("East","North","South", "West")) +
#   theme_bw()

#### Spatial for Tonkolili ####
tonk <- data.frame(matrix(rep(NA, nrow(admin3.sp)*3), ncol=3))
names(tonk) <- c("CHCODE", "from_tonk", "to_tonk")
for (row in 1:nrow(admin3.sp)){
  chief <- admin3.sp$CHCODE[row]
  tonk[row, "CHCODE"] <- chief
  if (is.element(chief, data_admin3$Chief_From) & is.element(chief, data_admin3$Chief_To) & chief != 2505){
    tonk[row, "from_tonk"] <-  max(0, data_admin3[data_admin3$Chief_From == 2505 & data_admin3$Chief_To == chief, "cum_trips"])
    tonk[row, "to_tonk"] <-  max(0, data_admin3[data_admin3$Chief_To == 2505 & data_admin3$Chief_From == chief, "cum_trips"])
  } else {
    tonk[row, "from_tonk"] <-  0
    tonk[row, "to_tonk"] <-  0
  }
}
admin3.sp.fort$from_tonk <- NA
admin3.sp.fort$to_tonk <- NA

for (row in 1:nrow(admin3.sp.fort)){
  if (is.element(admin3.sp.fort[row,"CHCODE"], tonk$CHCODE)){
    admin3.sp.fort[row, "from_tonk"] <- tonk[tonk$CHCODE == admin3.sp.fort[row,"CHCODE"],"from_tonk"]
    admin3.sp.fort[row, "to_tonk"] <- tonk[tonk$CHCODE == admin3.sp.fort[row,"CHCODE"],"to_tonk"]
  } else {
    admin3.sp.fort[row, "from_tonk"] <- 0
    admin3.sp.fort[row, "to_tonk"] <- 0
  }
}

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(from_tonk+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1)

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = from_tonk/sum(tonk$from_tonk), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1)

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(to_tonk+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1)

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = to_tonk/sum(tonk$to_tonk), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1)

#### Kambia Plots ####
# Focus on Tonko Limba Chiefdom (2207) in Kambia District
# http://www.reuters.com/article/2015/09/02/us-health-ebola-leone-idUSKCN0R22CV20150902
# http://www.cidrap.umn.edu/news-perspective/2015/09/more-ebola-sierra-leone-dallas-probe-notes-missteps
hist(log(data_admin3[data_admin3$Chief_From == 2207, "cum_trips"]), xlab = "log(cumulative trips)", main = "Trips from Tonko Limba, Kambia")
data_admin3 <- data_admin3[order(-data_admin3$cum_trips),]

plot(data_admin3[data_admin3$Chief_From == 2207, "cum_trips"], 
     main = "Trips from Tonko Limba, Kambia", xlab = "Chiefdom Rank", ylab = "Cumulative Trips")

# Pie chart (Chiefdoms)
lbls <- paste(data_admin3[data_admin3$Chief_From == 2207,"Chief_To"], "\n", round(data_admin3[data_admin3$Chief_From == 2207,"cum_trips"]/sum(data_admin3[data_admin3$Chief_From == 2207,"cum_trips"])*100, 0),"%", sep="")
pie(x = data_admin3[data_admin3$Chief_From == 2207,"cum_trips"], labels = lbls, main="Chiefdom Destinations\n from Tonko Limba, Kambia (2207)", )

lbls <- paste(data_admin3[data_admin3$Chief_To == 2207,"Chief_From"], "\n", round(data_admin3[data_admin3$Chief_To == 2207,"cum_trips"]/sum(data_admin3[data_admin3$Chief_To == 2207,"cum_trips"])*100, 0),"%", sep="")
pie(x = data_admin3[data_admin3$Chief_To == 2207,"cum_trips"], labels = lbls, main="Chiefdom Departures\n to Tonko Limba, Kambia (2207)", )

# List of chiefdom destinations
data_admin3.From2207 <- data_admin3[data_admin3$Chief_From == 2207,c("Chief_From","Chief_To", "cum_trips")]
# View(data_admin3.From2207[order(-data_admin3.From2207$cum_trips),])

sum(data_admin3.From2207[data_admin3.From2207$Chief_To >= 2000 & data_admin3.From2207$Chief_To < 3000,"cum_trips" ]) / sum(data_admin3.From2207[, "cum_trips"]) # 86.4% of all travel is to the North region

sum(data_admin3.From2207[data_admin3.From2207$Chief_To > 4000 ,"cum_trips" ]) / sum(data_admin3.From2207[data_admin3.From2207$Chief_To < 2000 | data_admin3.From2207$Chief_To >= 3000, "cum_trips"]) # 95.0% of all travel outside of the North region is to Western Area

sum(data_admin3.From2207[data_admin3.From2207$Chief_To == 2102,"cum_trips" ]) / sum(data_admin3.From2207[data_admin3.From2207$Chief_To >= 2100 & data_admin3.From2207$Chief_To < 2200, "cum_trips"]) # Only 9.9% of all trips to Bombali District are to Makeni Town

sum(data_admin3.From2207[data_admin3.From2207$Chief_To == 2112,"cum_trips" ]) / sum(data_admin3.From2207[data_admin3.From2207$Chief_To >= 2100 & data_admin3.From2207$Chief_To < 2200, "cum_trips"]) # 52.1% of all trips to Bombali District are to Sella Limba

sum(data_admin3.From2207[data_admin3.From2207$Chief_To == 2111,"cum_trips" ]) / sum(data_admin3.From2207[data_admin3.From2207$Chief_To >= 2100 & data_admin3.From2207$Chief_To < 2200, "cum_trips"]) # 35.5% of all trips to Bombali District are to Sandra Tenda

sum(data_admin3.From2207[data_admin3.From2207$Chief_To == 2111 | data_admin3.From2207$Chief_To == 2112,"cum_trips" ]) / sum(data_admin3.From2207[data_admin3.From2207$Chief_To >= 2100 & data_admin3.From2207$Chief_To < 2200, "cum_trips"]) # 87.7% of all trips to Bombali District are to Sella Limba or Sandra Tenda

sum(data_admin3.From2207[data_admin3.From2207$Chief_To == 2201,"cum_trips" ]) / sum(data_admin3.From2207[data_admin3.From2207$Chief_To >= 2200 & data_admin3.From2207$Chief_To < 2300, "cum_trips"]) # 71.2% of travel to the Kambia District is to Bramala Chiefdom

sum(data_admin3.From2207[data_admin3.From2207$Chief_To == 2203,"cum_trips" ]) / sum(data_admin3.From2207[data_admin3.From2207$Chief_To >= 2200 & data_admin3.From2207$Chief_To < 2300, "cum_trips"]) # 25.8% of travel to the Kambia District is to Magbema Chiefdom

sum(data_admin3.From2207[data_admin3.From2207$Chief_To == 2201 | data_admin3.From2207$Chief_To == 2203,"cum_trips" ]) / sum(data_admin3.From2207[data_admin3.From2207$Chief_To >= 2200 & data_admin3.From2207$Chief_To < 2300, "cum_trips"]) # 97.1% of travel to the Kambia District is to Bramala or Magbema Chiefdoms

# Pie chart (Districts)
data_Kambia_districts <- data.frame(sort(unique(data_admin2[,1])))
names(data_Kambia_districts) <- c("district")
data_Kambia_districts$from <- NA
data_Kambia_districts$to <- NA
for (dist in data_Kambia_districts$district){
  cat(dist, "\n")
  data_Kambia_districts[data_Kambia_districts$district == dist, "from"] <- 
    sum(data_admin3[data_admin3$Chief_From == 2207 & floor(data_admin3$Chief_To/100) == dist, "cum_trips"])
  data_Kambia_districts[data_Kambia_districts$district == dist, "to"] <- 
    sum(data_admin3[data_admin3$Chief_To == 2207 & floor(data_admin3$Chief_From/100) == dist, "cum_trips"])
}
data_Kambia_districts$region <- floor(data_Kambia_districts$district/10)
data_Kambia_districts$region <- factor(data_Kambia_districts$region, 
                                     levels = c(2, 4, 1, 3),
                                     labels = c("North", "West", "East", "South"))

data_Kambia_districts$district_name <- c("Kailahun", "Kenema", "Kono", "Bombali", "Kambia", "Koinadugu", "Port Loko", "Tonkolili", "Bo", "Bonthe", "Moyamba", "Pujehun", "Western Area Rural", "Western Area Urban")

data_Kambia_districts <- data_Kambia_districts[order(data_Kambia_districts$to),] # Sort data so for horizontal bar chart

data_Kambia_districts$district <- factor(data_Kambia_districts$district, levels = data_Kambia_districts$district, 
                                       labels = data_Kambia_districts$district_name)

ggplot(data=data_Kambia_districts) +
  geom_bar(aes(x=district, y=from, fill = region), stat="identity") +
  #   xlab("Destination District Number") +
  ylab("Number of trips") +
  ggtitle("Trips from Tonko Limba, Kambia") +
  scale_fill_discrete(name = "Region") +
  theme_bw() + coord_flip() +
  theme(text = element_text(size=18)) +
  theme(axis.title.y = element_blank())

# Pie chart (Districts)
data_kambia_districts <- data.frame(sort(unique(data_admin2[,1])))
names(data_kambia_districts) <- c("district")
data_kambia_districts$from <- NA
data_kambia_districts$to <- NA
for (dist in data_kambia_districts$district){
  cat(dist, "\n")
  data_kambia_districts[data_kambia_districts$district == dist, "from"] <- 
    sum(data_admin3[data_admin3$Chief_From == 2207 & floor(data_admin3$Chief_To/100) == dist, "cum_trips"])
  data_kambia_districts[data_kambia_districts$district == dist, "to"] <- 
    sum(data_admin3[data_admin3$Chief_To == 2207 & floor(data_admin3$Chief_From/100) == dist, "cum_trips"])
}
data_kambia_districts$region <- floor(data_kambia_districts$district/10)

# lbls <- paste(data_kambia_districts$district, "\n", round(data_kambia_districts$from/sum(data_kambia_districts$from)*100, 0),"%", sep="")
# pie(x = data_kambia_districts$from, labels = lbls, main="District Destinations\n from Tonko Limba, Kambia (2207)")
# 
# lbls <- paste(data_kambia_districts$district, "\n", round(data_kambia_districts$to/sum(data_kambia_districts$to)*100, 0),"%", sep="")
# pie(x = data_kambia_districts$to, labels = lbls, main="District Destinations\n to Tonko Limba, Kambia (2207)")

ggplot(data=data_kambia_districts) +
  geom_bar(aes(x=factor(district), y=from, fill = factor(region)), stat="identity") +
  xlab("Source District Number") +
  ylab("Number of trips") +
  ggtitle("Trips from Tonko Limba, Kambia") +
  scale_fill_discrete(name = "Region",labels=c("East","North","South", "West")) +
  theme_bw()

sum(data_kambia_districts[data_kambia_districts$district < 30 & data_kambia_districts$district >= 20, "from"]) / sum(data_kambia_districts[, "from"])
sum(data_kambia_districts[data_kambia_districts$district >= 40, "from"]) / sum(data_kambia_districts[, "from"])

ggplot(data=data_kambia_districts) +
  geom_bar(aes(x=factor(district), y=to, fill = factor(region)), stat="identity") +
  xlab("Destination District Number") +
  ylab("Number of trips") +
  ggtitle("Trips to Tonko Limba, Kambia") +
  scale_fill_discrete(name = "Region", labels=c("East","North","South", "West")) +
  theme_bw()

#### Spatial for Kambia ####

sella_coords <- c(9.252489, -12.773020)

kambia <- data.frame(matrix(rep(NA, nrow(admin3.sp)*3), ncol=3))
names(kambia) <- c("CHCODE", "from_kambia", "to_kambia")
for (row in 1:nrow(admin3.sp)){
  chief <- admin3.sp$CHCODE[row]
  kambia[row, "CHCODE"] <- chief
  if (is.element(chief, data_admin3$Chief_From) & is.element(chief, data_admin3$Chief_To) & chief != 2207){
    kambia[row, "from_kambia"] <-  max(0, data_admin3[data_admin3$Chief_From == 2207 & data_admin3$Chief_To == chief, "cum_trips"])
    kambia[row, "to_kambia"] <-  max(0, data_admin3[data_admin3$Chief_To == 2207 & data_admin3$Chief_From == chief, "cum_trips"])
  } else {
    kambia[row, "from_kambia"] <-  0
    kambia[row, "to_kambia"] <-  0
  }
}
admin3.sp.fort$from_kambia <- NA
admin3.sp.fort$to_kambia <- NA

for (row in 1:nrow(admin3.sp.fort)){
  if (is.element(admin3.sp.fort[row,"CHCODE"], kambia$CHCODE)){
    admin3.sp.fort[row, "from_kambia"] <- kambia[kambia$CHCODE == admin3.sp.fort[row,"CHCODE"],"from_kambia"]
    admin3.sp.fort[row, "to_kambia"] <- kambia[kambia$CHCODE == admin3.sp.fort[row,"CHCODE"],"to_kambia"]
  } else {
    admin3.sp.fort[row, "from_kambia"] <- 0
    admin3.sp.fort[row, "to_kambia"] <- 0
  }
}

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(from_kambia+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1) +
  geom_point(aes(x=sella_coords[2], y=sella_coords[1]), shape = "X", size=3, color="red")

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = from_kambia/sum(kambia$from_kambia), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1) +
  geom_point(aes(x=sella_coords[2], y=sella_coords[1]), shape = "X", size=3, color="red")

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(to_kambia+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white")+
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1) +
  geom_point(aes(x=sella_coords[2], y=sella_coords[1]), shape = "X", size=3, color="red")

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = to_kambia/sum(kambia$to_kambia), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1) +
  geom_point(aes(x=sella_coords[2], y=sella_coords[1]), shape = "X", size=3, color="red")

#### Bombali Plots ####
# Focus on Makeni Town Chiefdom (2102 by aggregation) in Bombali District
# Note that the quarantined village of Robuya, where she lived, is within a mile of Makeni Town, where she died.
# There may be a closer tower to consider
hist(log(data_admin3[data_admin3$Chief_From == 2102, "cum_trips"]), xlab = "log(cumulative trips)", main = "Trips from Makeni Town, Bombali")
data_admin3 <- data_admin3[order(-data_admin3$cum_trips),]

plot(data_admin3[data_admin3$Chief_From == 2102, "cum_trips"], 
     main = "Trips from Makeni Town, Bombali", xlab = "Chiefdom Rank", ylab = "Cumulative Trips")

# Pie chart (Chiefdoms)
lbls <- paste(data_admin3[data_admin3$Chief_From == 2102,"Chief_To"], "\n", round(data_admin3[data_admin3$Chief_From == 2102,"cum_trips"]/sum(data_admin3[data_admin3$Chief_From == 2102,"cum_trips"])*100, 0),"%", sep="")
pie(x = data_admin3[data_admin3$Chief_From == 2102,"cum_trips"], labels = lbls, main="Chiefdom Destinations\n from Makeni Town, Bombali (2102)", )

lbls <- paste(data_admin3[data_admin3$Chief_To == 2102,"Chief_From"], "\n", round(data_admin3[data_admin3$Chief_To == 2102,"cum_trips"]/sum(data_admin3[data_admin3$Chief_To == 2102,"cum_trips"])*100, 0),"%", sep="")
pie(x = data_admin3[data_admin3$Chief_To == 2102,"cum_trips"], labels = lbls, main="Chiefdom Departures\n to Makeni Town, Bombali (2102)", )

# List of chiefdom destinations
data_admin3.From2102 <- data_admin3[data_admin3$Chief_From == 2102,c("Chief_From","Chief_To", "cum_trips")]
# View(data_admin3.From2102[order(-data_admin3.From2102$cum_trips),])
sum(data_admin3.From2102[data_admin3.From2102$Chief_To == 1212 | data_admin3.From2102$Chief_To == 1313,"cum_trips" ]) / sum(data_admin3.From2102[data_admin3.From2102$Chief_To < 2000, "cum_trips"])
sum(data_admin3.From2102[data_admin3.From2102$Chief_To == 3108, "cum_trips"]) / sum(data_admin3.From2102[data_admin3.From2102$Chief_To > 3000 & data_admin3.From2102$Chief_To < 4000, "cum_trips"])


data_admin3.From2102[data_admin3.From2102$Chief_To == 1212 | data_admin3.From2102$Chief_To == 1313, ] / data_admin3.From2102[data_admin3.From2102$Chief_To < 2000, ])


# Pie chart (Districts)
data_Bombali_districts <- data.frame(sort(unique(data_admin2[,1])))
names(data_Bombali_districts) <- c("district")
data_Bombali_districts$from <- NA
data_Bombali_districts$to <- NA
for (dist in data_Bombali_districts$district){
  cat(dist, "\n")
  data_Bombali_districts[data_Bombali_districts$district == dist, "from"] <- 
    sum(data_admin3[data_admin3$Chief_From == 2102 & floor(data_admin3$Chief_To/100) == dist, "cum_trips"])
  data_Bombali_districts[data_Bombali_districts$district == dist, "to"] <- 
    sum(data_admin3[data_admin3$Chief_To == 2102 & floor(data_admin3$Chief_From/100) == dist, "cum_trips"])
}
data_Bombali_districts$region <- floor(data_Bombali_districts$district/10)
data_Bombali_districts$region <- factor(data_Bombali_districts$region, 
                                        levels = c(2, 4, 1, 3),
                                        labels = c("North", "West", "East", "South"))

data_Bombali_districts$district_name <- c("Kailahun", "Kenema", "Kono", "Bombali", "Kambia", "Koinadugu", "Port Loko", "Tonkolili", "Bo", "Bonthe", "Moyamba", "Pujehun", "Western Area Rural", "Western Area Urban")

data_Bombali_districts <- data_Bombali_districts[order(data_Bombali_districts$to),] # Sort data so for horizontal bar chart

data_Bombali_districts$district <- factor(data_Bombali_districts$district, levels = data_Bombali_districts$district, 
                                          labels = data_Bombali_districts$district_name)

plot <- 
ggplot(data=data_Bombali_districts) +
  geom_bar(aes(x=district, y=from, fill = region), stat="identity") +
#   xlab("Destination District Number") +
  ylab("Number of trips") +
  ggtitle("Trips from Makeni Town, Bombali") +
  scale_fill_discrete(name = "Region") +
  theme_bw() + coord_flip() +
  theme(text = element_text(size=18)) +
  theme(axis.title.y = element_blank())

setwd("/Users/peakcm/Desktop")
ggsave(plot, file="MakeniBars.png", dpi = 1000)

ggplot(data=data_Bombali_districts) +
  geom_bar(aes(x=district, y=to, fill = factor(region)), stat="identity") +
  xlab("Source District Number") +
  ylab("Number of trips") +
  ggtitle("Trips to Makeni Town, Bombali") +
  scale_fill_discrete(name = "Region", labels=c("East","North","South", "West")) +
  theme_bw() + coord_flip()

# Report proportion of trips instead
sum_from <- sum(data_Bombali_districts$from)
plot <- 
  ggplot(data=data_Bombali_districts) +
  geom_bar(aes(x=district, y=from/sum_from, fill = region), stat="identity") +
  #   xlab("Destination District Number") +
  ylab("Fraction of trips") +
  ggtitle("Top Destinations of Travel from Makeni Town") +
  scale_fill_discrete(name = "Region") +
  theme_bw() + coord_flip() +
  theme(text = element_text(size=18)) +
  theme(axis.title.y = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())

setwd("/Users/peakcm/Desktop")
ggsave(plot, file="MakeniBars.png", dpi = 1000)

#### Spatial for Bombali ####

robuya_coords <- c(8.874, -12)

bombali <- data.frame(matrix(rep(NA, nrow(admin3.sp)*3), ncol=3))
names(bombali) <- c("CHCODE", "from_bombali", "to_bombali")
for (row in 1:nrow(admin3.sp)){
  chief <- admin3.sp$CHCODE[row]
  bombali[row, "CHCODE"] <- chief
  if (is.element(chief, data_admin3$Chief_From) & is.element(chief, data_admin3$Chief_To) & chief != 2102){
    bombali[row, "from_bombali"] <-  max(0, data_admin3[data_admin3$Chief_From == 2102 & data_admin3$Chief_To == chief, "cum_trips"])
    bombali[row, "to_bombali"] <-  max(0, data_admin3[data_admin3$Chief_To == 2102 & data_admin3$Chief_From == chief, "cum_trips"])
  } else {
    bombali[row, "from_bombali"] <-  0
    bombali[row, "to_bombali"] <-  0
  }
}
admin3.sp.fort$from_bombali <- NA
admin3.sp.fort$to_bombali <- NA

for (row in 1:nrow(admin3.sp.fort)){
  if (is.element(admin3.sp.fort[row,"CHCODE"], bombali$CHCODE)){
    admin3.sp.fort[row, "from_bombali"] <- bombali[bombali$CHCODE == admin3.sp.fort[row,"CHCODE"],"from_bombali"]
    admin3.sp.fort[row, "to_bombali"] <- bombali[bombali$CHCODE == admin3.sp.fort[row,"CHCODE"],"to_bombali"]
  } else {
    admin3.sp.fort[row, "from_bombali"] <- 0
    admin3.sp.fort[row, "to_bombali"] <- 0
  }
}

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = from_bombali, group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2102,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Makeni Town, Bombali") +
  scale_fill_continuous(name = "Number of Trips", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1) +
  geom_polygon(data = admin2.sp.fort, aes(x=long, y=lat, group=group), colour = "black", fill=NA, linetype = 1, lwd = 0.3) +
  geom_polygon(data = admin1.sp.fort, aes(x=long, y=lat, group=group), colour = "black", fill=NA, linetype = 1, lwd = 0.3) +
  geom_point(aes(x=robuya_coords[2], y=robuya_coords[1]), shape = "X", size=3, color="red") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.border = element_blank())
  
ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = from_bombali/sum(bombali$from_bombali), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2102,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Makeni Town, Bombali") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1) +
  geom_point(aes(x=robuya_coords[2], y=robuya_coords[1]), shape = "X", size=3, color="red")

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = to_bombali, group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2102,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Makeni Town, Bombali") +
  scale_fill_continuous(name = "Number of Trips", low = "white")+
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1) +
  geom_point(aes(x=robuya_coords[2], y=robuya_coords[1]), shape = "X", size=3, color="red")

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = to_bombali/sum(bombali$to_bombali), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2102,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Makeni Town, Bombali") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1) +
  geom_point(aes(x=robuya_coords[2], y=robuya_coords[1]), shape = "X", size=3, color="red")

#### Find Chiefdoms without towers ####
length(unique(data_admin3$Chief_From)) / length(unique(towers$CHCODE)) #97% of chiefdoms with towers have travel reports

(length(unique(towers$CHCODE))) / (length(admin3.sp$CHCODE)) #58.6% of chiefdoms in Sierra Leone have at least one tower

unique(towers$CHCODE)
sort(admin3.sp$CHCODE)
sort(data_chief_pop$CHCODE) # aggregates Western Area

is.element(unique(towers$CHCODE), admin3.sp$CHCODE)
admin3.sp$CHCODE[is.element(admin3.sp$CHCODE, unique(towers$CHCODE))==1] #Chiefdoms with towers
admin3.sp$CHCODE[is.element(admin3.sp$CHCODE, unique(towers$CHCODE))==0] #Chiefdoms without towers

data_chief_pop[is.element(data_chief_pop$CHCODE, admin3.sp$CHCODE[is.element(admin3.sp$CHCODE, unique(towers$CHCODE))==0]), ]

hist(data_chief_pop[is.element(data_chief_pop$CHCODE, admin3.sp$CHCODE[is.element(admin3.sp$CHCODE, unique(towers$CHCODE))==0]), "Pop_2012_est"])

sum(data_chief_pop[is.element(data_chief_pop$CHCODE, admin3.sp$CHCODE[is.element(admin3.sp$CHCODE, unique(towers$CHCODE))==0]), "Pop_2012_est"]) /
sum(data_chief_pop[, "Pop_2012_est"])

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(from_tonk+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[is.element(admin3.sp.fort$CHCODE, towers$CHCODE)==0,], aes(x = long, y = lat, group = group), size=1.2, fill = "lightgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1)
  
#   geom_polygon(data = towers_buffer.fort, aes(x=long, y=lat), color = "grey")

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(from_kambia+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[is.element(admin3.sp.fort$CHCODE, towers$CHCODE)==0,], aes(x = long, y = lat, group = group), size=1.2, fill = "lightgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1) +
  geom_point(aes(x=sella_coords[2], y=sella_coords[1]), shape = "X", size=3, color="red") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="grey", size=18, pch = 1)

#### Country-wide maps ####
# What proportion of travel is through Freetown?
sum(data_admin3[data_admin3$Chief_From > 4000, "cum_trips"])
sum(data_admin3[data_admin3$Chief_From < 4000, "cum_trips"])

sum(data_admin3[data_admin3$Chief_From > 4000, "cum_trips"]) / sum(data_admin3[, "cum_trips"])
sum(data_admin3[data_admin3$Chief_From > 4000 & data_admin3$Chief_To > 4000, "cum_trips"]) / sum(data_admin3[, "cum_trips"])
sum(data_admin3[data_admin3$Chief_From > 4000 | data_admin3$Chief_To > 4000, "cum_trips"]) / sum(data_admin3[, "cum_trips"])

# What proportion of travel is through Makeni Town?
sum(data_admin3[data_admin3$Chief_From == 2102, "cum_trips"])
sum(data_admin3[data_admin3$Chief_From == 2102 | data_admin3$Chief_To == 2102, "cum_trips"])
sum(data_admin3[data_admin3$Chief_From != 2102, "cum_trips"])

sum(data_admin3[data_admin3$Chief_From == 2102, "cum_trips"]) / sum(data_admin3[, "cum_trips"])
sum(data_admin3[data_admin3$Chief_From == 2102 | data_admin3$Chief_To == 2102, "cum_trips"]) / sum(data_admin3[, "cum_trips"])

# Map of number of trips from each chiefdom
admin3.sp.fort$trips_from <- NA
admin3.sp.fort$trips_to <- NA

for (chief in unique(admin3.sp.fort$CHCODE)){
  admin3.sp.fort[admin3.sp.fort$CHCODE == chief, "trips_from"] <- sum(data_admin3[data_admin3$Chief_From == chief,"cum_trips"]) 
  admin3.sp.fort[admin3.sp.fort$CHCODE == chief, "trips_to"] <- sum(data_admin3[data_admin3$Chief_To == chief,"cum_trips"]) 
}

plot <- 
ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(trips_from+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[is.element(admin3.sp.fort$CHCODE, towers$CHCODE)==0,], aes(x = long, y = lat, group = group), size=1.2, fill = "lightgrey") +
  coord_equal() +
  theme_bw() + 
#   ggtitle("Cumulative Trips From Each Chiefdom") +
  scale_fill_continuous(name = "Number of Trips",
                        breaks = c(0, 2, 4, 6),
                        labels = c("1", "100", "10,000", "1,000,000"),
                        low="white", high="darkblue") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1)  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

setwd("/Users/peakcm/Desktop")
ggsave(plot, file="tripsfromchiefs.png", dpi = 1000)

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(trips_to+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[is.element(admin3.sp.fort$CHCODE, towers$CHCODE)==0,], aes(x = long, y = lat, group = group), size=1.2, fill = "lightgrey") +
  coord_equal() +
  theme_bw() + ggtitle("Cumulative Trips To Each Chiefdom") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white") +
  geom_point(data=towers.fort, aes(x=Long, y=Lat ), color="black", size=1)

# Map all connections
plot(admin3.sp)

for (row in 1:nrow(data_admin3)){
  from <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_From"])
  to <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_To"])
  
  lon_1 <- admin3.sp$centroids.Long[from]
  lat_1 <- admin3.sp$centroids.Lat[from]
  lon_2 <- admin3.sp$centroids.Long[to]
  lat_2 <- admin3.sp$centroids.Lat[to]
  inter <- gcIntermediate(c(lon_1, lat_1), c(lon_2, lat_2), n=50, addStartEnd=TRUE)
  lines(inter)
}

# Shade connections by weight
data_admin3 <- data_admin3[order(data_admin3$cum_trips),] #sort so the heavier connections are drawn later

colors <- colorRampPalette(brewer.pal(9,"Blues"))(100)

range <- max(data_admin3$cum_trips)
  
plot(admin3.sp)

for (row in 1:nrow(data_admin3)){
  from <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_From"])
  to <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_To"])
  
  lon_1 <- admin3.sp$centroids.Long[from]
  lat_1 <- admin3.sp$centroids.Lat[from]
  lon_2 <- admin3.sp$centroids.Long[to]
  lat_2 <- admin3.sp$centroids.Lat[to]
  inter <- gcIntermediate(c(lon_1, lat_1), c(lon_2, lat_2), n=50, addStartEnd=TRUE)
  
  colindex <- colors[ceiling(data_admin3[row,"cum_trips"] / range * 100)]
  lines(inter, col = colindex, lwd = (2*ceiling(data_admin3[row,"cum_trips"] / range))^2)
}

# Shade connections weighted by log of count
data_admin3 <- data_admin3[order(data_admin3$cum_trips),] #sort so the heavier connections are drawn later

range <- max(log(data_admin3$cum_trips))
colors <- colorRampPalette(brewer.pal(9,"Blues"))(100)
# To set transparency to ramp up with color
for (i in 1:length(colors)){
  if (i < 20){alpha <- 20
  } else if (i > 80){alpha <- 80
  } else {alpha <- i}
  
  colors[i] <- paste0(colors[i], as.character(alpha))
}

# dev.off()
# png(file="allroutes.png")

plot(admin3.sp)
# plot(admin1.sp[admin1.sp$DISCODE == 2,], add=TRUE, border = "red", lwd = 3, lty = 2)
# plot(admin1.sp[admin1.sp$DISCODE == 3,], add=TRUE, border = "purple", lwd = 3, lty = 2)
# plot(admin1.sp[admin1.sp$DISCODE == 1,], add=TRUE, border = "turquoise", lwd = 3, lty = 2)
# plot(admin1.sp[admin1.sp$DISCODE == 4,], add=TRUE, border = "green", lwd = 3, lty = 2)

geom_polygon(data = admin1.sp.fort, aes(x=long, y=lat, group=group), colour = "black", fill=NA, linetype = 1, lwd = 0.3) +
  
for (row in 1:nrow(data_admin3)){
  from <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_From"])
  to <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_To"])
  
  lon_1 <- admin3.sp$centroids.Long[from]
  lat_1 <- admin3.sp$centroids.Lat[from]
  lon_2 <- admin3.sp$centroids.Long[to]
  lat_2 <- admin3.sp$centroids.Lat[to]
  inter <- gcIntermediate(c(lon_1, lat_1), c(lon_2, lat_2), n=50, addStartEnd=TRUE)
  
  weight <- ceiling(log(data_admin3[row,"cum_trips"]) / range * 100) / 100
    
  lines(inter, col = colors[weight*100], lwd = (2.5*weight)^2)
}
# points(towers.fort$Long, towers.fort$Lat)
text(x = robuya_coords[2]+0.25, y = robuya_coords[1]-0.05, label = "Makeni", col = "black", font = 2)
text(x = robuya_coords[2]-1.65, y = robuya_coords[1]-0.45, label = "Freetown", col = "black", font = 2)
# text(x = robuya_coords[2]+0.5, y = robuya_coords[1]-0.8, label = "Bo", col = "white")

# Travel from Freetown. shade connections weighted by log of count
# dev.off()
plot(admin3.sp)
for (row in 1:nrow(data_admin3)){
  from <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_From"])
  to <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_To"])
  
  if (data_admin3[row, "Chief_From"] > 4100){
    lon_1 <- admin3.sp$centroids.Long[from]
    lat_1 <- admin3.sp$centroids.Lat[from]
    lon_2 <- admin3.sp$centroids.Long[to]
    lat_2 <- admin3.sp$centroids.Lat[to]
    inter <- gcIntermediate(c(lon_1, lat_1), c(lon_2, lat_2), n=50, addStartEnd=TRUE)
  }
  
  weight <- ceiling(log(data_admin3[row,"cum_trips"]) / range * 100) / 100
  
  lines(inter, col = colors[weight*100], lwd = (2.5*weight)^2)
}

# Travel from Bo Town shade connections weighted by log of count
# dev.off()
plot(admin3.sp)
for (row in 1:nrow(data_admin3)){
  from <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_From"])
  to <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_To"])
  
  if (data_admin3[row, "Chief_From"] == 3108){
    lon_1 <- admin3.sp$centroids.Long[from]
    lat_1 <- admin3.sp$centroids.Lat[from]
    lon_2 <- admin3.sp$centroids.Long[to]
    lat_2 <- admin3.sp$centroids.Lat[to]
    inter <- gcIntermediate(c(lon_1, lat_1), c(lon_2, lat_2), n=50, addStartEnd=TRUE)
  }
  
  weight <- ceiling(log(data_admin3[row,"cum_trips"]) / range * 100) / 100
  
  lines(inter, col = colors[weight*100], lwd = (2.5*weight)^2)
}

# Travel from Kenema Town. shade connections weighted by log of count
# dev.off()
plot(admin3.sp)
for (row in 1:nrow(data_admin3)){
  from <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_From"])
  to <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_To"])
  
  if (data_admin3[row, "Chief_From"] == 1212){
    lon_1 <- admin3.sp$centroids.Long[from]
    lat_1 <- admin3.sp$centroids.Lat[from]
    lon_2 <- admin3.sp$centroids.Long[to]
    lat_2 <- admin3.sp$centroids.Lat[to]
    inter <- gcIntermediate(c(lon_1, lat_1), c(lon_2, lat_2), n=50, addStartEnd=TRUE)
  }
  
  weight <- ceiling(log(data_admin3[row,"cum_trips"]) / range * 100) / 100
  
  lines(inter, col = colors[weight*100], lwd = (2.5*weight)^2)
}

# Travel from Tonko Limba shade connections weighted by log of count
# dev.off()
plot(admin3.sp)
for (row in 1:nrow(data_admin3)){
  from <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_From"])
  to <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_To"])
  
  if (data_admin3[row, "Chief_From"] == 2207){
    lon_1 <- admin3.sp$centroids.Long[from]
    lat_1 <- admin3.sp$centroids.Lat[from]
    lon_2 <- admin3.sp$centroids.Long[to]
    lat_2 <- admin3.sp$centroids.Lat[to]
    inter <- gcIntermediate(c(lon_1, lat_1), c(lon_2, lat_2), n=50, addStartEnd=TRUE)
  }
  
  weight <- ceiling(log(data_admin3[row,"cum_trips"]) / range * 100) / 100
  
  lines(inter, col = colors[weight*100], lwd = (2.5*weight)^2)
}


# Travel from Makeni shade connections weighted by log of count
# dev.off()

# colors <- colorRampPalette(brewer.pal(9,"Greys"))(100)
# # To set transparency to ramp up with color
# for (i in 1:length(colors)){
#   if (i < 20){alpha <- 20
#   } else if (i > 80){alpha <- 80
#   } else {alpha <- i}
#   
#   colors[i] <- paste0(colors[i], as.character(alpha))
# }

plot(admin3.sp)

plot(admin1.sp[admin1.sp$DISCODE == 2,], add=TRUE, col = "#F35D5A95")
plot(admin1.sp[admin1.sp$DISCODE == 3,], add=TRUE, col = "#B95EFF95")
plot(admin1.sp[admin1.sp$DISCODE == 1,], add=TRUE, col = "#17B2B795")
plot(admin1.sp[admin1.sp$DISCODE == 4,], add=TRUE, col = "#6BA10395")

plot(admin3.sp, col = NA, add = TRUE)

for (row in 1:nrow(data_admin3)){
  from <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_From"])
  to <- which(admin3.sp$CHCODE==data_admin3[row, "Chief_To"])
  
  if (data_admin3[row, "Chief_From"] == 2102){
    lon_1 <- admin3.sp$centroids.Long[from]
    lat_1 <- admin3.sp$centroids.Lat[from]
    lon_2 <- admin3.sp$centroids.Long[to]
    lat_2 <- admin3.sp$centroids.Lat[to]
    inter <- gcIntermediate(c(lon_1, lat_1), c(lon_2, lat_2), n=50, addStartEnd=TRUE)
  }
  
  weight <- ceiling(log(data_admin3[row,"cum_trips"]) / range * 100) / 100
  
  lines(inter, col = colors[weight*100], lwd = (2.5*weight)^2)
}
# text(x = robuya_coords[2]+0.25, y = robuya_coords[1], label = "Makeni", col = "black", font = 2)
# text(x = robuya_coords[2]-1.65, y = robuya_coords[1]-0.45, label = "Freetown", col = "black", font = 2)

#### Country-wide temporal series ####
# Melt data_admin3 call data
data_admin3.melt <- melt(data_admin3, id = c("Chief_From", "Chief_To"))

# Create a dataset with the cumulative trips on each day
data_admin3.daily <- data.frame(matrix(rep(NA, 2*(ncol(data_admin3)-3)), ncol=2))
names(data_admin3.daily) <- c("day", "cum_trips")
data_admin3.daily$day <- names(data_admin3)[3:(ncol(data_admin3)-1)]

for (row in 1:nrow(data_admin3.daily)){
  data_admin3.daily[row, "cum_trips"] <- sum(data_admin3.melt[data_admin3.melt$variable == data_admin3.daily[row, "day"],"value"])
}
data_admin3.daily$weekday <- factor(c(rep(c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"), times = floor(nrow(data_admin3.daily)/7)), c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday")),levels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))
tail(data_admin3.daily)

ggplot(data = data_admin3.daily) +
  # "three days stay at home" March 27-29 nationally
  annotate("rect", xmin = as.numeric(as.Date(c("03/27/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("03/29/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily$cum_trips), alpha = .2) +
  # Three saturdays (April 4, 11, 18)
  annotate("rect", xmin = as.numeric(as.Date(c("04/04/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("04/04/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily$cum_trips), alpha = .2) + 
  annotate("rect", xmin = as.numeric(as.Date(c("04/11/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("04/11/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily$cum_trips), alpha = .2) + 
  annotate("rect", xmin = as.numeric(as.Date(c("04/18/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("04/18/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily$cum_trips), alpha = .2) + 
  geom_point(aes(x=as.numeric(day), y=cum_trips, col=weekday), size=3) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Day") + ylab("Cumulative Trips") + ggtitle("Daily Trips") +
  scale_x_continuous(breaks = c(as.numeric(as.Date(c("04/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("04/15/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("05/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("05/15/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("06/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("06/15/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("07/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("07/15/2015"), format = "%m/%d/%Y"))),
                     labels = c("04/01/2015", "04/15/2015", "05/01/2015", "05/15/2015",
                                "06/01/2015", "06/15/2015", "07/01/2015", "07/15/2015"))

# Focus on Freetown
data_admin3.daily.Freetown <- data.frame(matrix(rep(NA, 2*(ncol(data_admin3)-3)), ncol=2))
names(data_admin3.daily.Freetown) <- c("day", "cum_trips")
data_admin3.daily.Freetown$day <- names(data_admin3)[3:(ncol(data_admin3)-1)]

for (row in 1:nrow(data_admin3.daily.Freetown)){
  data_admin3.daily.Freetown[row, "cum_trips"] <- sum(data_admin3.melt[data_admin3.melt$Chief_From > 4000 & 
                                                                     data_admin3.melt$variable == data_admin3.daily[row, "day"],"value"])
}
data_admin3.daily.Freetown$weekday <- factor(c(rep(c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"), times = floor(nrow(data_admin3.daily.Freetown)/7)), c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday")),levels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))
tail(data_admin3.daily.Freetown)

ggplot(data = data_admin3.daily.Freetown) +
  # "three days stay at home" March 27-29 nationally
  annotate("rect", xmin = as.numeric(as.Date(c("03/27/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("03/29/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily.Freetown$cum_trips), alpha = .2) +
  # Three saturdays (April 4, 11, 18)
  annotate("rect", xmin = as.numeric(as.Date(c("04/04/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("04/04/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily.Freetown$cum_trips), alpha = .2) + 
  annotate("rect", xmin = as.numeric(as.Date(c("04/11/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("04/11/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily.Freetown$cum_trips), alpha = .2) + 
  annotate("rect", xmin = as.numeric(as.Date(c("04/18/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("04/18/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily.Freetown$cum_trips), alpha = .2) + 
  geom_point(aes(x=as.numeric(day), y=cum_trips, col=weekday), size=3) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Day") + ylab("Cumulative Trips") + ggtitle("Daily Trips") +
  scale_x_continuous(breaks = c(as.numeric(as.Date(c("04/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("04/15/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("05/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("05/15/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("06/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("06/15/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("07/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("07/15/2015"), format = "%m/%d/%Y"))),
                     labels = c("04/01/2015", "04/15/2015", "05/01/2015", "05/15/2015",
                                "06/01/2015", "06/15/2015", "07/01/2015", "07/15/2015"))
  


# Focus on everything but Freetown
data_admin3.daily.NotFreetown <- data.frame(matrix(rep(NA, 2*(ncol(data_admin3)-3)), ncol=2))
names(data_admin3.daily.NotFreetown) <- c("day", "cum_trips")
data_admin3.daily.NotFreetown$day <- names(data_admin3)[3:(ncol(data_admin3)-1)]

for (row in 1:nrow(data_admin3.daily.NotFreetown)){
  data_admin3.daily.NotFreetown[row, "cum_trips"] <- sum(data_admin3.melt[data_admin3.melt$Chief_From < 4000 & 
                                                                         data_admin3.melt$variable == data_admin3.daily[row, "day"],"value"])
}
data_admin3.daily.NotFreetown$weekday <- factor(c(rep(c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"), times = floor(nrow(data_admin3.daily.NotFreetown)/7)), c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday")),levels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))
tail(data_admin3.daily.NotFreetown)

ggplot(data = data_admin3.daily.NotFreetown) +
  # "three days stay at home" March 27-29 nationally
  annotate("rect", xmin = as.numeric(as.Date(c("03/27/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("03/29/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily.NotFreetown$cum_trips), alpha = .2) +
  # Three saturdays (April 4, 11, 18)
  annotate("rect", xmin = as.numeric(as.Date(c("04/04/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("04/04/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily.NotFreetown$cum_trips), alpha = .2) + 
  annotate("rect", xmin = as.numeric(as.Date(c("04/11/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("04/11/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily.NotFreetown$cum_trips), alpha = .2) + 
  annotate("rect", xmin = as.numeric(as.Date(c("04/18/2015"), format = "%m/%d/%Y")) - 0.5,
           xmax = as.numeric(as.Date(c("04/18/2015"), format = "%m/%d/%Y")) + 0.5,
           ymin = 0, ymax = max(data_admin3.daily.NotFreetown$cum_trips), alpha = .2) + 
  geom_point(aes(x=as.numeric(day), y=cum_trips, col=weekday), size=3) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Day") + ylab("Cumulative Trips") + ggtitle("Daily Trips") +
  scale_x_continuous(breaks = c(as.numeric(as.Date(c("04/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("04/15/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("05/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("05/15/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("06/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("06/15/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("07/01/2015"), format = "%m/%d/%Y")),
                                as.numeric(as.Date(c("07/15/2015"), format = "%m/%d/%Y"))),
                     labels = c("04/01/2015", "04/15/2015", "05/01/2015", "05/15/2015",
                                "06/01/2015", "06/15/2015", "07/01/2015", "07/15/2015"))

#### Map spatial heterogeneity of national movement restriction intervention ####
# Make a sequence of plots with each chiefdom shaded by the number of trips from that chiefdom by day
# This was performed manually by changing 16514 to the other dates of interest and replotting.
# See folder "Daily maps from 03/20 to 04/01"
admin3.sp.fort$trips_from.16526 <- NA
admin3.sp.fort$trips_to.16526 <- NA

for (chief in unique(admin3.sp.fort$CHCODE)){
  admin3.sp.fort[admin3.sp.fort$CHCODE == chief, "trips_from.16526"] <- sum(data_admin3[data_admin3$Chief_From == chief,"16526"]) 
  admin3.sp.fort[admin3.sp.fort$CHCODE == chief, "trips_to.16526"] <- sum(data_admin3[data_admin3$Chief_To == chief,"16526"]) 
}

date <- as.character(as.Date(16526, origin = "1970-01-01"))
ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(trips_from.16526+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[is.element(admin3.sp.fort$CHCODE, towers$CHCODE)==0,], aes(x = long, y = lat, group = group), size=1.2, fill = "lightgrey") +
  coord_equal() +
  theme_bw() + ggtitle(print(date)) +
  scale_fill_continuous(name = "Log10(Number of Trips)", low="white", high="darkblue", limits = c(0,1.2*log10(max(admin3.sp.fort$trips_from.16514))))

# Make a map that compares the % change in travel during the stay at home days. (Admin 3)
admin3.sp.fort$trips_from.16524to16526 <- NA
admin3.sp.fort$trips_to.16524to16526 <- NA

for (chief in unique(admin3.sp.fort$CHCODE)){
  admin3.sp.fort[admin3.sp.fort$CHCODE == chief, "trips_from.16518to16520"] <- sum(data_admin3[data_admin3$Chief_From == chief,"cum_trips_16518to16520"]) 
  admin3.sp.fort[admin3.sp.fort$CHCODE == chief, "trips_to.16518to16520"] <- sum(data_admin3[data_admin3$Chief_To == chief,"cum_trips_16518to16520"]) 
}

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = (100*(trips_from.16521to16523 - trips_from.16518to16520) / trips_from.16518to16520), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[is.element(admin3.sp.fort$CHCODE, towers$CHCODE)==0,], aes(x = long, y = lat, group = group), size=1.2, fill = "lightgrey") +
  coord_equal() +
  theme_bw() + ggtitle("Decrease in Mobility during 3 National 'Stay At Home' days") +
  scale_fill_continuous(name = "Percent Change from\nPrevious 3 Days", low="lightgreen", high="black") +
  guides(fill = guide_legend(reverse=TRUE))

# Make a map that compares the % change in travel during the stay at home days. (Admin 2)
admin2.sp.fort$trips_from.165224to16526 <- NA
admin2.sp.fort$trips_to.16524to16526 <- NA

for (dis in unique(admin2.sp.fort$DISCODE)){
  admin2.sp.fort[admin2.sp.fort$DISCODE == dis, "trips_from.16524to16526"] <- sum(data_admin2[data_admin2$Dis_From == dis,"cum_trips_16524to16526"]) 
  admin2.sp.fort[admin2.sp.fort$DISCODE == dis, "trips_to.16524to16526"] <- sum(data_admin2[data_admin2$Dis_To == dis,"cum_trips_16524to16526"]) 
}

ggplot() +
  geom_polygon(data = admin2.sp.fort, aes(x = long, y = lat, fill = (100*(trips_from.16521to16523 - trips_from.16518to16520) / trips_from.16518to16520), group = group), colour = "darkgrey") +
  geom_polygon(data = admin2.sp.fort[is.element(admin2.sp.fort$CHCODE, towers$DISCODE)==0,], aes(x = long, y = lat, group = group), size=1.2, fill = "lightgrey") +
  coord_equal() +
  theme_bw() + ggtitle("Decrease in Mobility Between Districts during 3 National 'Stay At Home' days") +
  scale_fill_continuous(name = "Percent Change from\nPrevious 3 Days", low="lightgreen", high="black") +
  guides(fill = guide_legend(reverse=TRUE))

# See if travel FROM districts with ongoing transmission had more effect of intervention
# http://maps.who.int/MapJournal/?appid=cc1ec3ef5c3944079c5e7935a45b71d4&webmap=bb9e11adb0ac447f919281f67ce9aa59
Transmission.Dis <- c( "21", "22", "24", "33", "41", "42")

a <- sum(data_admin2[is.element(data_admin2$Dis_From,Transmission.Dis)==1,"cum_trips_16521to16523" ])
b <- sum(data_admin2[is.element(data_admin2$Dis_From,Transmission.Dis)==0,"cum_trips_16521to16523" ])
c <- sum(data_admin2[is.element(data_admin2$Dis_From,Transmission.Dis)==1,"cum_trips_16518to16520" ])
d <- sum(data_admin2[is.element(data_admin2$Dis_From,Transmission.Dis)==0,"cum_trips_16518to16520" ])
  
cat("Pecent change in inter-district travel from districts with ongoing transmission: ", (a-c)/c * 100)
cat("Pecent change in inter-district travel from districts withOUT ongoing transmission: ", (b-d)/d * 100)

chisq.mat <- matrix(c(a,b,c,d), ncol=2, byrow = TRUE)
chisq.mat
chisq.test(chisq.mat)
cat("The percent reduction in inter-district travel from districts with ongoing transmission\nwas significantly greater than from districts withOUT ongoing transmission ( p = ", chisq.test(chisq.mat)$p.value,")")

# See if travel TO districts with ongoing transmission had more effect of intervention
Transmission.Dis <- c( "21", "22", "24", "33", "41", "42")

a <- sum(data_admin2[is.element(data_admin2$Dis_To,Transmission.Dis)==1,"cum_trips_16521to16523" ])
b <- sum(data_admin2[is.element(data_admin2$Dis_To,Transmission.Dis)==0,"cum_trips_16521to16523" ])
c <- sum(data_admin2[is.element(data_admin2$Dis_To,Transmission.Dis)==1,"cum_trips_16518to16520" ])
d <- sum(data_admin2[is.element(data_admin2$Dis_To,Transmission.Dis)==0,"cum_trips_16518to16520" ])

cat("Pecent change in inter-district travel to districts with ongoing transmission: ", (a-c)/c * 100)
cat("Pecent change in inter-district travel to districts withOUT ongoing transmission: ", (b-d)/d * 100)

chisq.mat <- matrix(c(a,b,c,d), ncol=2, byrow = TRUE)
chisq.mat
chisq.test(chisq.mat)
cat("The percent reduction in inter-district travel to districts with ongoing transmission\nwas significantly less than from districts withOUT ongoing transmission ( p = ", chisq.test(chisq.mat)$p.value,")")

# See if travel TO or FROM districts with ongoing transmission had more effect of intervention
Transmission.Dis <- c( "21", "22", "24", "33", "41", "42")

a <- sum(data_admin2[is.element(data_admin2$Dis_To,Transmission.Dis)==1 | is.element(data_admin2$Dis_From,Transmission.Dis)==1,"cum_trips_16521to16523" ])
b <- sum(data_admin2[is.element(data_admin2$Dis_To,Transmission.Dis)==0 & is.element(data_admin2$Dis_From,Transmission.Dis)==0,"cum_trips_16521to16523" ])
c <- sum(data_admin2[is.element(data_admin2$Dis_To,Transmission.Dis)==1 | is.element(data_admin2$Dis_From,Transmission.Dis)==1,"cum_trips_16518to16520" ])
d <- sum(data_admin2[is.element(data_admin2$Dis_To,Transmission.Dis)==0 & is.element(data_admin2$Dis_From,Transmission.Dis)==0,"cum_trips_16518to16520" ])

cat("Pecent change in inter-district travel to or from districts with ongoing transmission: ", (a-c)/c * 100)
cat("Pecent change in inter-district travel to or from districts withOUT ongoing transmission: ", (b-d)/d * 100)

chisq.mat <- matrix(c(a,b,c,d), ncol=2, byrow = TRUE)
chisq.mat
chisq.test(chisq.mat)
cat("The percent reduction in inter-district travel to or from districts with ongoing transmission\nwas not significantly different than from districts withOUT ongoing transmission ( p = ", chisq.test(chisq.mat)$p.value,")")

# Make a bar chart with travel before, during, and after intervention
data_admin2.melt <- melt(data_admin2, id = c("Dis_From","Dis_To"))
ggplot(data_admin2.melt[is.element(data_admin2.melt$variable, c("cum_trips_16518to16520","cum_trips_16521to16523", "cum_trips_16524to16526")),], aes(factor(Dis_From), value)) +
  geom_bar(aes(fill=variable), position = "dodge", stat = "identity") +
  scale_fill_discrete(name = "Time Period", labels=c("Before Intervetion","During Intervention","After Intervention")) +
  xlab("Source District") + ylab("Number of Trips")

ggplot(data_admin2.melt[is.element(data_admin2.melt$variable, c("cum_trips_16518to16520","cum_trips_16521to16523", "cum_trips_16524to16526")),], aes(factor(Dis_To), value)) +
  geom_bar(aes(fill=variable), position = "dodge", stat = "identity") +
  scale_fill_discrete(name = "Time Period", labels=c("Before Intervetion","During Intervention","After Intervention")) +
  xlab("Destination District") + ylab("Number of Trips")

# Plot the change in travel as a function of travel distance
data_admin2$Hours <- NA
for (i in 1:nrow(data_admin2)){
  data_admin2[i, "Hours"] <- travel.times[travel.times$Dis_From == data_admin2[i, "Dis_From"] &
                                            travel.times$Dis_To == data_admin2[i, "Dis_To"], "Hours"]
}
ggplot(data_admin2, aes(x=Hours, y=cum_trips)) + 
  geom_boxplot(aes(group = factor(Hours)))

ggplot(data_admin2[data_admin2$cum_trips_16518to16520 > 5,], aes(x=Hours, y= 100*(cum_trips_16521to16523 - cum_trips_16518to16520)/(cum_trips_16518to16520))) + 
  geom_boxplot(aes(group = factor(Hours))) +
  theme_bw() + ylab("Percent change in inter-district trips") + xlab("Travel time between Districts (Hours)") +
  ggtitle("Effect of national stay at home days by distance traveled")

# Make a series of daily maps that show the weight of each connection
# Shade connections weighted by log of count
data_admin2 <- data_admin2[order(data_admin2$cum_trips),] #sort so the heavier connections are drawn later

start <- 1
range <- 1.2*max(log(data_admin2[,"16519"]))
colors <- colorRampPalette(brewer.pal(9,"Blues"))(100)

# dev.off()
# detach("package:plotly", unload=TRUE)
layout(t(c(1,2,3)))
for (days in c("16520","16521","16524")){
  plot(admin2.sp)
  title(main = as.character(as.Date(as.numeric(days), origin = "1970-01-01")))
  for (row in start:nrow(data_admin2)){
    
    from <- which(admin2.sp$DISCODE==data_admin2[row, "Dis_From"])
    to <- which(admin2.sp$DISCODE==data_admin2[row, "Dis_To"])
    
    lon_1 <- admin2.sp$centroids.Long[from]
    lat_1 <- admin2.sp$centroids.Lat[from]
    lon_2 <- admin2.sp$centroids.Long[to]
    lat_2 <- admin2.sp$centroids.Lat[to]
    inter <- gcIntermediate(c(lon_1, lat_1), c(lon_2, lat_2), n=50, addStartEnd=TRUE)
    
    weight <- ceiling(log(data_admin2[row,as.character(days)]) / range * 100) / 100
    
    lines(inter, col = colors[weight*100], lwd = 3*weight^2)
  }
}

#### Calculate distance between towers ####
# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}
earth.dist(towers[1,"Long"], towers[1,"Lat"], towers[20,"Long"], towers[20, "Lat"])

towers_dist <- data.frame(matrix(rep(NA, length(unique(towers$CellID))^2), nrow=length(unique(towers$CellID))))
row.names(towers_dist) <- unique(towers$CellID)
names(towers_dist) <- unique(towers$CellID)

for (i in 1:nrow(towers_dist)){
  for (j in 1:ncol(towers_dist)){
    long1 <- towers[towers$CellID == row.names(towers_dist)[i], "Long"][1]
    lat1  <- towers[towers$CellID == row.names(towers_dist)[i], "Lat"][1]
    long2 <- towers[towers$CellID == row.names(towers_dist)[j], "Long"][1]
    lat2  <- towers[towers$CellID == row.names(towers_dist)[j], "Lat"][1]
    towers_dist[i,j] <- earth.dist(long1, lat1, long2, lat2)
  }
}
write.csv(towers_dist, file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/towers_dist.csv", )
towers_dist <- read.csv("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/towers_dist.csv")

#### Make a table with groupings based on distance ####
towers_dist <- read.csv("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/towers_dist.csv")
towers_dist[1:5, 1:5]

groups <- data.frame(towers_dist[,1])
names(groups) <- c("Tower_ID")

groups$km5 <- NA # Make a group where islands are sets of towers within 5km of all others
group <- 1
for (i in 1:nrow(groups)){  
  neighbors <- c()
  for (j in 2:nrow(groups)){
    if (towers_dist[i, j] <= 5){
      if (is.element(towers_dist[j,1], neighbors) == 0){
        neighbors <- c(neighbors, towers_dist[j,1])
      }
    }
  }
  if (sum(is.na(groups[is.element(groups$Tower_ID, neighbors), "km5"])==0) > 0 ){
    groups[i, "km5"] <- groups[is.element(groups$Tower_ID, neighbors), "km5"][1]
  } else {
    groups[i, "km5"] <- group
    group <- group + 1
  }
}
plot(groups$km5)

groups$km10 <- NA # Make a group where islands are sets of towers within 10km of all others
group <- 1
for (i in 1:nrow(groups)){  
  neighbors <- c()
  for (j in 2:nrow(groups)){
    if (towers_dist[i, j] <= 10){
      if (is.element(towers_dist[j,1], neighbors) == 0){
        neighbors <- c(neighbors, towers_dist[j,1])
      }
    }
  }
  if (sum(is.na(groups[is.element(groups$Tower_ID, neighbors), "km10"])==0) > 0 ){
    groups[i, "km10"] <- groups[is.element(groups$Tower_ID, neighbors), "km10"][1]
  } else {
    groups[i, "km10"] <- group
    group <- group + 1
  }
}
plot(groups$km10)
  
groups$km20 <- NA # Make a group where islands are sets of towers within 10km of all others
group <- 1
for (i in 1:nrow(groups)){  
  neighbors <- c()
  for (j in 2:nrow(groups)){
    if (towers_dist[i, j] <= 20){
      if (is.element(towers_dist[j,1], neighbors) == 0){
        neighbors <- c(neighbors, towers_dist[j,1])
      }
    }
  }
  if (sum(is.na(groups[is.element(groups$Tower_ID, neighbors), "km20"])==0) > 0 ){
    groups[i, "km20"] <- groups[is.element(groups$Tower_ID, neighbors), "km20"][1]
  } else {
    groups[i, "km20"] <- group
    group <- group + 1
  }
}
plot(groups$km20)
hist(groups$km20, breaks = c(1:max(groups$km20)))

groups$km5_conservative <- NA # Make a group where islands are made such that each tower is within 5km of another tower in the gorup
  
  add <- 0
  which_within_5km <- c()
  iteration = 1
  add <- towers_dist[which(towers_dist[i,2:ncol(towers_dist)] <= 5), 1]
  
  if (length(add) > 0){
    which_within_5km <- c(which_within_5km, add)
    add <- towers_dist[which(towers_dist[is.element(towers_dist$X, which_within_5km), 2:ncol(towers_dist)] <= 5), 1]
    add <- add[is.element(add, which_within_5km)==0]
  }
  
  within_5km <- groups$Tower_ID[]
  
}