#### Load or Save Workspace ####
# save.image("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/process_data.RData")
# load("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/process_data.RData")

#### Libraries ####
library(ggplot2)
library(spatial)
library(rgeos)
library(gpclib)
library(sp)
library(rgdal)
library(maptools)
require(chron)
library(spdep)

#### Tasks ####
# Check if there are towers in the chiefdoms that don't get any travel. Especially near Tonkolili and Kambia
  # What proportion of chiefdoms have towers in them?
  # Why are towers near Bo and other places not counted inside that chiefdom? Problem is that the towns (1291,1391,2191,3191)
# Look at temporal trends for the whole country
# Create map of main transportation movements
# Look at number of trips during quarantine days compared to non
  # Before and after for Tonkolili (July 24)
  # Three saturdays (April 4, 11, 18) nationally
  # "three days stay at home" March 27-29 nationally
  # before and after schools reopen (April 14)

#### Read Data ####
# Create a dataset for the Admin 2 Trips
setwd("/Users/peakcm/Documents/SLE_Mobility/sle_ericson")
data_admin2 <- read.csv(file = "IndivMvtBtwnAdmin2DaySep1Agg.csv")

# Create a dataset for the Admin 3 Trips
setwd("/Users/peakcm/Documents/SLE_Mobility/sle_ericson")
data_admin3 <- read.csv(file = "IndivMvtBtwnAdmin3DaySep1Agg.csv")

date.start <- as.numeric(as.Date(c("03/20/2015"), format = "%m/%d/%Y"))
dates <- seq(from = date.start, to = date.start + ncol(data_admin3) - 3)
names(data_admin3) <- c("Chief_From", "Chief_To",  dates)
data_admin3$cum_trips <- apply(data_admin3[,3:ncol(data_admin3)], 1, sum)

# Add city CHCODES to the encompassing polygon
data_admin3[data_admin3$Chief_From == 1291, "Chief_From"] <- 1212
data_admin3[data_admin3$Chief_To == 1291, "Chief_To"] <- 1212

data_admin3[data_admin3$Chief_From == 1391, "Chief_From"] <- 1313
data_admin3[data_admin3$Chief_To == 1391, "Chief_To"] <- 1313

data_admin3[data_admin3$Chief_To == 2191, "Chief_To"] <- 2102
data_admin3[data_admin3$Chief_From == 2191, "Chief_From"] <- 2102

data_admin3[data_admin3$Chief_From == 3191, "Chief_From"] <- 3108
data_admin3[data_admin3$Chief_To == 3191, "Chief_To"] <- 3108

# Combine some Freetown towers into the encompassing polygon
data_admin3[data_admin3$Chief_From == 4201, "Chief_From"] <- 4208
data_admin3[data_admin3$Chief_To == 4201, "Chief_To"] <- 4208

data_admin3[data_admin3$Chief_From == 4202, "Chief_From"] <- 4208
data_admin3[data_admin3$Chief_To == 4202, "Chief_To"] <- 4208

data_admin3[data_admin3$Chief_From == 4206, "Chief_From"] <- 4208
data_admin3[data_admin3$Chief_To == 4206, "Chief_To"] <- 4208

data_admin3[data_admin3$Chief_From == 4207, "Chief_From"] <- 4208
data_admin3[data_admin3$Chief_To == 4207, "Chief_To"] <- 4208

# Create a dataset for the tower locations provided to us by Ericsson
setwd("/Users/peakcm/Documents/SLE_Mobility/data")
towers <- read.csv(file = "Tower_Locations.csv")
towers.fort <- fortify(towers, region = admin3)

# Create a dataset for the tower locations with CHCODE assignments
work.dir <- "/Users/peakcm/Documents/SLE_Mobility/data"
towers.admin3 <- readOGR(work.dir, layer = 'Towers_Admin_1_2_3')
towers.admin3.df <- data.frame(towers.admin3)

# Create a shapefile for Sierra Leone Admin 3
work.dir <- "/Users/peakcm/Documents/SLE_Mobility/Arc GIS/Open Humanitarian Data Repository/Sierra Leone Chiefdoms SLGov Admin_3 2012"
admin3.sp <- readOGR(work.dir, layer = 'Sierra_Leone_Chiefdoms_SLGov_Admin_3_2012')
admin3.sp <- admin3.sp[admin3.sp$CHCODE > 0,]
admin3.sp.fort <- fortify(admin3.sp, region = "CHCODE")
admin3.sp.fort$CHCODE <- as.numeric(admin3.sp.fort$id)

#### Tonkolili Plots ####
# Focus on Kholifa Rowala Chiefdom (2505) in Tonkolili District

hist(log(data_admin3[data_admin3$Chief_From == 2505, "cum_trips"]), xlab = "log(cumulative trips)", main = "Trips from Kholifa Rowala, Tonkolili")
data_admin3 <- data_admin3[order(-data_admin3$cum_trips),]
plot(data_admin3[data_admin3$Chief_From == 2505, "cum_trips"], main = "Trips from Tonkolili")

#Split data into before and after case detection
date.tonk <- as.numeric(as.Date(c("07/24/2015"), format = "%m/%d/%Y"))
# plot(data_admin3[data_admin3$Chief_From == 2505, "cum_trips"], main = "Trips from Tonkolili")

# Pie chart (Chiefdoms)
layout(t(c(1,2)))
lbls <- paste(data_admin3[data_admin3$Chief_From == 2505,"Chief_To"], "\n", round(data_admin3[data_admin3$Chief_From == 2505,"cum_trips"]/sum(data_admin3[data_admin3$Chief_From == 2505,"cum_trips"])*100, 0),"%", sep="")
pie(x = data_admin3[data_admin3$Chief_From == 2505,"cum_trips"], labels = lbls, main="Chiefdom Destinations from\nKholifa Rowala, Tonkolili (2505)", )

lbls <- paste(data_admin3[data_admin3$Chief_To == 2505,"Chief_From"], "\n", round(data_admin3[data_admin3$Chief_To == 2505,"cum_trips"]/sum(data_admin3[data_admin3$Chief_To == 2505,"cum_trips"])*100, 0),"%", sep="")
pie(x = data_admin3[data_admin3$Chief_To == 2505,"cum_trips"], labels = lbls, main="Chiefdom Departures to\nKholifa Rowala, Tonkolili (2505)", )

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

# layout(t(c(1,2)))
# lbls <- paste(data_Tonk_districts$district, "\n", round(data_Tonk_districts$from/sum(data_Tonk_districts$from)*100, 0),"%", sep="")
# pie(x = data_Tonk_districts$from, labels = lbls, main="District Destinations\n from Kholifa Rowala, Tonkolili (2505)")
# 
# lbls <- paste(data_Tonk_districts$district, "\n", round(data_Tonk_districts$to/sum(data_Tonk_districts$to)*100, 0),"%", sep="")
# pie(x = data_Tonk_districts$to, labels = lbls, main="District Destinations\n to Kholifa Rowala, Tonkolili (2505)")

ggplot(data=data_Tonk_districts) +
  geom_bar(aes(x=factor(district), y=to, fill = factor(region)), stat="identity") +
  xlab("Destination District Number") +
  ylab("Number of trips") +
  ggtitle("Trips to Kholifa Rowala, Tonkolili") +
  scale_fill_discrete(name = "Region", labels=c("East","North","South", "West")) +
  theme_bw()

ggplot(data=data_Tonk_districts) +
  geom_bar(aes(x=factor(district), y=from, fill = factor(region)), stat="identity") +
  xlab("Source District Number") +
  ylab("Number of trips") +
  ggtitle("Trips from Kholifa Rowala, Tonkolili") +
  scale_fill_discrete(name = "Region",labels=c("East","North","South", "West")) +
  theme_bw()

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
  geom_point(data=towers.admin3.df, aes(x=Long, y=Lat ), color="black", size=1)

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = from_tonk/sum(tonk$from_tonk), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white") +
  geom_point(data=towers.admin3.df, aes(x=Long, y=Lat ), color="black", size=1)

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(to_tonk+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white") +
  geom_point(data=towers.admin3.df, aes(x=Long, y=Lat ), color="black", size=1)

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = to_tonk/sum(tonk$to_tonk), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white") +
  geom_point(data=towers.admin3.df, aes(x=Long, y=Lat ), color="black", size=1)

#### Kambia Plots ####
# Focus on Tonko Limba Chiefdom (2207) in Kambia District
# http://www.reuters.com/article/2015/09/02/us-health-ebola-leone-idUSKCN0R22CV20150902
layout(c(1))
hist(log(data_admin3[data_admin3$Chief_From == 2207, "cum_trips"]), xlab = "log(cumulative trips)", main = "Trips from Tonko Limba, Kambia")
data_admin3 <- data_admin3[order(-data_admin3$cum_trips),]

plot(data_admin3[data_admin3$Chief_From == 2207, "cum_trips"], 
     main = "Trips from Tonko Limba, Kambia", xlab = "Chiefdom Rank", ylab = "Cumulative Trips")

# Pie chart (Chiefdoms)
layout(t(c(1,2)))
lbls <- paste(data_admin3[data_admin3$Chief_From == 2207,"Chief_To"], "\n", round(data_admin3[data_admin3$Chief_From == 2207,"cum_trips"]/sum(data_admin3[data_admin3$Chief_From == 2207,"cum_trips"])*100, 0),"%", sep="")
pie(x = data_admin3[data_admin3$Chief_From == 2207,"cum_trips"], labels = lbls, main="Chiefdom Destinations\n from Tonko Limba, Kambia (2207)", )

lbls <- paste(data_admin3[data_admin3$Chief_To == 2207,"Chief_From"], "\n", round(data_admin3[data_admin3$Chief_To == 2207,"cum_trips"]/sum(data_admin3[data_admin3$Chief_To == 2207,"cum_trips"])*100, 0),"%", sep="")
pie(x = data_admin3[data_admin3$Chief_To == 2207,"cum_trips"], labels = lbls, main="Chiefdom Departures\n to Tonko Limba, Kambia (2207)", )

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

# layout(t(c(1,2)))
# lbls <- paste(data_kambia_districts$district, "\n", round(data_kambia_districts$from/sum(data_kambia_districts$from)*100, 0),"%", sep="")
# pie(x = data_kambia_districts$from, labels = lbls, main="District Destinations\n from Tonko Limba, Kambia (2207)")
# 
# lbls <- paste(data_kambia_districts$district, "\n", round(data_kambia_districts$to/sum(data_kambia_districts$to)*100, 0),"%", sep="")
# pie(x = data_kambia_districts$to, labels = lbls, main="District Destinations\n to Tonko Limba, Kambia (2207)")

ggplot(data=data_kambia_districts) +
  geom_bar(aes(x=factor(district), y=to, fill = factor(region)), stat="identity") +
  xlab("Destination District Number") +
  ylab("Number of trips") +
  ggtitle("Trips to Tonko Limba, Kambia") +
  scale_fill_discrete(name = "Region", labels=c("East","North","South", "West")) +
  theme_bw()

ggplot(data=data_kambia_districts) +
  geom_bar(aes(x=factor(district), y=from, fill = factor(region)), stat="identity") +
  xlab("Source District Number") +
  ylab("Number of trips") +
  ggtitle("Trips from Tonko Limba, Kambia") +
  scale_fill_discrete(name = "Region",labels=c("East","North","South", "West")) +
  theme_bw()

#### Spatial for Kambia ####
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
  geom_point(data=towers.admin3.df, aes(x=Long, y=Lat ), color="black", size=1)

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = from_kambia/sum(kambia$from_kambia), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white") +
  geom_point(data=towers.admin3.df, aes(x=Long, y=Lat ), color="black", size=1)

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(to_kambia+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white")+
  geom_point(data=towers.admin3.df, aes(x=Long, y=Lat ), color="black", size=1)

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = to_kambia/sum(kambia$to_kambia), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white") +
  geom_point(data=towers.admin3.df, aes(x=Long, y=Lat ), color="black", size=1)

#### Find Chiefdoms without towers ####
length(unique(data_admin3$Chief_From))
length(unique(towers$admin3))

ggplot() +
  geom_polygon(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(from_tonk+1), group = group), colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[is.element(admin3.sp.fort$CHCODE, towers$admin3)==0,], aes(x = long, y = lat, group = group), size=1.2, fill = "lightgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow", fill = "white") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white") +
  geom_point(data=towers.admin3.df, aes(x=Long, y=Lat ), color="black", size=1)

#### Country-wide maps ####
# Create a map of all the tower locations


#### Country-wide temporal series ####
