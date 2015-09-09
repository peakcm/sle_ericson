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

#### Read Data ####
setwd("/Users/peakcm/Documents/SLE_Mobility/sle_ericson")
data_admin3 <- read.csv(file = "IndivMvtBtwnAdmin3DaySep1Agg.csv")
data_admin2 <- read.csv(file = "IndivMvtBtwnAdmin2DaySep1Agg.csv")

date.start <- as.numeric(as.Date(c("03/20/2015"), format = "%m/%d/%Y"))
dates <- seq(from = date.start, to = date.start + ncol(data_admin3) - 3)

names(data_admin3) <- c("Chief_From", "Chief_To",  dates)
# data_admin3$Chief_To <- as.numeric(data_admin3$Chief_To)

data_admin3$cum_trips <- apply(data_admin3[,3:ncol(data_admin3)], 1, sum)

#### Country-wide maps ####

#### Country-wide temporal series ####


#### Tonkolili Plots ####
# Focus on Kholifa Rowala Chiefdom (2505) in Tonkolili District

hist(log(data_admin3[data_admin3$Chief_From == 2505, "cum_trips"]), xlab = "log(cumulative trips)", main = "Trips from Kholifa Rowala, Tonkolili")
data_admin3 <- data_admin3[order(-data_admin3$cum_trips),]
plot(data_admin3[data_admin3$Chief_From == 2505, "cum_trips"], main = "Trips from Tonkolili")

#Split data into before and after case detection
date.tonk <- as.numeric(as.Date(c("07/24/2015"), format = "%m/%d/%Y"))
plot(data_admin3[data_admin3$Chief_From == 2505, "cum_trips"], main = "Trips from Tonkolili")

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
layout(c(1))
work.dir <- "/Users/peakcm/Documents/2014 Cholera OCV/Data - Raw/GIS Shapefiles/Shapefiles/Admin 3"

admin3.sp <- readOGR(work.dir, layer = 'SLE_Adm3_1m_gov')
admin3.sp <- admin3.sp[admin3.sp$CHCODE > 0,]
admin3.sp.fort <- fortify(admin3.sp, region = "CHCODE")
admin3.sp.fort$CHCODE <- as.numeric(admin3.sp.fort$id)

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

ggplot(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(from_tonk+1), group = group)) +
  geom_polygon(colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white")

ggplot(data = admin3.sp.fort, aes(x = long, y = lat, fill = from_tonk/sum(tonk$from_tonk), group = group)) +
  geom_polygon(colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white")

ggplot(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(to_tonk+1), group = group)) +
  geom_polygon(colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white")

ggplot(data = admin3.sp.fort, aes(x = long, y = lat, fill = to_tonk/sum(tonk$to_tonk), group = group)) +
  geom_polygon(colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2505,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Kholifa Rowala, Tonkolili") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white")

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
layout(c(1))
work.dir <- "/Users/peakcm/Documents/2014 Cholera OCV/Data - Raw/GIS Shapefiles/Shapefiles/Admin 3"

admin3.sp <- readOGR(work.dir, layer = 'SLE_Adm3_1m_gov')
admin3.sp <- admin3.sp[admin3.sp$CHCODE > 0,]
admin3.sp.fort <- fortify(admin3.sp, region = "CHCODE")
admin3.sp.fort$CHCODE <- as.numeric(admin3.sp.fort$id)

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

ggplot(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(from_kambia+1), group = group)) +
  geom_polygon(colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white")

ggplot(data = admin3.sp.fort, aes(x = long, y = lat, fill = from_kambia/sum(kambia$from_kambia), group = group)) +
  geom_polygon(colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow") +
  coord_equal() +
  theme_bw() + ggtitle("Destinations from Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white")

ggplot(data = admin3.sp.fort, aes(x = long, y = lat, fill = log10(to_kambia+1), group = group)) +
  geom_polygon(colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Log10(Number of Trips)", low = "white")

ggplot(data = admin3.sp.fort, aes(x = long, y = lat, fill = to_kambia/sum(kambia$to_kambia), group = group)) +
  geom_polygon(colour = "darkgrey") +
  geom_polygon(data = admin3.sp.fort[admin3.sp.fort$CHCODE == 2207,], aes(x = long, y = lat, group = group), size=1.2, colour = "yellow") +
  coord_equal() +
  theme_bw() + ggtitle("Sources to Tonko Limba, Kambia") +
  scale_fill_continuous(name = "Proportion of Trips", low = "white")

