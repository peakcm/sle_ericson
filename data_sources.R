#### Header ####
# Code to look at each district epi curve with additional markers for CDR data and interventions at national and district levels

#### Load Libraries ####
library(reshape2)
library(ggplot2)
library(magrittr)

#### Read WHO Data ####
# Import Ebola district data from WHO
# http://apps.who.int/gho/data/view.ebola-sitrep.ebola-country-SLE-new-conf-prob-districs-20151230-data?lang=en
setwd("/Users/peakcm/Documents/SLE_Mobility/District Epi Curves")
data_ebola <- read.csv(file = "confirmed_cases_week_district.csv")

View(data_ebola)
names(data_ebola)[1:4] <- c("District", "Source", "New", "Confirmation")
origin = "2013-12-30"
names(data_ebola)[5:length(names(data_ebola))] <- as.Date(seq(from = 0, to = (length(names(data_ebola))-5)*7, by = 7), origin = origin)

for (row in 1:nrow(data_ebola)){
  if (data_ebola[row, "District"] != ""){
    d <- data_ebola[row, "District"]
  } else {
    data_ebola[row, "District"] <- d
  }
  if (data_ebola[row, "Source"] != ""){
    s <- data_ebola[row, "Source"]
  } else {
    data_ebola[row, "Source"] <- s
  }
}
data_ebola <- data_ebola[,-3]
View(data_ebola)

data_ebola.m <- melt(data_ebola, id.vars = c("District", "Source", "Confirmation"))
data_ebola.m$variable <- as.Date(as.numeric(as.character(data_ebola.m$variable)) - as.numeric(as.character(data_ebola.m$variable[1])), origin = "2013-12-30")
View(data_ebola.m)

# Add data on movement restrictions
data_interventions <- data.frame(matrix(nrow = 1, ncol = 5))
names(data_interventions) <- c("Range", "District", "Type", "Date_Start", "Date_End")
data_interventions[1,] <- c("National", "All", "National_Lock_Down", "2014-09-19", "2014-09-21")
data_interventions[2,] <- c("National", "All", "National_Lock_Down", "2015-03-27", "2015-03-29")
data_interventions[3,] <- c("National", "All", "National_Lock_Down", "2015-04-04", "2015-04-05")
data_interventions[4,] <- c("National", "All", "National_Lock_Down", "2015-04-11", "2015-04-12")
data_interventions[5,] <- c("National", "All", "National_Lock_Down", "2015-04-18", "2015-04-19")

data_interventions[6,] <- c("District", "KENEMA", "Quarantine", "2014-08-07", "2015-01-23")
data_interventions[7,] <- c("District", "KAILAHUN", "Quarantine", "2014-08-07", "2015-01-23")

data_interventions[8,] <- c("District", "PORT LOKO", "Quarantine", "2014-09-25", "2015-01-23")
data_interventions[9,] <- c("District", "BOMBALI", "Quarantine", "2014-09-25", "2015-01-23")
data_interventions[10,] <- c("District", "MOYAMBA", "Quarantine", "2014-09-25", "2015-01-23")

data_interventions[11,] <- c("District", "PORT LOKO", "Quarantine", "2015-06-25", "2015-08-17") #Unsure of end date
data_interventions[12,] <- c("District", "KAMBIA", "Quarantine", "2015-06-25", "2015-08-17") #Unsure of end date
data_interventions[13,] <- c("District", "KONO", "Quarantine", "2014-12-10", "2014-10-23")
data_interventions[14,] <- c("District", "TONKOLILI", "Quarantine", "2014-12-03", "2015-01-18")

data_interventions[15,] <- c("District", "KOINADUGU", "Quarantine", "2014-08-04", "2014-11-12")


rows <- nrow(data_interventions)
for (row in 1:rows){
  if (data_interventions[row,"Range"] == "National"){
    for (d in unique(data_ebola.m$District)){
      data_interventions <- rbind(data_interventions, c("National", as.character(d), data_interventions[row,"Type"], data_interventions[row, "Date_Start"], data_interventions[row, "Date_End"]))
    }
  }
}

data_interventions$Date_Start <- as.Date(data_interventions$Date_Start, origin = origin)
data_interventions$Date_End <- as.Date(data_interventions$Date_End, origin = origin)

data_interventions <- data_interventions[data_interventions$District != "All",]

# Add indicator for CDR data range
data_CDR <- data.frame(matrix(nrow = 1, ncol = 3))
names(data_CDR) <- c("Set", "Date_Start", "Date_End")

data_CDR[1,] <- c(1, "2014-06-01", "2014-06-01")
data_CDR <- rbind(data_CDR, c(1, "2014-06-03", "2014-06-05"))
data_CDR <- rbind(data_CDR, c(1, "2014-06-13", "2014-06-16"))
data_CDR <- rbind(data_CDR, c(1, "2014-06-19", "2014-06-25"))
data_CDR <- rbind(data_CDR, c(1, "2014-07-01", "2014-07-31"))
data_CDR <- rbind(data_CDR, c(1, "2014-08-14", "2014-10-25"))
data_CDR <- rbind(data_CDR, c(1, "2014-10-27", "2014-10-28"))
data_CDR <- rbind(data_CDR, c(1, "2014-10-29", "2014-12-31"))
data_CDR <- rbind(data_CDR, c(1, "2015-01-02", "2015-01-16"))
data_CDR <- rbind(data_CDR, c(2, "2015-03-02", "2015-06-30"))

data_CDR$Date_Start <- as.Date(data_CDR$Date_Start, origin = origin)
data_CDR$Date_End <- as.Date(data_CDR$Date_End, origin = origin)

#### Plot ####
ggplot(data_ebola.m[data_ebola.m$Confirmation == "Confirmed" &
                      data_ebola.m$Source == "Patient database" &
                      is.na(data_ebola.m$value)==0,],
       aes(x = as.numeric(variable), y = value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw() +
  geom_segment(data = data_interventions[data_interventions$Range == "National",], aes(x = as.numeric(Date_Start), xend = as.numeric(Date_End), y = 200, yend = 200), color = "red", size = 2, alpha = 0.5) +
  geom_segment(data = data_interventions[data_interventions$Range == "District",], aes(x = as.numeric(Date_Start), xend = as.numeric(Date_End), y = 180, yend = 180), color = "blue", size = 2, alpha = 0.5) +
  geom_segment(data = data_CDR, aes(x = as.numeric(Date_Start), xend = as.numeric(Date_End), y = 220, yend = 220), color = "forestgreen", size = 2, alpha = 0.5) +
  ylab("Weekly Confirmed Cases") +
  xlab("Date") +
  scale_x_continuous(breaks = as.numeric(as.Date(c("2014-06-01", "2014-08-01", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01", "2015-06-01", "2015-08-01"), origin = origin)), labels = as.character(as.Date(c("2014-06-01", "2014-08-01", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01", "2015-06-01", "2015-08-01"), origin = origin))) +
  facet_grid(District~.)

focus_district <- "PORT LOKO"
ggplot(data_ebola.m[data_ebola.m$Confirmation == "Confirmed" &
                      data_ebola.m$Source == "Patient database" &
                      is.na(data_ebola.m$value)==0 &
                      data_ebola.m$District == focus_district,],
       aes(x = as.numeric(variable), y = value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw() +
  geom_segment(data = data_interventions[data_interventions$Range == "National",], aes(x = as.numeric(Date_Start), xend = as.numeric(Date_End), y = 160, yend = 160), color = "red", size = 4, alpha = 0.5) +
  geom_segment(data = data_interventions[data_interventions$Range == "District" & data_interventions$District == focus_district,], aes(x = as.numeric(Date_Start), xend = as.numeric(Date_End), y = 140, yend = 140), color = "blue", size = 4, alpha = 0.5) +
  geom_segment(data = data_CDR, aes(x = as.numeric(Date_Start), xend = as.numeric(Date_End), y = 180, yend = 180), color = "forestgreen", size = 4, alpha = 0.5) +
  ylab("Weekly Confirmed Cases") +
  xlab("Date") +
  ggtitle(focus_district) +
  scale_x_continuous(breaks = as.numeric(as.Date(c("2014-06-01", "2014-08-01", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01", "2015-06-01", "2015-08-01"), origin = origin)), labels = as.character(as.Date(c("2014-06-01", "2014-08-01", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01", "2015-06-01", "2015-08-01"), origin = origin))) +
  annotate("text", x = as.numeric(as.Date("2015-7-15", origin = origin)), y = 185, label = "Mobile Phone Data", color = "forestgreen", alpha = 0.8) +
  annotate("text", x = as.numeric(as.Date("2015-7-15", origin = origin)), y = 165, label = "National Stay-at-Home Days", color = "red", alpha = 0.8) +
  annotate("text", x = as.numeric(as.Date("2015-7-15", origin = origin)), y = 145, label = "District Quarantine", color = "blue", alpha = 0.8)

#### Read Chiefdom data ####

#### Read PNAS incidence data ####
setwd("/Users/peakcm/Dropbox/Ebola/Spatial Analysis SL PNAS/")
data_ebola_PNAS <- read.csv(file = "PNAS_Cases_Confirmed.csv")

data_ebola_PNAS$date <- as.Date(data_ebola_PNAS$Date.of.symptom.onset, format = "%d-%b-%y")
date_range <- seq(from = min(data_ebola_PNAS$date),to =  max(data_ebola_PNAS$date), by = 1)

data_ebola_PNAS_melt <- melt(data = data_ebola_PNAS[,c("CHCODE", "date")], id.vars = c("CHCODE", "date"))

data_ebola_chiefdom_daily <- dcast(data_ebola_PNAS_melt, CHCODE ~ date, length)

# Add a zero-cases observation for each date for each chiefdom
missing_dates <- date_range[which((as.character(date_range) %in% names(data_ebola_chiefdom_daily)) == FALSE)]
for (missing_date in as.character(missing_dates)){
  data_ebola_chiefdom_daily <- cbind(data_ebola_chiefdom_daily, rep(0, nrow(data_ebola_chiefdom_daily)))
  names(data_ebola_chiefdom_daily)[length(names(data_ebola_chiefdom_daily))] <- paste(as.character(missing_date))
}

data_ebola_chiefdom_daily_melt <- melt(data_ebola_chiefdom_daily, id.vars = c("CHCODE"))
data_ebola_chiefdom_daily_melt <- data_ebola_chiefdom_daily_melt[order(data_ebola_chiefdom_daily_melt$CHCODE, data_ebola_chiefdom_daily_melt$variable),]

write.csv(data_ebola_chiefdom_daily_melt, file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/data_ebola_chiefdom_daily_melt.csv")

#### Read PNAS Population data ####
setwd("/Users/peakcm/Dropbox/Ebola/Spatial Analysis SL PNAS/")
data_pop_PNAS <- read.csv(file = "PNAS_Population.csv")
