# Analyze transition matrix pulled from Ericsson server

#### Load Libraries ####
library(ggplot2)
library(reshape2)
library(plyr)
library(TTR)
library(forecast)
library(nlme)      # Estimation of mixed effects models
library(lme4)      # Alternative package for mixed effects models
library(plm)       # Econometrics package for linear panel models
library(arm)       # Gelman & Hill code for mixed effects simulation
library(pcse)      # Calculate PCSEs for LS models (Beck & Katz)
library(tseries)   # For ADF unit root test
library(simcf)     # For panel functions and simulators
library(tile)           # For visualization of model inference
library(RColorBrewer)   # For nice colors
library(MASS)           # For mvrnorm()
source("/Users/peakcm/Desktop/Panel Data/Topic 7/helperCigs.R")  # For graphics functions

#### Save/Load Workspace ####
# save.image("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/20160419_workspace_analyze_transition_matrix.RData")
load("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/20160419_workspace_analyze_transition_matrix.RData")

# save.image("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/20160419_workspace_analyze_transition_matrix_2day.RData")
# load("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/20160419_workspace_analyze_transition_matrix_2day.RData")

# save.image("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/20160419_workspace_analyze_transition_matrix_3day.RData")
# load("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/20160419_workspace_analyze_transition_matrix_3day.RData")

#### Load Data ####
# Load transition matrix
# data_1day <- read.table("/Users/peakcm/Documents/SLE_Mobility/transition_matrix_collated_1day.csv", header = TRUE, sep = "\t", colClasses = c("character",rep("numeric", times = 103)))

# data_1day <- read.table("/Users/peakcm/Documents/SLE_Mobility/transition_matrix_collated_2day.csv", header = TRUE, sep = "\t", colClasses = c("character",rep("numeric", times = 103)))

# data_1day <- read.table("/Users/peakcm/Documents/SLE_Mobility/transition_matrix_collated_3day.csv", header = TRUE, sep = "\t", colClasses = c("character",rep("numeric", times = 103)))

setwd("/Users/peakcm/Dropbox/Ebola/Spatial Analysis SL PNAS/")
data_pop_PNAS <- read.csv(file = "PNAS_Population.csv")

#### Manipulate Travel Data ####
# split location tuple into two columns
nchar(data_1day[1,"tower_pair"])
summary(nchar(data_1day[,"tower_pair"]))
(x <- data_1day[54942,"tower_pair"]) # for a 6-character-long one
(x <- data_1day[59527,"tower_pair"]) # for a 14-character-long one

(first <- substr(x, start = 2, stop = regexpr(",", x)[1]-1))
(second <- substr(x, start = regexpr(",", x)[1]+2, stop = nchar(x)-1))

fcn_split_loc_tuple <- function(tuple, choice){
  if (choice == 1){
    first <- substr(tuple, start = 2, stop = regexpr(",", tuple)[1]-1)
    return(first)
  } else if (choice == 2){
    second <- substr(tuple, start = regexpr(",", tuple)[1]+2, stop = nchar(tuple)-1)
    return(second)
  }
}
fcn_split_loc_tuple(x, choice = 1)
fcn_split_loc_tuple(x, choice = 2)

head(data_1day$tower_pair)
cell_ids_first <- as.numeric(as.character(sapply(data_1day$tower_pair, function(x) fcn_split_loc_tuple(x, choice = 1))))
head(cell_ids_first, 100)
summary(cell_ids_first)
cell_ids_second <- as.numeric(as.character(sapply(data_1day$tower_pair, function(x) fcn_split_loc_tuple(x, choice = 2))))
head(cell_ids_second, 100)
summary(cell_ids_second)

data_1day <- data.frame(cbind(cell_ids_first, cell_ids_second, data_1day))
names(data_1day)[1] <- "cell_id_current"
names(data_1day)[2] <- "cell_id_previous"
head(data_1day)

# Add chiefdom, district, region for cell_id_current and cell_id_previous
tower_locations <- read.csv("/Users/peakcm/Documents/SLE_Mobility/data/Tower_Locations.csv")
tower_locations <- tower_locations[order(tower_locations$CellID),]
repeats <- tower_locations[tower_locations$CellID %in% names(which(table(tower_locations$CellID)>1)),]
repeats <- repeats[order(repeats$CellID),]
repeats_different_locations <- repeats[1,]
repeats_different_locations[1,] <- NA

for (i in seq(from = 1, to = (nrow(repeats)-3), by = 2)){ #every other tower, until the end, where it's triplicate, so you ignore
  if (repeats[i, "admin3"] != repeats[i+1, "admin3"]){
    repeats_different_locations <- rbind(repeats_different_locations, repeats[i,], repeats[i+1,])
  }
}
repeats_different_locations <- repeats_different_locations[2:nrow(repeats_different_locations),]
repeats_different_locations <- repeats_different_locations[order(repeats_different_locations$CellID),]

fcn_link_tower_political_unit <- function(cell_id, tower_locations, admin_unit, collapsed = FALSE){
  output <- tower_locations[tower_locations$CellID == cell_id, admin_unit]
  if (collapsed == FALSE){
    if (length(unique(output)) > 1){
      return(NA)
    } else { return(output[1]) }
  } else {
    if (length(unique(output)) > 1){
      return(NA)
    } else { 
      if (admin_unit == "admin3" & is.na(output[1])==0){
        if (as.numeric(output[1]) >= 4100 & as.numeric(output[1]) <= 4199){
          return(4199)
        } else if (as.numeric(output[1]) >= 4200){
          return(4299)
        } else if (output[1] == 1291){
          return(1212)
        } else if (output[1] == 1391){
          return(1304)
        } else if (output[1] == 2191){
          return(2102)
        } else if (output[1] == 3191){
          return(3108)
        } else if (output[1] == 3291){
          return(3204)
        } else {
          return(output[1])
          }
        } else {return(output[1])}
      }
    }
}

collapsed <- TRUE #Collapse the chiefdoms down to a list where we have ebola data

# collapsed <- FALSE
data_1day$admin3_current <- sapply(data_1day$cell_id_current, function(x) fcn_link_tower_political_unit(x, tower_locations, "admin3", collapsed))
data_1day$admin2_current <- sapply(data_1day$cell_id_current, function(x) fcn_link_tower_political_unit(x, tower_locations, "admin2"))
data_1day$admin1_current <- sapply(data_1day$cell_id_current, function(x) fcn_link_tower_political_unit(x, tower_locations, "admin1"))
 
data_1day$admin3_previous <- sapply(data_1day$cell_id_previous, function(x) fcn_link_tower_political_unit(x, tower_locations, "admin3", collapsed))
data_1day$admin2_previous <- sapply(data_1day$cell_id_previous, function(x) fcn_link_tower_political_unit(x, tower_locations, "admin2"))
data_1day$admin1_previous <- sapply(data_1day$cell_id_previous, function(x) fcn_link_tower_political_unit(x, tower_locations, "admin1"))

head(data_1day)

# Show rows missing chiefdom data
head(data_1day[is.na(data_1day$admin3_current)==1 , c("cell_id_current", "cell_id_previous", "tower_pair", "admin3_current", "admin3_previous")], 50)
head(data_1day[is.na(data_1day$admin3_previous)==1 , c("cell_id_current", "cell_id_previous", "tower_pair", "admin3_current", "admin3_previous")], 50)

# What percent of data are we missing at each admin level
nrow(data_1day[is.na(data_1day$admin3_current)==1 | is.na(data_1day$admin3_previous)==1 , ]) / nrow(data_1day)
nrow(data_1day[is.na(data_1day$admin2_current)==1 | is.na(data_1day$admin2_previous)==1 , ]) / nrow(data_1day)
nrow(data_1day[is.na(data_1day$admin1_current)==1 | is.na(data_1day$admin1_previous)==1 , ]) / nrow(data_1day)

# These are the truly missing towers
head(data_1day[is.na(data_1day$admin3_current)==1 & (data_1day$cell_id_current %in% repeats_different_locations$CellID)==0, c("cell_id_current", "cell_id_previous", "tower_pair", "admin3_current", "admin3_previous")], 50)
nrow(data_1day[is.na(data_1day$admin3_current)==1 & (data_1day$cell_id_current %in% repeats_different_locations$CellID)==0, c("cell_id_current", "cell_id_previous", "tower_pair", "admin3_current", "admin3_previous")])
cat("The following towers have unknown locations:", as.character(unique(data_1day[is.na(data_1day$admin3_current)==1 & (data_1day$cell_id_current %in% repeats_different_locations$CellID)==0, c("cell_id_current")])))

head(data_1day[is.na(data_1day$admin3_previous)==1 & (data_1day$cell_id_current %in% repeats_different_locations$CellID)==0, c("cell_id_current", "cell_id_previous", "tower_pair", "admin3_current", "admin3_previous")], 50)
nrow(data_1day[is.na(data_1day$admin3_previous)==1 & (data_1day$cell_id_previous %in% repeats_different_locations$CellID)==0, c("cell_id_current", "cell_id_previous", "tower_pair", "admin3_current", "admin3_previous")])
cat("The following towers have unknown locations:", as.character(unique(data_1day[is.na(data_1day$admin3_previous)==1 & (data_1day$cell_id_previous %in% repeats_different_locations$CellID)==0, c("cell_id_previous")])))

# Melt data
data_1day_melt <- melt(data_1day, id.vars = c("cell_id_current", "cell_id_previous", "tower_pair", 
                                              "admin3_current", "admin3_previous", 
                                              "admin2_current", "admin2_previous",
                                              "admin1_current", "admin1_previous"))
data_1day_melt_original <- data_1day_melt
data_1day_melt <- data_1day_melt[is.na(data_1day_melt$admin3_current) == 0 & is.na(data_1day_melt$admin3_previous) == 0,]
head(data_1day_melt)

# Convert to dates
fcn_convert_to_date <- function(string){
  string <- as.character(string)
  string <- substr(string, start = 2, stop = nchar(string))
  return(as.Date(string, format = "%Y.%m.%d"))
}

data_1day_melt$variable <- fcn_convert_to_date(data_1day_melt$variable)
# data_1day_melt$date <- fcn_convert_to_date(data_1day_melt$date)
if (names(data_1day_melt)[10] == "variable") {names(data_1day_melt)[10] <- "date"}
if (names(data_1day_melt)[11] == "value") {names(data_1day_melt)[11] <- "count"}
head(data_1day_melt)

# Plot
cell_1 <- c(2188, 1468)
cell_2 <- c(2188, 1468)
head(data_1day_melt[data_1day_melt$cell_id_current %in% cell_1 & data_1day_melt$cell_id_previous %in% cell_2,])
ggplot(data_1day_melt[data_1day_melt$cell_id_current %in% cell_1 & data_1day_melt$cell_id_previous %in% cell_2,], aes(x = date, y = count, group = interaction(cell_id_current, cell_id_previous))) +
  geom_line(aes(color = interaction(cell_id_current, cell_id_previous)))

# Create a dataset with the sum of trips across all time
data_1day_melt <- data_1day_melt[order(data_1day_melt$cell_id_current, data_1day_melt$cell_id_previous, data_1day_melt$date),]

data_1day_tower_cumtrips <- dcast(data_1day_melt,
                                    cell_id_current + cell_id_previous + tower_pair + 
                                    admin3_current + admin3_previous +
                                    admin2_current + admin2_previous +
                                    admin1_current + admin1_previous ~ . , sum, value.var = "count" )
names(data_1day_tower_cumtrips)[length(names(data_1day_tower_cumtrips))] <- "cumtrips"
# View(data_1day_tower_cumtrips)
hist(log(data_1day_tower_cumtrips$cumtrips))

# Melt, cast, and remelt data to remove repeated entries for chiefdoms depending on number of tower combinations.
data_1day_melt_chiefdom <- data_1day_melt[,c("admin3_current", "admin3_previous", "date", "count")]
data_1day_chiefdom <- dcast(data_1day_melt_chiefdom,
                            admin3_current + admin3_previous +
                              date ~ . , sum, value.var = "count")
data_1day_melt_chiefdom <- melt(data_1day_chiefdom, id.vars = c("admin3_current", "admin3_previous", "date"))
names(data_1day_melt_chiefdom)[c(4,5)] <- c("variable", "count")

# Create a dataset with trips between chiefdoms
names(data_1day_chiefdom)[length(names(data_1day_chiefdom))] <- "trips"
head(data_1day_chiefdom)
hist(log(data_1day_chiefdom$trips))
hist(log(data_1day_chiefdom[data_1day_chiefdom$admin3_current == 4299,]$trips))
ggplot(data = data_1day_chiefdom[data_1day_chiefdom$admin3_previous == 4299,], aes(x = date, y = log10(trips), group = admin3_current, color = admin3_current)) + geom_line()
data_1day_chiefdom[data_1day_chiefdom$admin3_current == 1102 & data_1day_chiefdom$admin3_previous == 1102 & data_1day_chiefdom$date == "2015-03-27",]

data_1day_melt_chiefdom$day_of_week <- as.factor(as.POSIXlt(data_1day_melt_chiefdom$date)$wday)

data_1day_melt_chiefdom$nsahd <- 0
data_1day_melt_chiefdom[data_1day_melt_chiefdom$date == "2015-03-27" |
                          data_1day_melt_chiefdom$date == "2015-03-28" |
                          data_1day_melt_chiefdom$date =="2015-03-29","nsahd"] <- 1

data_1day_melt_chiefdom$pair <- factor(paste(as.character(data_1day_melt_chiefdom$admin3_current), as.character(data_1day_melt_chiefdom$admin3_previous)))

data_1day_melt_chiefdom_na.rm <- data_1day_melt_chiefdom[is.na(data_1day_melt_chiefdom$admin3_current)==0 & is.na(data_1day_melt_chiefdom$admin3_previous)==0,]

data_1day_melt_chiefdom_na.rm$same_chiefdom <- 0
data_1day_melt_chiefdom_na.rm[data_1day_melt_chiefdom_na.rm$admin3_current == data_1day_melt_chiefdom_na.rm$admin3_previous, "same_chiefdom"] <- 1

summary(data_1day_melt_chiefdom_na.rm[data_1day_melt_chiefdom_na.rm$same_chiefdom == 1 &
                                        data_1day_melt_chiefdom_na.rm$nsahd == 1,"count"])
summary(data_1day_melt_chiefdom_na.rm[data_1day_melt_chiefdom_na.rm$same_chiefdom == 1 &
                                        data_1day_melt_chiefdom_na.rm$nsahd == 0,"count"])

summary(data_1day_melt_chiefdom_na.rm[data_1day_melt_chiefdom_na.rm$same_chiefdom == 0 &
                                        data_1day_melt_chiefdom_na.rm$nsahd == 1,"count"])
summary(data_1day_melt_chiefdom_na.rm[data_1day_melt_chiefdom_na.rm$same_chiefdom == 0 &
                                        data_1day_melt_chiefdom_na.rm$nsahd == 0,"count"])

# Create a dataset with cumulative trips between chiefdoms
data_1day_chiefdom_cumtrips <- dcast(data_1day_melt_chiefdom, admin3_current + admin3_previous + pair ~ . , sum, value.var = "count")
names(data_1day_chiefdom_cumtrips)[length(names(data_1day_chiefdom_cumtrips))] <- "cumtrips"
head(data_1day_chiefdom_cumtrips)
hist(log10(data_1day_chiefdom_cumtrips$cumtrips))

# Make a list of chiefdom pairs that have at least N trips per day
min_trips = 100 # For 1 trip per day 
pairs_cumtrips_min100 <- data_1day_chiefdom_cumtrips[data_1day_chiefdom_cumtrips$cumtrips > 100,"pair"]
length(pairs_cumtrips_min100)
hist(log10(data_1day_chiefdom_cumtrips[data_1day_chiefdom_cumtrips$pair %in% pairs_cumtrips_min100,]$cumtrips))

min_trips = 1000 # For 10 trips per day
pairs_cumtrips_min1000 <- data_1day_chiefdom_cumtrips[data_1day_chiefdom_cumtrips$cumtrips > 1000,"pair"]
length(pairs_cumtrips_min1000)
hist(log10(data_1day_chiefdom_cumtrips[data_1day_chiefdom_cumtrips$pair %in% pairs_cumtrips_min1000,]$cumtrips))

# format for time series ARIMA model
temp <- data_1day_melt_chiefdom_na.rm[,c("admin3_current", "admin3_previous", "date", "count")]
data_1day_chiefdom_na.rm <- dcast(data = temp, admin3_current + admin3_previous ~ date , value.var = "count")
head(data_1day_chiefdom_na.rm)
head(data_1day_chiefdom_na.rm[data_1day_chiefdom_na.rm$admin3_current == 1102 & data_1day_chiefdom_na.rm$admin3_previous==1102,])

# format for a very very long time series 
 # Create a single sequence of number of trips between chiefdoms with NA's in between, holding periodicity
 # Can add an external varaible for the NSAHD's 000011100000....000NANANANANANA00000111100000... etc
 # Can add external variable for incidence (consider rolling average) in source, destination, and difference between two

#### Save/Load dataset ####
# write.csv(data_1day_melt_chiefdom_na.rm, file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/data_1day_melt_chiefdom_na.rm.csv")
# data_1day_melt_chiefdom_na.rm <- read.csv(file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/data_1day_melt_chiefdom_na.rm.csv")

# write.csv(data_1day_melt_chiefdom_na.rm, file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/data_2day_melt_chiefdom_na.rm.csv")
# data_1day_melt_chiefdom_na.rm <- read.csv(file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/data_2day_melt_chiefdom_na.rm.csv")

write.csv(data_1day_melt_chiefdom_na.rm, file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/data_3day_melt_chiefdom_na.rm.csv")
data_1day_melt_chiefdom_na.rm <- read.csv(file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/data_3day_melt_chiefdom_na.rm.csv")

#### Borrow from Christopher Adolph's method. Topic 7. "panelGMMtemplate.r ####
CHList <- unique(as.character(data_1day_chiefdom_na.rm$admin3_current))
nCH <- length(CHList)
dateList <- unique(data_1day_melt$date)
ndate <- length(dateList)
tripsMat <- casesMat <- popMat <- NULL

# Change structure so that each column is a chiefdom pair and each row is a day
data_1day_chiefdom_na.rm_transpose <- t(data_1day_chiefdom_na.rm)
colnames <- data_1day_chiefdom_na.rm_transpose[c(1,2),]
colnames <- rbind(colnames, seq(1, nrow(colnames)))
nCH_pairs <- ncol(colnames)
tripsMat <- data_1day_chiefdom_na.rm_transpose[3:nrow(data_1day_chiefdom_na.rm_transpose),]
tripsMat_log10 <- log10(tripsMat+1) 

tripsTrace <- polylinesTile(y=tripsMat_log10,
                             x=matrix(dateList, nrow=ndate, ncol=nCH_pairs),
                             id=matrix(1:nCH_pairs, nrow=ndate, ncol=nCH_pairs, byrow=TRUE),
                             col="gray75",
                             plot=1)

tile(tripsTrace)

# Panel based diagnostics available in the plm library
# First, create a plm data frame (special data frame that "knows" the unit variable and time variable
pdata <- pdata.frame(data_1day_melt_chiefdom, index=c("pair", "date"))

pgmmformula.1a <- count ~ nsahd

#### Borrow from Christopher Adolph's method. Topic 6. "panelARIMAtemplate.r ####

df_ARIMA <- data_1day_melt_chiefdom_na.rm

# Create lags and differences now to correctly listwise delete
count_lag0 <- lagpanel(df_ARIMA$count, df_ARIMA$pair, df_ARIMA$date, 1) # Y(t-1)
count_diff0 <- df_ARIMA$count - count_lag0 # Y(t) - Y(t-1)
count_difflag0 <- lagpanel(count_diff0, df_ARIMA$pair, df_ARIMA$date, 1) # Y(t-1) - Y(t-2)
count_diff10 <- count_diff0 - count_difflag0 #Trying to calculate the "second difference" [Y(t) - Y(t-1)] - [Y(t-1) - Y(t-2)]
count_difflag20 <- lagpanel(count_diff0, df_ARIMA$pair, df_ARIMA$date, 2) # Y(t-2) - Y(t-3)

# Create a code for the chiefdom pairs
pair_codebook <- data.frame(pair = unique(df_ARIMA$pair), pair_code = seq(1, length(unique(df_ARIMA$pair))))
pair_converter <- function(pair, codebook){
  codebook[codebook$pair == pair, "pair_code"][1]
}
pair_converter("1102 1110", pair_codebook)

df_ARIMA$pair_code <- rep(seq(1, length(unique(df_ARIMA$pair))), each = length(unique(df_ARIMA$date)))

# Check to make sure df_ARIMA$pair_code is done correctly
for (row in floor(runif(n = 100, min = 1, max = nrow(df_ARIMA)))){
  if (pair_converter(df_ARIMA[row, "pair"], pair_codebook) != df_ARIMA[row,"pair_code"]){
    cat("nope")
  } else {cat(".")}
}

# Listwise delete using only the data we need
df_ARIMA_select <- na.omit(cbind(df_ARIMA$pair_code,
                            as.factor(df_ARIMA$date),
                            df_ARIMA$count,
                            df_ARIMA$nsahd,
                            df_ARIMA$day_of_week,
                            df_ARIMA$same_chiefdom,
                            df_ARIMA$admin3_current,
                            df_ARIMA$admin3_previous,
                            count_lag0,
                            count_diff0,
                            count_difflag0,
                            count_diff10,
                            count_difflag20))
df_ARIMA_select <- as.data.frame(df_ARIMA_select)
names(df_ARIMA_select) <- c("pair_code","date","count","nsahd",
                       "day_of_week","same_chiefdom","admin3_current","admin3_previous",
                       "count_lag", "count_diff", "count_difflag", "count_diff1", "count_difflag2")

# Read ebola incidence data
data_ebola_chiefdom_daily_melt <- read.csv(file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/data_ebola_chiefdom_daily_melt.csv")
data_ebola_chiefdom_daily_melt <- data_ebola_chiefdom_daily_melt[order(data_ebola_chiefdom_daily_melt$CHCODE, data_ebola_chiefdom_daily_melt$variable),]

length(unique(c(unique(data_ebola_chiefdom_daily_melt$CHCODE), unique(df_ARIMA_select$admin3_current))))
relevant_CHCODES <- sort(unique(c(unique(data_ebola_chiefdom_daily_melt$CHCODE), unique(df_ARIMA_select$admin3_current))))
CHCODES_missing_from_travel_data <- relevant_CHCODES[which(relevant_CHCODES %in% df_ARIMA_select$admin3_current == FALSE)]
CHCODES_missing_from_ebola_data <- relevant_CHCODES[which(relevant_CHCODES %in% data_ebola_chiefdom_daily_melt$CHCODE == FALSE)]

# Add missing chiefdoms to the ebola data with zero cases on each day
for (chief in CHCODES_missing_from_ebola_data){   
  new_rows <- data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$CHCODE == 1101,]
  new_rows$CHCODE <- chief
  new_rows$value <- 0
  data_ebola_chiefdom_daily_melt <- rbind(data_ebola_chiefdom_daily_melt, new_rows)
}
data_ebola_chiefdom_daily_melt <- data_ebola_chiefdom_daily_melt[order(data_ebola_chiefdom_daily_melt$CHCODE, data_ebola_chiefdom_daily_melt$variable),]
relevant_CHCODES[which(relevant_CHCODES %in% data_ebola_chiefdom_daily_melt$CHCODE == FALSE)]

names(data_ebola_chiefdom_daily_melt)[4] <- c("daily_cases")

# Create a rolling weekly count
data_ebola_chiefdom_daily_melt$weekly_cases_rolling <- NA
weekly <- function(x,n=7){filter(x,rep(1,n), sides=1)}
for (chief in unique(data_ebola_chiefdom_daily_melt$CHCODE)){
  data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$CHCODE == chief,"weekly_cases_rolling"] <- weekly(data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$CHCODE == chief, "daily_cases"])
}

# Create a cumulative case count
data_ebola_chiefdom_daily_melt$cum_cases <- NA
for (chief in unique(data_ebola_chiefdom_daily_melt$CHCODE)){
  data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$CHCODE == chief,"cum_cases"] <- cumsum(data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$CHCODE == chief, "daily_cases"])
}

# Add a "date" stamp to the ebola data that matches the format in the travel dataset
data_ebola_chiefdom_daily_melt$date <- NA
start <- 4 # March 23, 2015
end <- 103
day_start <- as.Date("2015-03-23")
for (counter in start:end){
  day_current <- day_start + counter - start
  data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$variable == as.character(day_current),"date"] <- counter
  counter <- counter + 1
  cat(data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$variable == as.character(day_current),"date"][1], "\n")
}

sum(data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$date %in% 103, "variable"] != "2015-06-30")

# Add ebola data to travel dataset
df_ARIMA_select$admin3_current_daily_incidence <- NA
df_ARIMA_select$admin3_previous_daily_incidence <- NA

df_ARIMA_select$admin3_current_cum_incidence <- NA
df_ARIMA_select$admin3_previous_cum_incidence <- NA

df_ARIMA_select$admin3_current_weekly_incidence <- NA
df_ARIMA_select$admin3_previous_weekly_incidence <- NA

for (chief_current in unique(df_ARIMA_select$admin3_current)){
  for (chief_previous in unique(df_ARIMA_select$admin3_previous)){
    if (nrow(df_ARIMA_select[df_ARIMA_select$admin3_current == chief_current & df_ARIMA_select$admin3_previous == chief_previous,]) > 0){
      if (nrow(df_ARIMA_select[df_ARIMA_select$admin3_current == chief_current & df_ARIMA_select$admin3_previous == chief_previous,]) != nrow(data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$CHCODE == chief_current & data_ebola_chiefdom_daily_melt$date %in% seq(4, 103),c("daily_cases", "cum_cases", "weekly_cases_rolling")])){cat("\nError 1. chief_current = ", chief_current, "and chief_previous = ", chief_previous)}
      df_ARIMA_select[df_ARIMA_select$admin3_current == chief_current & df_ARIMA_select$admin3_previous == chief_previous, c("admin3_current_daily_incidence", "admin3_current_cum_incidence", "admin3_current_weekly_incidence")] <- data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$CHCODE == chief_current & data_ebola_chiefdom_daily_melt$date %in% seq(4, 103),c("daily_cases", "cum_cases", "weekly_cases_rolling")]
      df_ARIMA_select[df_ARIMA_select$admin3_current == chief_current & df_ARIMA_select$admin3_previous == chief_previous,  c("admin3_previous_daily_incidence", "admin3_previous_cum_incidence", "admin3_previous_weekly_incidence")] <- data_ebola_chiefdom_daily_melt[data_ebola_chiefdom_daily_melt$CHCODE == chief_previous & data_ebola_chiefdom_daily_melt$date %in% seq(4, 103),c("daily_cases", "cum_cases", "weekly_cases_rolling")]
    }
  }
  cat(".")
}

df_ARIMA_select$admin3_total_daily_incidence <- df_ARIMA_select$admin3_previous_daily_incidence + df_ARIMA_select$admin3_current_daily_incidence
df_ARIMA_select$admin3_previous_minus_current_daily_incidence <- df_ARIMA_select$admin3_previous_daily_incidence - df_ARIMA_select$admin3_current_daily_incidence

df_ARIMA_select$admin3_total_cum_incidence <- df_ARIMA_select$admin3_previous_cum_incidence + df_ARIMA_select$admin3_current_cum_incidence
df_ARIMA_select$admin3_previous_minus_current_cum_incidence <- df_ARIMA_select$admin3_previous_cum_incidence - df_ARIMA_select$admin3_current_cum_incidence

df_ARIMA_select$admin3_total_weekly_incidence <- df_ARIMA_select$admin3_previous_weekly_incidence + df_ARIMA_select$admin3_current_weekly_incidence
df_ARIMA_select$admin3_previous_minus_current_weekly_incidence <- df_ARIMA_select$admin3_previous_weekly_incidence - df_ARIMA_select$admin3_current_weekly_incidence

# Drop first day, with no observations
df_ARIMA_select <- df_ARIMA_select[df_ARIMA_select$date > 4,]

# Add intervention data (local quarantines)
lockdown_saturdays_list <- as.numeric(as.Date(c("2015-04-04", "2015-04-11", "2015-04-18")) - as.Date("2015-03-23"))
df_ARIMA_select$lockdown_saturdays <- 0
df_ARIMA_select[df_ARIMA_select$date %in% lockdown_saturdays_list,"lockdown_saturdays"] <- 1
  
  # Operation Northern Push. started June 16 for at least 21 days. chiefdom quarantines in Kambia (22) and Port Loko (24)
north_push_start <- as.numeric(as.Date("2015-06-16") - as.Date("2015-03-23"))
north_push_chiefdoms <- unique(df_ARIMA_select$admin3_current)[(unique(df_ARIMA_select$admin3_current) >= 2200 & unique(df_ARIMA_select$admin3_current) < 2300) | (unique(df_ARIMA_select$admin3_current) >= 2400 & unique(df_ARIMA_select$admin3_current) < 2500)]
df_ARIMA_select$operation_northern_push_previous <- 0
df_ARIMA_select$operation_northern_push_current <- 0

df_ARIMA_select[df_ARIMA_select$admin3_current %in% north_push_chiefdoms & df_ARIMA_select$date > north_push_start, "operation_northern_push_current"] <- 1
df_ARIMA_select[df_ARIMA_select$admin3_previous %in% north_push_chiefdoms & df_ARIMA_select$date > north_push_start, "operation_northern_push_previous"] <- 1

  # Create a control variable for Operation Northern Push. started June 16 for at least 21 days. chiefdom quarantines in Kambia (22) and Port Loko (24). Include all other districts.
df_ARIMA_select$operation_northern_push_previous_control <- 0
df_ARIMA_select$operation_northern_push_current_control <- 0

df_ARIMA_select[(df_ARIMA_select$admin3_current %in% north_push_chiefdoms)==0 & df_ARIMA_select$date > north_push_start, "operation_northern_push_current_control"] <- 1
df_ARIMA_select[(df_ARIMA_select$admin3_current %in% north_push_chiefdoms)==0 & df_ARIMA_select$date > north_push_start, "operation_northern_push_previous_control"] <- 1

# Add population data
setwd("/Users/peakcm/Dropbox/Ebola/Spatial Analysis SL PNAS/")
data_pop_PNAS <- read.csv(file = "PNAS_Population.csv")

df_ARIMA_select$population_current <- NA
df_ARIMA_select$population_previous <- NA

for (chief in unique(df_ARIMA_select$admin3_current)){
  df_ARIMA_select[df_ARIMA_select$admin3_current == chief, "population_current"] <- data_pop_PNAS[data_pop_PNAS$CHCODE == chief, "Total2014Inferred"]
  df_ARIMA_select[df_ARIMA_select$admin3_previous == chief, "population_previous"] <- data_pop_PNAS[data_pop_PNAS$CHCODE == chief, "Total2014Inferred"]
}

# Add ebola incidence per capita data
df_ARIMA_select$admin3_total_cum_incidence_per_100000pop <- NA
df_ARIMA_select$admin3_total_cum_incidence_per_100000pop <- 100000 * df_ARIMA_select$admin3_total_cum_incidence / (df_ARIMA_select$population_current + df_ARIMA_select$population_previous)

#### Save combined data ####
# write.csv(df_ARIMA_select, file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/df_ARIMA_select.csv") #note that currently, this is the min1000 file
# write.csv(df_ARIMA_select, file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/df_ARIMA_select_2day.csv") #note that currently, this is the min1000 file
write.csv(df_ARIMA_select, file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/df_ARIMA_select_3day.csv") #note that currently, this is the min1000 file

##### Load combined data ####
# df_ARIMA_select <- read.csv(file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/df_ARIMA_select.csv")
# df_ARIMA_select <- read.csv(file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/df_ARIMA_select_2day.csv")
df_ARIMA_select <- read.csv(file = "/Users/peakcm/Documents/SLE_Mobility/sle_ericson/df_ARIMA_select_3day.csv")

#### Restrict the data to only those pairs with at least 10 trips per day ####
df_ARIMA_select <- df_ARIMA_select[df_ARIMA_select$pair_code %in% pair_codebook[pair_codebook$pair %in% pairs_cumtrips_min1000,"pair_code"],]
nrow(df_ARIMA_select)

# Create a list of the unique chiefdom pairs
pairlist <- unique(df_ARIMA_select$pair_code)

# Create a matrix of fixed effect dummies (for convenience)
# fe <- makeFEdummies(df_ARIMA_select$pair_code)
fe <- model.matrix(~factor(df_ARIMA_select$pair_code)-1)
colnames(fe) <- seq(1, length(colnames(fe)))

# Find the number of units
n <- length(pairlist)

setwd("/Users/peakcm/Documents/SLE_Mobility/sle_ericson/Output")
# Diagnose time series
# Look at the time series for each pair
for (i in 1:length(pairlist)) {
  currpair <- pairlist[i]
  filename <- paste("pair",as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"]),".pdf",sep="")
  pdf(filename,width=6,height=3.25)
  plot(df_ARIMA_select$count[df_ARIMA_select$pair_code == currpair],type="l",ylab="count",xlab="Time",
       main = paste("Pair",as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"])) )
  dev.off()
}

# Look at the ACF for each pair
for (i in 1:length(pairlist)) {
  currpair <- pairlist[i]
  filename <- paste("acfpair",as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"]),".pdf",sep="")
  pdf(filename,width=6,height=3.25)
  acf(df_ARIMA_select$count[df_ARIMA_select$pair_code==currpair], main = paste("Pair",as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"])))
  dev.off()
}

# Look at the PACF
for (i in 1:length(pairlist)) {
  currpair <- pairlist[i]
  filename <- paste("pacfpair",as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"]),".pdf",sep="")
  pdf(filename,width=6,height=3.25)
  pacf(df_ARIMA$count[df_ARIMA_select$pair_code==currpair], main = paste("Pair",as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"])))
  dev.off()
}

# Check for a unit root in each pair
PPtest.pvalues <- rep(0,n)
adftest.pvalues <- rep(0,n)
for (i in 1:length(pairlist)) {
  currpair <- pairlist[i]
  
  # Check PP unit root test, omitting errors due to short series
  curPP <- try(PP.test(df_ARIMA_select$count[df_ARIMA_select$pair_code==currpair])$p.value)
  if (any(class(curPP)=="try-error")) curPP <- NA
  PPtest.pvalues[i] <- curPP
  
  curadf <- try(adf.test(df_ARIMA_select$count[df_ARIMA_select$pair_code==currpair])$p.value)
  if (any(class(curadf)=="try-error")) curadf <- NA
  adftest.pvalues[i] <- curadf
}

pdf("PPtest.pdf",width=6,height=3.25)
hist(PPtest.pvalues, breaks = seq(0,1,0.01))          # Plot a histogram of the p-values
dev.off()

pdf("adftest.pdf",width=6,height=3.25)
hist(adftest.pvalues, breaks = seq(0,1,0.01))         # Plot a histogram of the p-values
dev.off()

# Repeat the ACF, PACF, and unit root tests on the log transformed

for (i in 1:length(pairlist)) {
  currpair <- pairlist[i]
  filename <- paste("logcountpair",as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"]),".pdf",sep="")
  pdf(filename,width=6,height=3.25)
  plot(log10(1+df_ARIMA_select$count[df_ARIMA_select$pair_code==currpair]),type="l",ylab="log10(count)",xlab="Time",
       main = paste("Pair", as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"])) )
  dev.off()
}

# Look at the ACF for each country in 
for (i in 1:length(pairlist)) {
  currpair <- pairlist[i]
  filename <- paste("acflogcountpair",as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"]),".pdf",sep="")
  pdf(filename,width=6,height=3.25)
  acf(log10(1+df_ARIMA_select$count[df_ARIMA_select$pair_code==currpair]), main = as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"]))
  dev.off()
}

# Look at the PACF in 
for (i in 1:length(pairlist)) {
  currpair <- pairlist[i]
  filename <- paste("pacflogcountpair",as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"]),".pdf",sep="")
  pdf(filename,width=6,height=3.25)
  pacf(log10(1+df_ARIMA_select$count[df_ARIMA_select$pair_code==currpair]), main = as.character(pair_codebook[pair_codebook$pair_code == currpair, "pair"]))
  dev.off()
}

# Check for a unit root in each country, 
PPtestdiff.pvalues <- rep(0,n)
adftestdiff.pvalues <- rep(0,n)
for (i in 1:length(pairlist)) {
  currpair <- pairlist[i]
  
  # Check PP unit root test, omitting errors due to short series
  curPPdiff <- try(PP.test(df_ARIMA_select$count_diff[df_ARIMA_select$pair_code==currpair])$p.value)
  if (any(class(curPPdiff)=="try-error")) curPPdiff <- NA
  PPtestdiff.pvalues[i] <- curPPdiff
  
  curadfdiff <- try(adf.test(df_ARIMA_select$count_diff[df_ARIMA_select$pair_code==currpair])$p.value)
  if (any(class(curadfdiff)=="try-error")) curadfdiff <- NA
  adftestdiff.pvalues[i] <- curadfdiff
}

pdf("PPtestdiff.pdf",width=6,height=3.25)
hist(PPtestdiff.pvalues, breaks = seq(0,1,0.01))          # Plot a histogram of the p-values
dev.off()

pdf("adftestdiff.pdf",width=6,height=3.25)
hist(adftestdiff.pvalues, breaks = seq(0,1,0.01))         # Plot a histogram of the p-values
dev.off()

#### Find ARIMA characteristics ####

# Run auto.arima to find the range of possible values
auto_ARIMA_output <- data.frame(matrix(NA, nrow = length(pairlist), ncol = 5))
names(auto_ARIMA_output) <- c("pair_code","p", "d", "q", "AIC")
auto_ARIMA_output$pair_code <- pairlist

for (pair in pairlist){
  ts <- log(1+ts(df_ARIMA_select[df_ARIMA_select$pair_code == pair, "count"]))
  out <- auto.arima(ts)
  p <- out$arma
  auto_ARIMA_output[auto_ARIMA_output$pair_code == pair, c("p","d","q","AIC")] <- c(arimaorder(out), out$aic)
}
View(auto_ARIMA_output)
hist(auto_ARIMA_output$p)
hist(auto_ARIMA_output$d)
hist(auto_ARIMA_output$q)

# Select possible candidates
p_candidates <- c(0,1,2)
d_candidates <- c(0,1)
q_candidates <- c(0,1,2)

ARIMA_output <- data.frame(matrix(NA, nrow = length(pairlist)*length(p_candidates)*length(q_candidates)*length(d_candidates), ncol = 5))
names(ARIMA_output) <- c("pair_code","p", "d", "q", "AIC")
ARIMA_output$pair_code <- rep(pairlist, each = length(p_candidates)*length(q_candidates)*length(d_candidates))

row = 1
while (row <= nrow(ARIMA_output)){
  pair <- ARIMA_output[row, "pair_code"]
  ts <- log(1+ts(df_ARIMA_select[df_ARIMA_select$pair_code == pair, "count"]))
  for (p in p_candidates){
    for (d in d_candidates){
      for (q in q_candidates){
        if (sum(c(p,d,q) == c(1,1,1))==3){
          out <- arima(ts, order = c(p,d,q))
        } else {
          out <- arima(ts, order = c(p,d,q), method = "ML")
        }
        ARIMA_output[row,] <- c(pair,p,d,q,out$aic)
        row = row + 1
      }
    }
  }
  cat(".")
}

ARIMA_output$pdq <- paste(ARIMA_output$p, ARIMA_output$d, ARIMA_output$q, sep = ",")

sort(tapply(ARIMA_output$AIC, ARIMA_output$pdq, mean)) #2,0,2 seems to be min. 1,0,2 and 1,0,1 are very close too. 2,0,2 is not recommended, though, because p and q are both >1

#### Random Effects Model ####
# See "Lab_5.R

search()
detach(df_ARIMA_select)
attach(df_ARIMA_select)
names(df_ARIMA_select)

# Estimate a random effects AR(I)MA(p,q) model using lme (Restricted ML)
lme.res1 <- lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  fixed = log(count+1) ~ nsahd + nsahd*same_chiefdom + as.factor(day_of_week),			# i.e. response variable and explanatory variables 
  
  # The random effects component
  random = list(pair_code = ~ 1),						# 1 indicates the intercept and COUNTRY indicates the grouping
  
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ date | pair_code,
                        p = 1,  # AR(p) order
                        q = 2   # MA(q) order
  ) 
)

# Extract model results
pe.res1 <- fixed.effects(lme.res1)        # Point estimates of fixed effects
vc.res1 <- vcov(lme.res1)                 # Var-cov matrix of fixed effects estimates
se.res1 <- sqrt(diag(vc.res1))            # std erros of fixed effects estimates
re.res1 <- random.effects(lme.res1)       # "Estimated" random effects by group 
ll.res1 <- logLik(lme.res1)               # Log-likelihood at maximum
resid.res1 <- resid(lme.res1)             # Residuals
aic.res1 <- AIC(lme.res1)                 # Akaike Information Criterion

summary(lme.res1)

# p,d,q AIC records
# 1,0,2 AIC = 41244.09. 1-exp(-1.019) = 64% decrease travel between chiefdoms during nsahd. 2.7% decrease in travel in within-chiefdom during nsahd. exp(3.51) = 33 times higher travel within chiefdoms than between. All super significant (|t| > 30).
# 1,0,1 AIC = 
# 2,0,1 AIC = 

# Include ebola incidence metrics
# Estimate a random effects AR(I)MA(p,q) model using lme (Restricted ML)
lme.res2 <- lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  fixed = log(count+1) ~ as.factor(day_of_week) + nsahd + nsahd*same_chiefdom + admin3_total_cum_incidence*nsahd ,			# i.e. response variable and explanatory variables 
  
  # The random effects component
  random = ~ 1 | pair_code,						# 1 indicates the intercept and COUNTRY indicates the grouping
  
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ date | pair_code,
                        p = 1,  # AR(p) order
                        q = 2   # MA(q) order
  ) 
)

# Extract model results
pe.res2 <- fixed.effects(lme.res2)        # Point estimates of fixed effects
vc.res2 <- vcov(lme.res2)                 # Var-cov matrix of fixed effects estimates
se.res2 <- sqrt(diag(vc.res2))            # std erros of fixed effects estimates
re.res2 <- random.effects(lme.res2)       # "Estimated" random effects by group 
ll.res2 <- logLik(lme.res2)               # Log-likelihood at maximum
resid.res2 <- resid(lme.res2)             # Residuals
aic.res2 <- AIC(lme.res2)                 # Akaike Information Criterion

summary(lme.res2)

# p,d,q AIC
# Previous and Current weekly incidence plus their interaction with nsahd. 1,0,2. AIC = 41698.85. The weekly incidence wasn't significant on its own, but for each additional 10 cases in the origin (destination) chiefdom, the travel reduced by an additional 6.7% (5.2%)
# Previous and current cum incidence plus their interaction with nsahd. 1,0,2. AIC = 41455.42. The cumulative incidence weren't significant on their own, but for each additional 100 cases in the origin (destination) chiefdom, the intervention reduced travel by an additional 7.5% (5.9%)
# Total cum incidence plus its interaction with nsahd. 1,0,2. AIC = 41433.56 The cumulative incidence wasn't significant on its own, but for each additional 100 cases in either the origin or destination chiefdom, the intervention reduced travel by an additional 6.8% (noting results above, the incidence in the origin is slightly more influential, at 7.5% as compared to 5.9%).

# Include nsahd, saturday lockdowns, operational northern push, and ebola incidence metrics
# Estimate a random effects AR(I)MA(p,q) model using lme (Restricted ML)
lme.res3 <- lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  fixed = log(count+1) ~ as.factor(day_of_week) + nsahd + nsahd*same_chiefdom + nsahd*admin3_total_cum_incidence_per_100000pop + lockdown_saturdays + lockdown_saturdays*same_chiefdom + operation_northern_push_current + operation_northern_push_previous +  operation_northern_push_current*operation_northern_push_previous,			# i.e. response variable and explanatory variables 
  
  # The random effects component
  random = ~ 1 | pair_code,						# 1 indicates the intercept and COUNTRY indicates the grouping
  
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ date | pair_code,
                        p = 1,  # AR(p) order
                        q = 2   # MA(q) order
  ) 
)

# Extract model results
pe.res3 <- fixed.effects(lme.res3)        # Point estimates of fixed effects
vc.res3 <- vcov(lme.res3)                 # Var-cov matrix of fixed effects estimates
se.res3 <- sqrt(diag(vc.res3))            # std erros of fixed effects estimates
re.res3 <- random.effects(lme.res3)       # "Estimated" random effects by group 
ll.res3 <- logLik(lme.res3)               # Log-likelihood at maximum
resid.res3 <- resid(lme.res3)             # Residuals
aic.res3 <- AIC(lme.res3)                 # Akaike Information Criterion

summary(lme.res3)
1-exp(pe.res3["nsahd"])
1-exp(100*pe.res3["admin3_total_cum_incidence_per_100000pop"])
(1- exp(pe.res3["nsahd"] + 10*pe.res3["nsahd:admin3_total_cum_incidence_per_100000pop"])) - (1- exp(pe.res3["nsahd"]))
1-exp(pe.res3["operation_northern_push_current"])
1-exp(pe.res3["operation_northern_push_previous"])

# AIC = 40421.89 including nsahd, total_cum_incidence_per_100000pop, lockdown_saturdays, operation_northern_push_current, operation_northern_push_previous, and each of their interactions with same_chiefdom. However, lockdown_saturdays are turning up positive, even when a fixed effect for day_of_week is added. Consider interacting these?

#### Random effects model, excluding freetown ####

search()
detach(df_ARIMA_select)
df_ARIMA_select_no4299 <- df_ARIMA_select[(df_ARIMA_select$admin3_current %in% "4299") == 0 & (df_ARIMA_select$admin3_previous %in% "4299") == 0,]
attach(df_ARIMA_select_no4299)
names(df_ARIMA_select_no4299)

# Include ebola incidence metrics
# Estimate a random effects AR(I)MA(p,q) model using lme (Restricted ML)
lme.res2_no4299 <- lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  fixed = log(count+1) ~ nsahd + nsahd*same_chiefdom + admin3_total_cum_incidence*nsahd ,			# i.e. response variable and explanatory variables 
  
  # The random effects component
  random = ~ 1 | pair_code,						# 1 indicates the intercept and COUNTRY indicates the grouping
  
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ date | pair_code,
                        p = 1,  # AR(p) order
                        q = 2   # MA(q) order
  ) 
)

# Extract model results
pe.res2_no4299 <- fixed.effects(lme.res2_no4299)        # Point estimates of fixed effects
vc.res2_no4299 <- vcov(lme.res2_no4299)                 # Var-cov matrix of fixed effects estimates
se.res2_no4299 <- sqrt(diag(vc.res2_no4299))            # std erros of fixed effects estimates
re.res2_no4299 <- random.effects(lme.res2_no4299)       # "Estimated" random effects by group 
ll.res2_no4299 <- logLik(lme.res2_no4299)               # Log-likelihood at maximum
resid.res2_no4299 <- resid(lme.res2_no4299)             # Residuals
aic.res2_no4299 <- AIC(lme.res2_no4299)                 # Akaike Information Criterion

summary(lme.res2_no4299)

# p,d,q AIC
# Total cum incidence plus its interaction with nsahd. Excluding any trips to or from Freetown. 1,0,2. AIC = 34970.3. The effect of nsahds was simlar, at a 46% decrease in travel (as opposed to a 53% decrease including Freetown). The cumulative incidence still wasn't significant on its own, but for each additional 100 cases in either the origin or destination chiefdom, the intervention reduced travel by an additional 17%, which is twice as influential as when we Freetown
# Consider adding a variable for (distance between chiefdoms) as an effect modifier. Did closer trips get more affected by nsahds?

#### Random effects model, excluding travel within chiefdoms ####

search()
detach(df_ARIMA_select)
df_ARIMA_select_nosame <- df_ARIMA_select[(df_ARIMA_select$same_chiefdom == 0),]
attach(df_ARIMA_select_nosame)
names(df_ARIMA_select_nosame)

# Include ebola incidence metrics
# Estimate a random effects AR(I)MA(p,q) model using lme (Restricted ML)
lme.res2_nosame <- lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  fixed = log(count+1) ~ as.factor(day_of_week) + nsahd + admin3_total_cum_incidence*nsahd + lockdown_saturdays + operation_northern_push_current + operation_northern_push_previous +  operation_northern_push_current*operation_northern_push_previous,			# i.e. response variable and explanatory variables 
  
  # The random effects component
  random = ~ 1 | pair_code,						# 1 indicates the intercept and COUNTRY indicates the grouping
  
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ date | pair_code,
                        p = 1,  # AR(p) order
                        q = 2   # MA(q) order
  ) 
)

# Extract model results
pe.res2_nosame <- fixed.effects(lme.res2_nosame)        # Point estimates of fixed effects
vc.res2_nosame <- vcov(lme.res2_nosame)                 # Var-cov matrix of fixed effects estimates
se.res2_nosame <- sqrt(diag(vc.res2_nosame))            # std erros of fixed effects estimates
re.res2_nosame <- random.effects(lme.res2_nosame)       # "Estimated" random effects by group 
ll.res2_nosame <- logLik(lme.res2_nosame)               # Log-likelihood at maximum
resid.res2_nosame <- resid(lme.res2_nosame)             # Residuals
aic.res2_nosame <- AIC(lme.res2_nosame)                 # Akaike Information Criterion

plot(exp(pe.res2_nosame[2:7]), type = "l", main = "Fixed effect for day of week\n(Ref: Sunday)", xlab = "Day of week", ylab = "Effect Size")
hist(resid.res2_nosame, breaks = 100)
summary(lme.res2_nosame)

# p,d,q AIC
# AIC = 39469.5. Same patterns for day_of_week. nsahd 54% reduction in travel, and an extra 7% reduction per 100 cases. 1% increase in travel per 100 cases. 1% increase in travel during lockdown saturdays. If the destination or origin were in operation northern push, there was a decrease in travel.

#### Mixed effects model ####

search()
detach(df_ARIMA_select_no4299)
attach(df_ARIMA_select)

#Mixed Effects Models
# Estimate a mixed effects AR(I)MA(p,q) model using lme (Restricted ML)
lme.resME1 <- lme(# A formula object including the response,
  # the fixed covariates, and the country fixed effects
  # (either as dummy variables or as a "factor" variable),
  # then remove the model intercept with - 1
  fixed = log(count+1) ~ nsahd + fe - 1 ,
  # NOTE:  I must drop OIL, which doesn't vary over
  #        time for any country.
  #        If I leave it in, I get a singularity error
  
  # The random effects component
  random = ~ 1 | pair_code,
  
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ date | pair_code,
                        p = 1,  # AR(p) order
                        q = 2   # MA(q) order
  ) 
)

# p,d,q

# Extract model results
pe.resME1 <- fixed.effects(lme.resME1)        # Point estimates of fixed effects
vc.resME1 <- vcov(lme.resME1)                 # Var-cov matrix of fixed effects estimates
se.resME1 <- sqrt(diag(vc.resME1))            # std erros of fixed effects estimates
re.resME1 <- random.effects(lme.resME1)       # "Estimated" random effects by group 
ll.resME1 <- logLik(lme.resME1)               # Log-likelihood at maximum
resid.resME1 <- resid(lme.resME1)             # Residuals
aic.resME1 <- AIC(lme.resME1)                 # Akaike Information Criterion

summary(lme.resME1)

#### Detect Anomalies ####
# https://blog.twitter.com/2015/introducing-practical-and-robust-anomaly-detection-in-a-time-series
library(devtools)
# devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

# help(AnomalyDetectionTs)

#examples
# data(raw_data)
# res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
# res$plot
# AnomalyDetectionVec(raw_data[,2], max_anoms=0.02, period=1440, direction='both', only_last=FALSE, plot=TRUE)
# res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', only_last="day", plot=TRUE)
# res$plot

#travel data
ch_curr <- 4299
ch_prev <- 4199

extract <- df_ARIMA[df_ARIMA$admin3_current == ch_curr & df_ARIMA$admin3_previous == ch_prev,c("date", "count")]
extract$date <- as.POSIXct(extract$date)
# extract$count <- log(extract$count + 1)
extract <- extract[extract$date >= as.POSIXct("2015-03-20"),]

res = AnomalyDetectionTs(extract,max_anoms = 0.02, direction = 'both', plot = TRUE)
res$plot
res$anoms

# Record anomalies
neg_anom_data <- data.frame(matrix(NA, nrow = 1, ncol = 2))
names(neg_anom_data) <- c("day", "pair_code")
pos_anom_data <- neg_anom_data

# Set a minimum date. Repeat method where min date is after NSAHDs to see if this explains the possitive anomalies
min_date <- "2015-03-20"
min_date <- "2015-03-30"

for (pair in pairs_cumtrips_min1000){
  extract <- df_ARIMA[df_ARIMA$pair == pair,c("date", "count")]
  extract$date <- as.POSIXct(extract$date)
  extract <- extract[extract$date >= as.POSIXct(min_date),]
  
  neg_res = AnomalyDetectionTs(extract, max_anoms = 0.02, direction = 'neg', plot = FALSE)
  
  if (length(neg_res$anoms) > 0){
    neg_anom_data <- rbind(neg_anom_data, data.frame("day" = as.character(c(neg_res$anoms[1])$timestamp), "pair_code" = rep(pair, length(neg_res$anoms[1]))))
  }
  
  pos_res = AnomalyDetectionTs(extract, max_anoms = 0.02, direction = 'pos', plot = FALSE)
  
  if (length(pos_res$anoms) > 0){
    pos_anom_data <- rbind(pos_anom_data, data.frame("day" = as.character(c(pos_res$anoms[1])$timestamp), "pair_code" = rep(pair, length(pos_res$anoms[1]))))
  }
  cat(".")
}
neg_anom_data <- neg_anom_data[is.na(neg_anom_data$day)==0,]
pos_anom_data <- pos_anom_data[is.na(pos_anom_data$day)==0,]

neg_anom_data_long <- dcast(neg_anom_data, formula = day ~ . , length)
names(neg_anom_data_long)[2] <- "count"
neg_anom_data_long$day <- as.POSIXct(neg_anom_data_long$day)
neg_anom_data_long$direction <- "Negative"

pos_anom_data_long <- dcast(pos_anom_data, formula = day ~ . , length)
names(pos_anom_data_long)[2] <- "count"
pos_anom_data_long$day <- as.POSIXct(pos_anom_data_long$day)
pos_anom_data_long$direction <- "Positive"

anom_data_long <- rbind(neg_anom_data_long,pos_anom_data_long )

ggplot() +
  theme_classic() +
  geom_bar(data = neg_anom_data_long, aes(x = day, y = count), stat = "identity", fill = "lightblue") +
  geom_bar(data = pos_anom_data_long, aes(x = day, y = count), stat = "identity", fill = "pink") +
  facet_grid(direction~., space = "free", scales = "free_y") +
  ggtitle("Anomalies Detected")
