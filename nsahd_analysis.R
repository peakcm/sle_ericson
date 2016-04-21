#### National Stay at Home Days analysis ####

# install.packages("plyr")
# require(plyr)
# install.packages("ggplot2")
# library(ggplot2)
library(fastmatch)
library(data.table)
library(magrittr)

#### Load .csv data ####
# Load data from the "nsahd_crossover_windows.py" processing code
mch20_22 <- read.csv("~/processed_data/2015/NSAHD_c_1.csv")
mch27_29 <- read.csv("~/processed_data/2015/NSAHD_e_2.csv") #takes about 100 seconds
apr03_05 <- read.csv("~/processed_data/2015/NSAHD_c_2.csv")

# Select window
data <- mch20_22
window_start <- as.POSIXct("2015-03-20 06:00:00")
window_end <- as.POSIXct("2015-03-22 18:00:00")
rm("mch20_22")

data <- mch27_29
window_start <- as.POSIXct("2015-03-27 06:00:00")
window_end <- as.POSIXct("2015-03-29 18:00:00")
rm("mch27_29")

data <- apr03_05
window_start <- as.POSIXct("2015-04-03 06:00:00")
window_end <- as.POSIXct("2015-04-05 18:00:00")
rm("apr03_05")

# Import data on inter-tower distance
towers_dist <- read.csv("~/towers_dist.csv")
View(towers_dist)
row.names(towers_dist) <- towers_dist[,1]
towers_dist <- towers_dist[,2:ncol(towers_dist)]
names(towers_dist) <- row.names(towers_dist)
View(towers_dist)

#### Reformat .csv data ####
# Describe data
names(data) <- c("event","user_id", "time", "cell_id")
head(data)
tail(data)
nrow(data)

# Convert times
data$time <- as.POSIXct(strptime(as.character(data$time), format = "%y%m%d%H%M%S"))
data <- data[order(data$time),] #Reorder by time
head(data, n = 15) #Note if the observations start before the time window
tail(data, n = 15) #Note if the observations go beyond the time window
nrow(data)
data <- data[data$time >= window_start & data$time <= window_end,] #Restrict to window
nrow(data)
head(data, n = 5)
tail(data, n = 15)
sum(is.na(data$time))
data <- data[is.na(data$user_id)==0,] #Drop missing users (same as dropping missing times or cell_id)
tail(data, n = 5)

hours <- seq(window_start, window_end, by = 60*60)
hours_of_day <- rep(c(seq(6, 24), seq(0,5)), times = ceiling(length(hours)/24))[1:length(hours)]
calls <- rep(NA, length(hours))

for (j in 1:(length(hours)-1)){
  first <- head(which(data$time >= hours[j]), n=1)
  last <- tail(which(data$time < (hours[j+1])), n=1)
  calls[j] <- last - first + 1
  cat(j,"\n")
}

# Convert user_ids
summary(nchar(as.character(data$user_id))) # 44 characters is too long. the last 22 are universal.
data$user_id <- strtrim(as.character(data$user_id), 22) # Shorten to first 22
data$user_id <- as.factor(data$user_id)

# Convert cell_ids
data$cell_id <- as.factor(data$cell_id)

# Drop "event" column
data <- data[,c("user_id", "time", "cell_id")]

#### Create data_trim that drops call events from misbehaving towers ####
towers_located_repeats_concerning <- as.numeric(c( "181" , "221" ,"222" ,"223",  "281"  ,"282",  "283",  "311",  "312"  
                                                   ,"313"  ,"391"  ,"392"  ,"393",  
                                                   "481","482",  "483"  ,"791"  ,"792",  "793",  "1401", "1402",
                                                   "1403", "1661", "1662", "1663" ,"1941","1942" ,"1943")  )
towers_located_wrongly_geocoded <- c("3651", "3652", "3653", "237", "238", "239")

towers_bad <- c(towers_located_repeats_concerning, towers_located_wrongly_geocoded)
data_trim <- data[which((data$cell_id %in% towers_bad)==0),]
data <- data_trim

#### For towers with multiple listings to essentially the same location ####
towers_located_repeats_not_concerning # Defined below
to_drop <- c("11b","12b", "13b", "31b", "32b", "33b", "151b", "152b", "153b", "397b", "3661b", "3662b", "3663b", "3678b", "3678c")
towers_dist <- towers_dist[(row.names(towers_dist) %in% to_drop)==0, (names(towers_dist) %in% to_drop)==0 ]
# Note the order below. it was done manually and must match names()
to_rename <- c("3661a","3662a", "3663a", "11a", "12a", "13a", "31a", "32a", "33a", "151a", "152a", "153a", "3678a", "397a")
names(towers_dist)[which(names(towers_dist) %in% to_rename)] == to_rename
names(towers_dist)[which(names(towers_dist) %in% to_rename)] <- c("3661","3662", "3663", "11", "12", "13", "31", "32", "33", "151", "152", "153", "3678", "397")
names(towers_dist)[which(names(towers_dist) %in% c("3661","3662", "3663", "11", "12", "13", "31", "32", "33", "151", "152", "153", "3678", "397"))]
row.names(towers_dist)[which(row.names(towers_dist) %in% to_rename)] == to_rename
row.names(towers_dist)[which(row.names(towers_dist) %in% to_rename)] <- c("3661","3662", "3663", "11", "12", "13", "31", "32", "33", "151", "152", "153", "3678", "397")
row.names(towers_dist)[which(row.names(towers_dist) %in% c("3661","3662", "3663", "11", "12", "13", "31", "32", "33", "151", "152", "153", "3678", "397"))]

#### or: Load previously processed data from R workspace ####
load("/home/coreypeak/processed_data/2015/NSAHD_Analysis_c1-2_e2.RData")

#### Create new dataset with the format: user_id, freq, cell_ids ####
data_trim <- data_trim[order(data_trim$user_id, data_trim$time),] #sort by user_id, then time
write.csv(data_trim, "~/processed_data/2015/data_trim_events_c_2.csv", row.names = FALSE, quote = FALSE) # write the file so python can read it
# run compile.py script 
# Save the output file as, for example, calls_by_user_c_1.csv
data_trim_users_DT_mch20_22 <- as.data.table(read.csv("~/processed_data/2015/calls_by_user_c_1.csv"))
data_trim_users_DT_mch27_29 <- as.data.table(read.csv("~/processed_data/2015/calls_by_user_e_2.csv"))
data_trim_users_DT_apr03_05 <- as.data.table(read.csv("~/processed_data/2015/calls_by_user_c_2.csv"))

data_trim_users_DT_mch20_22$cell_ids <- strsplit(as.character(data_trim_users_DT_mch20_22$cell_ids), split = "|", fixed = TRUE)
data_trim_users_DT_mch27_29$cell_ids <- strsplit(as.character(data_trim_users_DT_mch27_29$cell_ids), split = "|", fixed = TRUE)
data_trim_users_DT_apr03_05$cell_ids <- strsplit(as.character(data_trim_users_DT_apr03_05$cell_ids), split = "|", fixed = TRUE)

### Choose which days and trimmed/non-trimmed data you'd like to use ####
days <- "mch20_22"
trim <- FALSE
data <- data_mch20_22
data_users_DT <- data_users_DT_mch20_22

days <- "mch20_22"
trim <- TRUE
data <- data_trim
data_users_DT <- data_trim_users_DT_mch20_22

days <- "mch27_29"
trim <- FALSE
data <- data_mch27_29
data_users_DT <- data_users_DT_mch27_29

days <- "mch27_29"
trim <- TRUE
data <- data_trim
data_users_DT <- data_trim_users_DT_mch27_29

days <- "apr03_05"
trim <- FALSE
data <- data_apr03_05
data_users_DT <- data_users_DT_apr03_05

days <- "apr03_05"
trim <- TRUE
data <- data_trim_apr03_05
data_users_DT <- data_trim_users_DT_apr03_05

#### Create towers_dist_corrected that adjusts abberant towers ####
# Set all distances from towers 3651, 3652, 3653 to the same as 3791, 3792, 3793
# Note that in data_trim, calls managed by towers 3651, 3652, and 3653 are just dropped
towers_dist_corrected <- towers_dist
towers_dist_corrected[c("3651", "3652", "3653"),] <- towers_dist_corrected[c("3791", "3792", "3793"),]
towers_dist_corrected[,c("3651", "3652", "3653")] <- towers_dist_corrected[,c("3791", "3792", "3793")]
towers_dist_corrected[c("3651", "3652", "3653"),c("3791", "3792", "3793")]
towers_dist_corrected[c("3791", "3792", "3793"),c("3651", "3652", "3653")]
sum(towers_dist_corrected[c("3791", "3792", "3793"),] != towers_dist_corrected[c("3651", "3652", "3653"),])
sum(towers_dist_corrected[,c("3791", "3792", "3793")] != towers_dist_corrected[,c("3651", "3652", "3653")])

#### Calculate tower distances and stationarity ####

# Functions to calculate distance traveled. This, or fcn_distance_pairs is currently a slow function
fcn_distance <- function(input_list, towers, threshold = 0){
  # cat(class(input_list))
  vector <- unlist(input_list)
  # cat(class(vector))
  if (length(unique(vector)) == 1){
    distance <- 0
  } else {
    # cat(length(vector))
    distance <- sum(sapply(2:length(vector), function(x) fcn_distance_pairs(vector[x], vector[x-1], towers, threshold)))
  }
  return(distance)
}

fcn_distance_pairs <- function(x1, x2, towers, threshold = 0){
  missing = 0
  row.names <- row.names(towers)
  tower_1 = c()
  tower_1_rows = c()
  if (x1 %in% row.names){
    tower_1 <- x1
  } else if (paste(x1,"a",sep="") %in% row.names){
    tower_1_rows <- which(row.names %in% c(paste(x1,"a",sep=""), paste(x1,"b",sep="")))
  } else {missing = 1}
  
  tower_2 = c()
  tower_2_rows = c()
  if (x2 %in% row.names(towers)){
    tower_2 <- x2
  } else if (paste(x2,"a",sep="") %in% row.names){
    tower_2_rows <- which(row.names %in% c(paste(x2,"a",sep=""), paste(x2,"b",sep="")))
  } else {missing = 1}
  
  if (missing == 1){
    out <- 0
  } else if (length(tower_1) == 1 & length(tower_2) == 1){  # If we have a single location for both towers 1 and 2
    out <- towers[which(row.names==as.character(x1)), as.character(x2)]
  } else if (length(tower_1_rows) > 0 & length(tower_2) == 1){  # If we have a single location for only tower 2
    out <- min(towers[tower_1_rows, as.character(x2)])
  } else if (length(tower_1) == 1 & length(tower_2_rows) > 0){  # If we ahve a single location for only tower 1
    out <- min(towers[tower_2_rows, as.character(x1)])
  } else if (length(tower_1_rows) > 0 & length(tower_2_rows) > 0){  # If we have more than one location for each tower 1 and 2
    out <- min(towers[tower_1_rows, tower_2_rows])
  }
  
  if (threshold != 0 & out < threshold){out <- 0}
  
  return(out)
}

# Calculate distance traveled
head(data_users_DT)
n = nrow(data_users_DT)
# n = 1000
data_users_DT$distance <- 999 # This missing indicator is needed in order to keep this field numeric
data_users_DT$distance[1:n] <- as.numeric(sapply(data_users_DT$cell_ids[1:n], function(x) fcn_distance(x, towers_dist))) # This step is slow (~1hr)
data_users_DT[data_users_DT$distance == 999,]$distance <- NA # replace missing indicator with NA

data_users_DT$distance_thresh_3 <- 999 # missing indicator, needed to keep this vector numeric
data_users_DT$distance_thresh_3[1:n] <- as.numeric(sapply(data_users_DT$cell_ids[1:n], function(x) fcn_distance(x, towers_dist, threshold = 3)))  # This step is slow
data_users_DT[data_users_DT$distance_thresh_3 == 999,]$distance_thresh_3 <- NA # replace missing indicator with NA

data_users_DT$distance_thresh_10 <- 999 # missing indicator, needed to keep this vector numeric
data_users_DT$distance_thresh_10[1:n] <- as.numeric(sapply(data_users_DT$cell_ids[1:n], function(x) fcn_distance(x, towers_dist, threshold = 10))) # This step is slow
data_users_DT[data_users_DT$distance_thresh_10 == 999,]$distance_thresh_10 <- NA # replace missing indicator with NA

# data_users_DT$distance_corrected <- 999 # This missing indicator is needed in order to keep this field numeric
# data_users_DT$distance_corrected[1:n] <- as.numeric(sapply(data_users_DT$cell_ids[1:n], function(x) fcn_distance(x, towers_dist_corrected))) # This step is slow
# data_users_DT[data_users_DT$distance_corrected == 999,]$distance_corrected <- NA # replace missing indicator with NA

# Calculate stationarity
data_users_DT$stationary_same_tower <- NA #Calcualte those who use the same tower again.
# data_users_DT[length(unique(unlist(data_users_DT$cell_ids)))]$stationary_same_tower <- TRUE
# data_users_DT[data_users_DT$distance != 0,]$stationary_same_tower <- FALSE

data_users_DT$stationary_thresh_0 <- NA
data_users_DT[data_users_DT$distance == 0,]$stationary_thresh_0 <- TRUE
data_users_DT[data_users_DT$distance != 0,]$stationary_thresh_0 <- FALSE

data_users_DT$stationary_thresh_3 <- NA
data_users_DT[data_users_DT$distance_thresh_3 == 0,]$stationary_thresh_3 <- TRUE
data_users_DT[data_users_DT$distance_thresh_3 != 0,]$stationary_thresh_3 <- FALSE

data_users_DT$stationary_thresh_10 <- NA
data_users_DT[data_users_DT$distance_thresh_10 == 0,]$stationary_thresh_10 <- TRUE
data_users_DT[data_users_DT$distance_thresh_10 != 0,]$stationary_thresh_10 <- FALSE

#### Quality Checks: Towers ####
# Create a dataset that tracks suspcious tower transitions
towers_errors <- data.frame(matrix(rep(0, nrow(towers_dist)^2), nrow = nrow(towers_dist)))
names(towers_errors) <- names(towers_dist)
row.names(towers_errors) <- row.names(towers_dist)

# Look for super travelers
head(data_users_DT[data_users_DT$distance > 1000,], 10)
id <- data_users_DT[data_users_DT$distance > 5000,]$user_id[5]

head(data_users_DT[data_users_DT$distance_thresh_3 > 1000,], 10)
id <- data_users_DT[data_users_DT$distance_thresh_3 > 10000,]$user_id[10]

head(data_users_DT[data_users_DT$distance_thresh_3 < 10000 & data_users_DT$distance_thresh_3 > 5000,], 10)
id <- data_users_DT[data_users_DT$distance_thresh_3 < 10000 & data_users_DT$distance_thresh_3 > 5000,]$user_id[1]

head(data_users_DT[data_users_DT$distance_corrected > 10000,], 10)
id <- data_users_DT[data_users_DT$distance_corrected > 5000,]$user_id[9]

vector <- as.numeric(unlist(data_users_DT[data_users_DT$user_id == as.character(id),]$cell_ids))
distances <- sapply(2:length(vector), function(x) fcn_distance_pairs(vector[x], vector[x-1], towers_dist))
# distances_corrected <- sapply(2:length(vector), function(x) fcn_distance_pairs(vector[x], vector[x-1], towers_dist_corrected))
plot(distances, as.character(vector[2:length(vector)]))
plot(distances)
weird <- 200 #set the threshold where travel above this is odd.
for (x in which(distances > weird)){
  cat(vector[x], vector[x + 1], "\n")
  towers_errors[which(row.names(towers_errors) == vector[x]), as.character(vector[x+1])] <- 1 + towers_errors[which(row.names(towers_errors) == vector[x]), as.character(vector[x+1])]
}

# Look at margins of the towers_errors data.frame to detect repeat offenders
towers_errors_right_margin <- apply(towers_errors, MARGIN = 2, sum)
towers_errors_left_margin <- apply(towers_errors, MARGIN = 1, sum)
plot(towers_errors_left_margin, towers_errors_right_margin)
plot(towers_errors_left_margin)
names(towers_errors)[towers_errors_left_margin > 20]
 # Towers 1102, 3793, 3651, 3652 are the main culprits so far, looking at the top 10 "mega travelers" with over 10,000 km traveled in 3 days
   # Towers 3651 and 3652 are the same tower in Western Area
   # Tower 3793 and 1102 are extremely close in the South (Bo Town)
tower <- 3652
row.names(towers_errors)[which(towers_errors[row.names(towers_errors) == tower,]>0)]
names(towers_errors)[towers_errors[,as.character(tower)]>0]
tower <- 3651
row.names(towers_errors)[which(towers_errors[row.names(towers_errors) == tower,]>0)]
names(towers_errors)[towers_errors[,as.character(tower)]>0]
  # 881, 1753, 313, and 3791 are all in the South, suggesting that 3652 and 3651 should be as well.
tower <- 1102
row.names(towers_errors)[which(towers_errors[row.names(towers_errors) == tower,]>0)]
names(towers_errors)[towers_errors[,as.character(tower)]>0]
tower <- 3793
row.names(towers_errors)[which(towers_errors[row.names(towers_errors) == tower,]>0)]
names(towers_errors)[towers_errors[,as.character(tower)]>0]
  # All the errors for 1102 and 3793 are to 3651-3, suggesting that 3651-3 are out of place
  # Consider re-locating towers 3651, 3652, 3653 to the location of 3793, the most common previous and subsequent tower
  # Also consider dropping towers 3651, 3652, and 3653
    # Option 1: Remove the towers from each cell_id list
    # Option 2: Set the distance from the towers to all other towers to 0
      # Option 2 could potentially overestimate stationarity, because excluded towers could make people eligible (>2 calls) but they can't move.
 # From the travellers with 5000 to 10000
  # Tower 791, 793, and 799 are also problematic. 791 and 793 are in both Southern and in Western Area. 799 is just in Western
tower <- 791
row.names(towers_errors)[which(towers_errors[row.names(towers_errors) == tower,]>0)]
names(towers_errors)[towers_errors[,as.character(tower)]>0]
# the towers it goes to are all in Western Area

tower <- 239
row.names(towers_errors)[which(towers_errors[row.names(towers_errors) == tower,]>0)]
names(towers_errors)[towers_errors[,as.character(tower)]>0]
# 239 appears to be mislocated. Lots of trips to the East, but it's in the north.

# For each tower, see where their most common tower that is immediately before, immediately after, and both
fcn_add_trip_count <- function(neighbors, towers_df, tower){
  table <- as.data.frame(table(neighbors))
  matches <- fmatch(table$neighbors, names(towers_df))
  trips <- table[is.na(matches)==0,"Freq"]
  matches <- matches[is.na(matches)==0]
  towers_df[matches, tower] <- trips + towers_df[matches, tower]
  return(towers_df)
}
  
towers_neighbor_next <- towers_dist
towers_neighbor_next[towers_neighbor_next > 0] <- 0

towers_neighbor_prev <- towers_neighbor_next
towers_neighbor_next_and_prev <- towers_neighbor_next

towers_seq <- unlist(data_users_DT$cell_ids)

for (tower in names(towers_neighbor_next)){
  appearances <- which(towers_seq == tower)
  if (length(appearances) > 10){ 
    appearances <- appearances[2:(length(appearances)-1)] #Drop the first and the last, just to make sure they're not the boundaries
    neighbors_next <- towers_seq[appearances - 1]
    neighbors_prev <- towers_seq[appearances + 1]
    neighbors_next_and_prev <- neighbors_next[neighbors_next == neighbors_prev]
    
    towers_neighbor_next <- fcn_add_trip_count(neighbors_next, towers_neighbor_next, tower)
    towers_neighbor_prev <- fcn_add_trip_count(neighbors_prev, towers_neighbor_prev, tower)
    towers_neighbor_next_and_prev <- fcn_add_trip_count(neighbors_next_and_prev, towers_neighbor_next_and_prev, tower)
  }
}

suspect_towers <- as.character(c(212, 3871, 3872, 3873, 3711, 3712, 821, 822, 823, 237, 238, 239)) # Most concerning is 237-239 though

min_mean <- 0
# for (tower in names(towers_neighbor_next)){
for (tower in suspect_towers){
  if (sum(as.numeric(towers_neighbor_next_and_prev[tower,])) > 1){
    mean_distance <- round(sum(as.numeric(towers_neighbor_next_and_prev[tower,]) * as.numeric(towers_dist[tower,]))/length(as.numeric(towers_neighbor_next_and_prev[tower,])>0),2)
    if (mean_distance > min_mean){
      plot(as.numeric(towers_dist[tower,]), log10(as.numeric(towers_neighbor_next_and_prev[tower,])),
           main = paste("Frequency of each tower being immediately\nbefore and after tower number",tower),
           ylab = "Log 10 Count", xlab = "Distance (km)", 
           ylim = c(0, 5), xlim = c(0, 350))
      text(x = 300, y = 4, paste("The mean distance\nis", mean_distance))
      cat ("Press [enter] to continue")
      line <- readline()
    }
  }
}
    # Stopped looking at each at tower 199
sort(towers_neighbor_next_and_prev["237",]) #238, 1931N, 91N, 1933N
  # 238 is in North. 3711, 3712, 212 are very close together in East. 92 is the exact same GPS as 238, but I think it's correctly located given it's "neighbors"
sort(towers_neighbor_next_and_prev["238",]) #237, 239, 3711E, 3712E, 92N
# 238 is in North. 3711, 3712, 212 are very close together in East. 92 is the exact same GPS as 238, but I think it's correctly located given it's "neighbors"
sort(towers_neighbor_next_and_prev["239",]) #238, 93N, 3873E, 3871E, 237, 3872E
# 238 is in North. 3711, 3712, 212 are very close together in East. 92 is the exact same GPS as 238, but I think it's correctly located given it's "neighbors"

# Try to detect if 3651, 3652, and 3653 really should be in Bo Town
  # Hypothesis: Most of the travel from these towers will be to the nearest tower.
  # Find the most frequent towers that are used right before and after 3651, 3652, and 3653
fcn_find_tower <- function(input_list, tower){
  length(grep(tower, input_list[[1]])) > 0
}

users_of_3712 <- unlist(lapply(data_users_DT$cell_ids, function(x) fcn_find_tower(x, "3712")))
sequence_3712 <- unlist(data_users_DT$cell_ids[users_of_3712])
previous_towers <- c()
next_towers <- c()
for (i in 2:(length(sequence_3712)-1)){
  if (sequence_3712[i] == "3712"){
    previous_towers <- c(previous_towers, sequence_3712[i-1])
    next_towers <- c(next_towers, sequence_3712[i+1])
  }
}
table(previous_towers[previous_towers != "3712"])
# Tower 3651 goes mostly to tower 1102 (Bo Town)
# Tower 3652 goes mostly to tower 1102, 3791, 3793 (Bo Town)
# Tower 3652 goes mostly to tower 881, 1102 (Bo)
# Towers 3712 and 238 seemed to have some weird behavior. 3712 is based in the East and does most of it's transitions there. 238 does transitions from the North to the East with different towers
fcn_distance_pairs(881,1102, towers_dist)
fcn_distance_pairs(3791,1102, towers_dist)
fcn_distance_pairs(3791,881, towers_dist) 
  #3791 is right in the middle. Set locations of 3651-3 to 3791-3
# Conclusion: Set all tower distances from location 3651 to essentially be from 3791, etc.

#### Explore results ####
# Towers used
towers_used <- as.character(unique(data$cell_id))
length(towers_used)
hist <- hist(as.numeric(data$cell_id), breaks = 600)
plot(sort(hist$counts), ylab = "counts")

towers_located <- names(towers_dist)
length(towers_located)

towers_located_repeats <- as.numeric(c("11"  , "12",   "13",   "31",   "32",   "33",   "151",  "152",  "153" , "181" , "221" ,
                            "222" ,"223",  "281"  ,"282",  "283",  "311",  "312"  ,"313"  ,"391"  ,"392"  ,"393",  
                            "397",  "481","482",  "483"  ,"791"  ,"792",  "793",  "1401", "1402",
                            "1403", "1661", "1662", "1663" ,"1941","1942" ,"1943", "3661", "3662", "3663", "3678")  )
#The following set is above as well
towers_located_repeats_concerning <- as.numeric(c( "181" , "221" ,"222" ,"223",  "281"  ,"282",  "283",  "311",  "312"  
                                                   ,"313"  ,"391"  ,"392"  ,"393",  
                                                   "481","482",  "483"  ,"791"  ,"792",  "793",  "1401", "1402",
                                                   "1403", "1661", "1662", "1663" ,"1941","1942" ,"1943")  )
towers_located_repeats_not_concerning <- towers_located_repeats[(towers_located_repeats %in% towers_located_repeats_concerning)==0]
  # Option 1: drop all towers with multiple locations. Those only account for <2% of the towers used
  # Option 2: when calculating distances, always use the minimum. So, if they go from Bo Town 799 to Unknown 791, we will assume they used the Bo Town 791, not the WA 791
    # Need to add 791a and 791b. When looking up distances, flag any tower that has multiple and then grep those columns and choose the minimum distance
    # use minimum funciton on both sides, so we can potentially underestimate travel distances
sum((as.character(data$cell_id) %in% towers_located_repeats_concerning)==1)
length(data[which(as.character(data$cell_id) %in% towers_located_repeats_concerning),"cell_id"]) / nrow(data)
# The concerning towers (the ones with multiple locations) only account for 5% of the towers used. 
sum((as.character(data_trim$cell_id) %in% towers_located_repeats_concerning)==1)
sum((as.character(data_trim$cell_id) %in% towers_located_repeats_concerning)==1) / nrow(data_trim)

sort((towers_used[which((towers_used %in% towers_located)==0)]))
# towers 1, 2, 3, and 399 are used by the CDR but aren't located in the geocoded towers list

sort((towers_located[which((towers_located %in% towers_used)==0)]))
# Towers located but not used: 18, 107, 108, 109, 217, 218, 219, 338, 358, 377, 378, 379, 488, 2191, 2193, 2321, 2322, 2323, 3673
sort((towers_located[which((towers_located %in% towers_used)==0)]))

# Number of calls placed during window
cat("There are", nrow(data), "calls/texts during the window")
hist(log10(data_users_DT$freq), breaks = 100, main = "Histogram of number of calls per user (log10)")

# Plot of calls placed by the hour
plot(hours, calls)
plot(hours_of_day, calls)

# Number of users who placed at least 1 call
cat("There were", nrow(data_users_DT), "unique users in window, placing an average of", round(nrow(data)/nrow(data_users_DT),2), "calls/texts")

# Number of users who placed at least 2 calls
cat(nrow(data_users_DT[data_users_DT$freq > 1,]), "of these users placed at least 2 calls in this window")
cat("And", nrow(data_users_DT[data_users_DT$freq > 1000,]), "of these users placed more than 1000 calls in this window")

# Plots of distance traveled
hist(log10(data_users_DT[data_users_DT$freq > 1,]$distance), main = "Histogram of Log10 distance traveled", xlab = "Log10 Distance (km)", xlim = c(-5, 5))
hist(log10(data_users_DT[data_users_DT$freq > 1,]$distance_thresh_3), main = "Histogram of Log10 distance traveled (3km threshold)", xlab = "Log10 Distance (km)",  xlim = c(-5, 5))
hist(log10(data_users_DT[data_users_DT$freq > 1,]$distance_thresh_10), main = "Histogram of Log10 distance traveled (10km threshold)", xlab = "Log10 Distance (km) (3km threshold)",  xlim = c(-5, 5))

# Fraction of users for whom all calls were managed by the same tower
min_calls = 2

fcn_fraction_stationary <- function(df, min_calls){

  a0 <- nrow(df[df$stationary_thresh_0 == TRUE & df$freq >= min_calls,])
  b0 <- nrow(df[df$stationary_thresh_0 == FALSE & df$freq >= min_calls,])
  c0 <- a0/(a0+b0)
  d0 <- summary(df[df$freq >= min_calls,]$distance)["Mean"]
  e0 <- summary(df[df$freq >= min_calls,]$distance)["Median"]
  
  a3 <- nrow(df[df$stationary_thresh_3 == TRUE & df$freq >= min_calls,])
  b3 <- nrow(df[df$stationary_thresh_3 == FALSE & df$freq >= min_calls,])
  c3 <- a3/(a3+b3)
  d3 <- summary(df[df$freq >= min_calls,]$distance_thresh_3)["Mean"]
  e3 <- summary(df[df$freq >= min_calls,]$distance_thresh_3)["Median"]
  
  a10 <- nrow(df[df$stationary_thresh_10 == TRUE & df$freq >= min_calls,])
  b10 <- nrow(df[df$stationary_thresh_10 == FALSE & df$freq >= min_calls,])
  c10 <- a10/(a10+b10)
  d10 <- summary(df[df$freq >= min_calls,]$distance_thresh_10)["Mean"]
  e10 <- summary(df[df$freq >= min_calls,]$distance_thresh_10)["Median"]
  
  thresholds = 3
  output <- data.frame(matrix(nrow = thresholds, ncol = 6))
  names(output) <- c("threshold_distance","n_stationary", "n_mobile", "frac_stationary", "mean_distance", "median_distance")
  output[1,] <- c(0, a0, b0, c0, d0, e0)
  output[2,] <- c(3, a3, b3, c3, d3, e3)
  output[3,] <- c(10, a10, b10, c10, d10, e10)
  
  return(output)
}

fcn_fraction_stationary(data_users_DT, min_calls = 2)

# Sensitivity analysis: Does the minimum number of calls influence results?
fcn_fraction_stationary(data_users_DT, min_calls = 3)
fcn_fraction_stationary(data_users_DT, min_calls = 4)

# Sensitivity analysis: Downsample number of calls during control periods to match exposure period
fcn_sens_downsample_calls <- function(df, downsample_frac){
  df$cell_ids_downsample <- df$cell_ids
  
  df_2 <- df[df$freq == 2,]
  df_3 <- df[df$freq == 3,]
  df_4 <- df[df$freq == 4,]
  df_5plus <- df[df$freq >= 5,]
  
  # Among those who called twice, only keep those who placed both calls
  df_2_new <- df_2[sample(nrow(df_2), size = round(nrow(df_2)*(downsample_frac^2))),] 
  
  # Among those who called three times, keep those who placed at least two calls...
  df_3_new <- df_3[sample(nrow(df_3), size = round(nrow(df_3)*(downsample_frac^3 + 3*(downsample_frac^2 * (1-downsample_frac))))),]
  df_3_3_new <- df_3_new[sample(nrow(df_3_new), size = round(nrow(df_3)*(downsample_frac^3))),] # Those who will continue to place all three calls
  df_3_2_new <- subset(df_3_new, !(df_3_new$user_id %in% df_3_3_new$user_id)) # Those who will now only place two calls
  # ... and then drop some from 3 calls to 2 calls
  df_3_2_new[is.na(df_3_2_new$cell_ids)==0,]$cell_ids_downsample <- lapply(df_3_2_new[is.na(df_3_2_new$cell_ids)==0]$cell_ids, function(x) sample(x, size = 2))
  df_3_new <- rbind(df_3_3_new, df_3_2_new)
  
  # Among those who called four times, keep those who placed at least two calls...
  df_4_new <- df_4[sample(nrow(df_4), size = round(nrow(df_4)*(downsample_frac^4 + 4*(downsample_frac^3 * (1-downsample_frac)) + 6*(downsample_frac^2 * (1-downsample_frac)^2)))),]
  df_4_4_new <- df_4_new[sample(nrow(df_4_new), size = round(nrow(df_4)*(downsample_frac^4))),] # Those who will continue to place all four calls
  df_4_3or2_new <- subset(df_4_new, !(df_4_new$user_id %in% df_4_4_new$user_id)) # Those who will have 2 or 3 calls now
  # ... for those 4-callers who will become 3-callers or 2-callers
  if (nrow(df_4_3or2_new) != 0){  # Only meaningful if we actually have people losing calls. This applies when downsample_frac != 1
    pr_3vs2 <- (4*downsample_frac^3*(1-downsample_frac)) / (4*downsample_frac^3*(1-downsample_frac) + 6*downsample_frac^2*(1-downsample_frac)^2) # 4 options for 4choose3 and 6 options for 4choose2
    df_4_3_new <- df_4_3or2_new[sample(nrow(df_4_3or2_new), size = round(nrow(df_4_3or2_new)* pr_3vs2)),] # Among those who had one or two calls dropped, these are the ones who only had one dropped
    df_4_2_new <- subset(df_4_3or2_new, !(df_4_3or2_new$user_id %in% df_4_3_new$user_id))
    # ... drop the chosen subset of 4-callers to 3-callers
    df_4_3_new[is.na(df_4_3_new$cell_ids)==0]$cell_ids_downsample <- lapply(df_4_3_new[is.na(df_4_3_new$cell_ids)==0]$cell_ids, function(x) sample(x, size = 3))
    # ... drop the chosen subset of 4-callers to 2-callers
    df_4_2_new[is.na(df_4_2_new$cell_ids)==0]$cell_ids_downsample <- lapply(df_4_2_new[is.na(df_4_2_new$cell_ids)==0]$cell_ids, function(x) sample(x, size = 2))
    df_4_new <- rbind(df_4_4_new, df_4_3_new, df_4_2_new)
  }
  
  # Among those who called more than 5 times, we can use the sample function directly, but we still will have some error due to the rounding
  df_5plus[is.na(df_5plus$cell_ids)==0,]$cell_ids_downsample  <- lapply(df_5plus[is.na(df_5plus$cell_ids)==0]$cell_ids, function(x) sample(x, size = round(length(x)*downsample_frac)))
  
  df <- rbind(df_2_new, df_3_new, df_4_new, df_5plus)
  df <- df[is.na(df$cell_ids_downsample)==0,]
  return(df)
}

# If we set downsample_frac = 1, this should give the same output as 
# fcn_fraction_stationary(data_users_DT, min_calls = 2)
# downsample_frac_mch20_22 <- 5622017/7259921
# downsample_frac_apr03_05 <- 5622017/7663672
# 
# data_users_DT %>%
#   fcn_sens_downsample_calls(downsample_frac = downsample_frac_apr03_05) %>%
#   fcn_fraction_stationary(min_calls = 2, downsample = TRUE)

# Test differences in fraction stationary
fisher.test(matrix(c(447796, 32673, 439959, 92308), ncol=2)) # c(stationary in E, mobile in E, stationary in c1, mobile in c1)
fisher.test(matrix(c(18488, 27402, 8930, 23260), ncol=2)) # c(stationary in E, mobile in E, stationary in c2, mobile in c2)

fisher.test(matrix(c(13663, 37247, 14084, 23260), ncol=2)) # c(stationary in c1, mobile in c1, stationary in c2, mobile in c2)

# Fraction of users for whom all calls were managed by towers within THRESHOLD distance of the first tower

#### Store datasets in R environment ####

# data_mch20_22 <- data
# data_users_DT_mch20_22 <- data_users_DT
# data_trim_mch20_22 <- data
# data_trim_users_DT_mch20_22 <- data_users_DT

# data_mch27_29 <- data
# data_users_DT_mch27_29 <- data_users_DT
# data_trim_mch27_29 <- data
# data_trim_users_DT_mch27_29 <- data_users_DT

# data_apr03_05 <- data
# data_users_DT_apr03_05 <- data_users_DT
# data_trim_apr03_05 <- data
# data_trim_users_DT_apr03_05 <- data_users_DT

#### Test functions ####
# Test the time conversion
test <- head(data)
names(test) <- c("event","user_id", "time", "cell_id")
test[grep("X",test$time),]
test[1,]
test$time <- as.POSIXct(strptime(as.character(test$time), format = "%y%m%d%H%M%S"))
test$time > as.POSIXct("2015-03-27 15:00:00") # Pick an interesting time point
class(test$time)
test <- test[order(test$time), ]

# Experiment with measuring number of calls
test <- head(data)
test
sum(test$user_id == "M/IvAFAeBGZVqx8mQvnXB7TqXoSWwJr0RIByaqfImSg=")
test$user_appearances <- NA
test <- rbind(test, test[1,])
test[7,"cell_id"] <- test[2,"cell_id"]
test$user_appearances <- sapply(test$user_id, function(x) sum(test$user_id == as.character(x)))

# Experiment with measuring number of towers used
test$towers <- sapply(test$user_id, function(x) length(unique(test[test$user_id == as.character(x), "cell_id"])))
test$towers <- sapply(test$user_id, function(x) fcn_list_cell_ids_visited(df = test, user_id = x))

# Test fcn_list_cell_ids_visited function
out <- fcn_list_cell_ids_visited(test, "M/IvAFAeBGZVqx8mQvnXB7TqXoSWwJr0RIByaqfImSg="); out
class(out)

#### Save workspace ####
# save.image("~/processed_data/2015/NSAHD_Analysis_c1-2_e2.RData")
