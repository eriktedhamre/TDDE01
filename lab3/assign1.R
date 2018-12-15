library("geosphere")
setwd("~/TDDE01/lab3/")
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "Latin1")
temps <- read.csv("temps50k.csv")
station.numbers <- unique(subset(stations, select = c(station_number)))
gaussian_kernel <-function(u){
  return(exp(-(u^2)))
}

distance_kernel <- function(station, target){
  station.long.lat <- c(stations[station_number == station, 'longitude'], stations[station_number == station, 'latitude'])
  distance <- distHaversine(station.long.lat, target)
  
  return(gaussian_kernel(distance)/dist_h)
}

time_kernel <- function(m.time, target){
  time1 <- as.difftime(m.time,"%H:%M:%S",units="hour")
  time2 <- as.difftime(target,"%H:%M:%S",units="hour")
  time.diff <- difftime(time1 = time1, time2 = time2) 
}

st <- merge(stations,temps,by="station_number")

dist_h <- 350000 
#date_h <-
#time_h <-
distance <- distHaversine(c(55.3836, 12.8203), target.pos)
    
target.longitude <- 58.4274 # The point to predict (up to the students)
target.latitude <- 14.826
target.pos <- c(target.longitude, target.latitude)

target.time <- "04:00:00"
target.date <- "2013-11-04" # The date to predict (up to the students)
station.time <- "14:00:00"
station.date <- "2014-10-04"
target.posix <- as.POSIXct(paste(target.date, target.time))
station.posix <- as.POSIXct(paste(station.date, station.time))
time.diff <- as.double.difftime(difftime(time1 = target.posix, time2 = station.posix, units = "days"))
times <- c("04:00:00", "06:00:00", ..., "24:00:00")
temp <- vector(length=length(times))

# Students’ code here
plot(temp, type="o")
