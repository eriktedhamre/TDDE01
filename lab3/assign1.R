library("geosphere")
setwd("~/TDDE01/lab3/")
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "Latin1")
temps <- read.csv("temps50k.csv")

gaussian_kernel <-function(u){
  return(exp(-(u^2))
}

distance_kernel <-function(station, target){
  station.long.lat <- c(stations[stations$station_number == station, 'longitude'], stations[stations$station_number == station, 'latitude'])
  distance <- distHaversine(station.long.lat, target, interest)
  
  return(gaussian_kernel(distance))
}

st <- merge(stations,temps,by="station_number")

distance_kernel <-
date_kernel <-
time_kernel <-
  
target.longitude <- 58.4274 # The point to predict (up to the students)
target.latitude <- 14.826
target.pos <- c(target.longitude, target.latitude)

date <- "2013-11-04" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", ..., "24:00:00")
temp <- vector(length=length(times))

# Students’ code here
plot(temp, type="o")
