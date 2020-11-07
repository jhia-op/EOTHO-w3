library(dplyr)
library(leaflet)
library(shiny)
library(ggplot2)

# The project will only support London postcodes #

# Get London Restaurants
restaurants <- read.csv("./restaurants_raw.csv")
ldn_rest <- restaurants %>%
  filter(County == "Greater London")
write.csv(ldn_rest, "restaurants_ldn.csv")


##### Geocoding the addresses of london restaurants #####

# Geocoder options 
# ggmap - requires google API key, which means creating a billing account
# tidygeocoder - best for US, but uses OSM data as well - trying this one out
# nominatim - osm, Robin lovelace is a contributor

library(tidygeocoder)
ldn_rest$full_add <- paste(ldn_rest$Line.1, ldn_rest$Line.2, ldn_rest$Postcode, ldn_rest$Town, ldn_rest$County, sep = " ")

# Create geocoding loop

## 1st round of geocoding - geocoding only with address
# Empty dataframe to start
ldn_rest_ll <- data.frame(matrix(ncol = 9, nrow = 0))
col <- c("Name", "Line.1", "Line.2", "Town", "County", "Postcode", "full_add", "latitude", "longitude")
colnames(ldn_rest_ll) <- col

for (i in 1:nrow(ldn_rest)) {
  
  x <- ldn_rest[i, ] %>%
    geocode(full_add, method = 'osm', lat = latitude , long = longitude)
  ldn_rest_ll <- rbind(ldn_rest_ll, x)
  
} ## 1256 NAs

# Write csv for first round of geocoding - 1256 NAs
write.csv(ldn_rest_ll, "restaurants_ldn_geocoded.csv")
round1 <- na.omit(ldn_rest_ll) ## 5437 completed

# 2nd round of geocoding - try geocoding with name of restaurant + address
na <- ldn_rest_ll[is.na(ldn_rest_ll$longitude), 1:7]
na$full_add <- paste(na$Name, na$Line.2, na$Postcode, na$Town, na$County, sep = " ")

na_ll <-  data.frame(matrix(ncol = 9, nrow = 0))
colnames(na_ll) <- col

for (i in 1:nrow(na)) {
  
  x <- na[i, ] %>%
    geocode(full_add, method = 'osm', lat = latitude , long = longitude)
  na_ll <- rbind(na_ll, x)
  
} 

sum(is.na(na_ll$latitude)) ## 956 NAs
round2 <- na.omit(na_ll) ## 300 completed

comb <- rbind(round1, round2)
write.csv(comb, "restaurants_ldn_geocoded.csv")

# 3rd round of geocoding - try geocoding with all the information
na <- na_ll[is.na(na_ll$longitude), 1:7]
na$full_add <- paste(na$Name, na$Line.1, na$Line.2, na$Postcode, na$Town, na$County, sep = " ")
na_ll <-  data.frame(matrix(ncol = 9, nrow = 0))
colnames(na_ll) <- col

for (i in 1:nrow(na)) {
  
  x <- na[i, ] %>%
    geocode(full_add, method = 'osm', lat = latitude , long = longitude)
  na_ll <- rbind(na_ll, x)
  
} 


# Try to create a pop-up map
m <- leaflet(x) %>% 
  setView(lng = -0.128004, lat = 51.508148, zoom = 10) %>% ## Need to sort out the zoom based on the miles entered
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(~X, ~Y, popup = ~as.character(pcd), label = ~as.character(pcd))
m



