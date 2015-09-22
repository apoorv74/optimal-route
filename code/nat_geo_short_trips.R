

#Declaring the libraries
library(rvest)
library(stringr)
library(ggmap)
library(leaflet)
library(TSP)


#Part 1 - Pulling the Data
url_stable <- 'http://www.natgeotraveller.in/magazine/section/short-breaks/'
pages <- c('','page2','page3','page4','page5','page6')
xpath_place_stable_1 <- '//*[@id="body"]/div/div'
xpath_place_stable_2 <- '/div[2]/h4'
xpath_tag_stable_1 <- '//*[@id="body"]/div/div'
xpath_tag_stable_2 <- '/div[2]/div[1]/p'
xpath_link_stable_1 <- '//*[@id="body"]/div/div'
xpath_link_stable_2 <-  '/div[2]/h4/a' 
place <- data.frame()
tag <- data.frame()
link <- data.frame()

for(i in 1:6)
{
  url <- paste0(url_stable,pages[i])
  for(j in 2:11)
  {
    xpath_place = paste0(xpath_place_stable_1,'[',j,']',xpath_place_stable_2)
    xpath_tag = paste0(xpath_tag_stable_1,'[',j,']',xpath_tag_stable_2)
    xpath_link = paste0(xpath_link_stable_1,'[',j,']',xpath_link_stable_2)
    data_place <- url %>% html() %>% html_nodes(xpath = xpath_place ) %>% html_text()
    data_tag <- url %>% html() %>% html_nodes(xpath = xpath_tag ) %>% html_text()
    data_link <- url %>% html() %>% html_nodes(xpath = xpath_link ) %>% html_attrs()
    place <- c(place,data_place)
    tag <- c(tag,data_tag)
    link <- c(link,paste0('http://www.natgeotraveller.in',data_link))
  }
}

place_df <- data.frame(unlist(place))
tag_df <- data.frame(unlist(tag))
link_df <- data.frame(unlist(link))
link_df <- data.frame(link_df[str_length(link_df$unlist.link.) > 29,])
short_breaks_df <- data.frame(place_df,tag_df,link_df)
names(short_breaks_df)[] <- c('Place','Tag','Link')
short_breaks_df$id <- 1:nrow(short_breaks_df)

#Getting Lat and Long of the places mentioned by NatGeo
coord_master <- data.frame()
for(i in 1:nrow(short_breaks_df)){
  coord <- as.numeric(geocode(as.character(short_breaks_df[i,1]),source = 'google'))
  coord_master <- rbind(coord_master,coord)
}

#Filtering for Indian Bounds
coord_master[is.na(coord_master)] <- 0
names(coord_master)[] <- c('lon','lat')
coord_master$id <- 1:nrow(coord_master)
coord_master <- coord_master[(coord_master$lon >= 68 & coord_master$lon <= 94) & (coord_master$lat >= 8 & coord_master$lat <= 38),]

#Joining with the short_breaks dataset
short_breaks_df <- left_join(short_breaks_df,coord_master,by=NULL)

#Manual entry for remaining Lat Longs
write.csv(short_breaks_df,'short_breaks_df.csv',row.names=F)

#Reading the completed file
short_breaks <- read.csv('short_breaks_df.csv')


#Visualisaing the pulled data - Getting ti know more about the destinations
#Adding Markers to the Indian map
natgeo_map <- leaflet(data = short_breaks) %>% addTiles() %>% addMarkers(~lon,~lat,popup = ~Place)

#Processing for getting an optimal route to cover all the destinations
#Algorithm implemented in Python
#Using expand.grid to get all combinations of places listed in the map
all_comb <- expand.grid(short_breaks$id,short_breaks$id)
names(all_comb)[] <- c('origin','destination')
co_ordinates <- short_breaks[,c(4:6)]
all_comb <- left_join(all_comb,co_ordinates,by=c('origin' = 'id'))
names(all_comb)[3:4] <- c('source_lon','source_lat')
all_comb <- left_join(all_comb,co_ordinates,by=c('destination' = 'id'))
names(all_comb)[5:6] <- c('destination_lon','destination_lat')

options(scipen = 20)
attach(all_comb)
dlon = (destination_lon - source_lon) * pi/180
dlat = (destination_lat - source_lat) * pi/180
a = (sin(dlat/2))^2 + cos(source_lat*pi/180) * cos(destination_lat*pi/180) * (sin(dlon/2))^2 
c = 2 * atan2( sqrt(a), sqrt(1-a) ) 
d = 6373 * c 
detach(all_comb)
#Joining it with the all_comb data frame
all_comb$distance <- d
write.csv(all_comb,'distance.csv',row.names=F)

#Getting source, destination and distance and saving it to our local
sd_distance <- all_comb[,c(1,2,7)]
write.csv(sd_distance,'sd_distance.csv',row.names=F)

#optimised route from genetic_route.py
route <- data.frame(id = c(40.0, 55.0, 2.0, 25.0, 37.0, 38.0, 46.0, 16.0, 31.0, 1.0, 44.0, 14.0, 12.0, 
           22.0, 35.0, 43.0, 48.0, 53.0, 20.0, 5.0, 51.0, 6.0, 41.0, 29.0, 39.0, 49.0, 
           45.0, 50.0, 21.0, 9.0, 17.0, 34.0, 8.0, 52.0, 32.0, 18.0, 4.0, 15.0, 13.0, 
           24.0, 23.0, 26.0, 42.0, 3.0, 7.0, 33.0, 19.0, 10.0, 28.0, 54.0, 47.0, 30.0, 
           36.0, 27.0, 11.0))

#Left join with co_ordonates to get the lat long of points
route <- left_join(route,co_ordinates,by=NULL)

address <- data.frame()
for(i in 1:nrow(route)){
address_i <- revgeocode(as.numeric(route[i,c(2,3)]))
address <- c(address,address_i)
}

address_df <- data.frame(unlist(address))
names(address_df)[] <- 'Spot_name'
address_df$Spot_name <- as.character(address_df$Spot_name) 
#Resolving address not mapped by google
address_df[3,'Spot_name'] <-'Villa Himalaya, Gund'
address_df[4,'Spot_name'] <- 'The Khyber Himalayan Resort & Spa'

#Writing the final destinations to the file
optimal_route <- left_join(route,short_breaks_df,by='id')
write.csv(optimal_route,'optimal_route.csv',row.names=F)
#Visualising the route generated

route_graph <- c(t(address_df))
route_graph <- paste0("'",route_graph,"'")
route_graph <- paste(route_graph,collapse = ",")


