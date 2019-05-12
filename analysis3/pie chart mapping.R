
library(maptools)
library(raster)
library(ggplot2)
library(rgdal)
library(ggmap)
library(scatterpie)
library(tidyr)
library(magrittr)
library(dplyr)


setwd("C:/Users/Deren/Desktop/graduation_project")

df <- read.csv("data/spotify_languages.csv") %>% dplyr::select(-X,-Region_Language)

artist_genres <- read.csv("data/data_with_artist_genres.csv") %>% distinct(Artist, .keep_all = TRUE)

df %<>% left_join(artist_genres, by="Artist")

coordinates <- read.csv("data/coordinates.csv")
continents <- read.csv("data/countryContinent.csv")

df%<>% left_join(coordinates, by = c('Region_Name' = 'name')) %>%
  mutate(Artist_Genres=if_else(grepl("pop",Artist_Genres),"pop", as.character(Artist_Genres)) ) %>%
  mutate(Artist_Genres=if_else(grepl("rap",Artist_Genres),"rap-hiphop", as.character(Artist_Genres)) ) %>% 
  mutate(Artist_Genres=if_else(grepl("hiphop",Artist_Genres),"rap-hiphop", as.character(Artist_Genres)) ) %>% 
  mutate(Artist_Genres=if_else(grepl("rock",Artist_Genres),"rock", as.character(Artist_Genres)) ) %>% 
  mutate(Streams=as.numeric(Streams)) %>% group_by(Region_Name) %>%
  mutate(total=sum(Streams)) %>% ungroup() %>% 
  group_by(Region_Name,Artist_Genres) %>% 
  mutate(GenreRatio=sum(Streams)/total) %>% 
  distinct(GenreRatio, .keep_all = TRUE) %>%
  dplyr::select(Region_Name,latitude,longitude,Artist_Genres,GenreRatio) %>% 
  arrange(Region_Name,desc(GenreRatio)) %>% 
  filter(Artist_Genres!="") %>% ungroup() 


worldmap <- map_data("world")
cc <- ccodes()

mappings <- c("UK"="United Kingdom", "USA"="United States")

cc$NAME[match(mappings, cc$NAME)] <- names(mappings)

worldmap <- left_join(worldmap, cc[,c("NAME","continent")], by=c("region"="NAME"))

data  <- df %>% tidyr::spread(key=Artist_Genres, value=GenreRatio) %>% 
  dplyr::select(Region_Name,longitude,latitude, `rap-hiphop`,latin,pop,rock ) %>% 
  distinct(pop, .keep_all = TRUE) %>% mutate(group=c(1:53)) %>% 
  mutate(group=as.factor(group)) %>% left_join(worldmap  %>% 
                                                 dplyr::select(region,continent), by=c("Region_Name"="region"))

continent <- worldmap %>% dplyr::filter(continent=="South America")

temp <- data %>% dplyr::filter(continent=="South America") %>% dplyr::select(-continent)

ggplot(continent,aes(x=long, y=lat, group=group) ) +
  geom_polygon(fill=NA, color="black")+
  geom_scatterpie(aes(x=longitude, y=latitude, group=Region_Name,  r=3.5), data = temp,
                  cols = c("rap-hiphop","latin","pop","rock"),color=NA)+
  labs(title = "South America", subtitle = "") + 
  theme()
