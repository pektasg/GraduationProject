
library(dplyr)
library(magrittr)
library(ggmap)
library(maps)
library(tidyverse)
library(data.table)
library(viridis)
library(grid)
library(gridExtra)

setwd("C:/Users/Deren/Desktop/graduation_project")

df <- read.csv("data/spotify_languages.csv") %>% select(-X,-Region_Language) %>%  mutate(Region_Name=as.character(Region_Name))

artist_genres <- read.csv("data/data_with_artist_genres.csv") %>% distinct(Artist, .keep_all = TRUE)

df <- df %>% left_join(artist_genres, by="Artist") 


df %<>% mutate(Artist_Genres=if_else(grepl("pop",Artist_Genres),"pop", as.character(Artist_Genres)) ) %>%
  mutate(Artist_Genres=if_else(grepl("rap",Artist_Genres),"rap-hiphop", as.character(Artist_Genres)) ) %>% 
  mutate(Artist_Genres=if_else(grepl("hiphop",Artist_Genres),"rap-hiphop", as.character(Artist_Genres)) ) %>% 
  mutate(Artist_Genres=if_else(grepl("rock",Artist_Genres),"rock", as.character(Artist_Genres)) ) %>% 
  mutate(Streams=as.numeric(Streams)) %>% group_by(Region_Name) %>%
  mutate(total=sum(Streams)) %>% ungroup() %>% 
  group_by(Region_Name,Artist_Genres) %>% 
  mutate(GenreRatio=sum(Streams)/total) %>% 
  distinct(GenreRatio, .keep_all = TRUE) %>%
  filter(Artist_Genres!="") %>% select(Region_Name,Artist_Genres,GenreRatio) %>% ungroup() 


df$Region_Name<- recode(df$Region_Name,'United States of America' = 'USA','United Kingdom' = 'UK')



grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

world_map <- map_data("world")

for(i in c("pop","latin","rock", "rap-hiphop")) {
  
  data <- df %>% filter(Artist_Genres==i)
  
  worldmap_joined <- left_join(world_map, data, by = c('region' = 'Region_Name'))
  
  
  plot <- ggplot(data = worldmap_joined, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = GenreRatio)) + scale_fill_viridis(option = 'plasma') + 
    labs(title = i, subtitle = "") + 
    theme()
  
  if(i=="pop"||i=="latin"){k=1}else{k=2}
  if(i=="pop"||i=="rock"){j=1}else{j=2}
  print(plot ,vp = vplayout(k,j))
  
}
