
library(dplyr)
library(magrittr)
library(lubridate)
library(markovchain)


setwd("C:/Users/Deren/Desktop/graduation_project")

df <- read.csv("data/spotify_original_data.csv") %>%  mutate(week=week(Date))

genres <- read.csv("data/data_with_artist_genres.csv") %>% rename(Genre=Artist_Genres) %>% distinct(Artist, .keep_all = TRUE)


distinct_data <- df %>% distinct(Track.Name, .keep_all = TRUE) %>% 
  select(Track.Name,Artist) %>%
  left_join(genres, by="Artist")

data <- aggregate(Streams ~ Track.Name+Region+week, data=df, FUN = sum) %>% 
  group_by(week,Region) %>%filter(Streams == max(Streams)) %>% 
  left_join(distinct_data, by="Track.Name") %>% ungroup()



data_base_country <- data %>% filter(Region=="global")
data_base_country[is.na(data_base_country)] <- "pop"
data_base_country %<>% mutate(Iterative_Genre ="")

for(i in 1:nrow(data_base_country)){
data_base_country$Iterative_Genre[i]<-data_base_country$Genre[i+1] %>% as.character()
if(i==nrow(data_base_country)){
  data_base_country$Iterative_Genre[i]<-data_base_country$Genre[1] %>% as.character()
}}

data_base_country %<>% mutate(Genre=as.character(Genre),Iterative_Genre=as.character(Iterative_Genre))



matrix <- table(data_base_country$Genre,data_base_country$Iterative_Genre) %>% as.matrix()

class(matrix) <- "matrix"

matrix <- matrix/rowSums(matrix) 

dtmcA <- new("markovchain",transitionMatrix=matrix,
             states=,
             name="MarkovChain A") #create the DTMC

plot(dtmcA)

data_base_country %<>% mutate(predicted_genre="") 

for(i in 1:nrow(data_base_country)){
  genre <- data_base_country$Genre[i]
  probs <- dtmcA[genre] %>% as.vector()
  predicted_genre <- sample(states(dtmcA), size=1,replace=TRUE,prob=probs)
  data_base_country$predicted_genre[i] <- predicted_genre
  
}

data_base_country %<>% mutate(flag=if_else(Iterative_Genre==predicted_genre,1,0)) %>% select(-Iterative_Genre)

sum(data_base_country$flag)/nrow(data_base_country)


