

library(magrittr)
library(chron)
library(tidyr)
library(xlsx)
library(broom)
library(stats)
library(dplyr)

setwd("C:/Users/Deren/Desktop/graduation_project")

genres <- read.csv("data/data_with_artist_genres.csv") %>% distinct(Artist, .keep_all = TRUE) 

df <- read.csv("data/data.csv")%>% rename(Track_Name=Track.Name)

data <- left_join(df,genres, by="Artist")

data %<>% select(Date,Region,Track_Name,Artist,Artist_Genres,Streams) %>% mutate(weekends=is.weekend(Date))

temp_data <- read.xlsx("data/country_to_language_code.xlsx",1) %>% rename(Region=Region_Code) %>% select(-Language_Code)

df<-left_join(data,temp_data,by="Region") %>% select(Date,Region_Name,Track_Name,Artist,Artist_Genres,Streams,weekends)

df %<>% mutate(flag= case_when(weekends==TRUE ~ "Weekends",weekends==FALSE ~ "Weekday"))

weekend_and_weekday_sum <- aggregate(Streams~Artist_Genres+flag+Region_Name,data=df, FUN = sum)

spread_weekend_and_weekday<- weekend_and_weekday_sum %>% spread(key=flag, value = Streams)

spread_weekend_and_weekday %<>% mutate(Weekday=if_else(is.na(Weekday),0, as.double(Weekday)),
                                       Weekends=if_else(is.na(Weekends),0,as.double(Weekends)) )

spread_weekend_and_weekday %<>% mutate(Weekday_new=Weekday/(Weekday+Weekends), Weekends= Weekends/(Weekday+Weekends) ) %>% 
  select(-Weekday) %>% rename(Weekday=Weekday_new) %>% arrange(Artist_Genres) %>% filter(Artist_Genres!="")


res <- t.test(spread_weekend_and_weekday$Weekday, spread_weekend_and_weekday$Weekends, paired = TRUE)
