
library(httr)
library(magrittr)
library(rjson)
library(dplyr)
library(stringr)
library(tidyr)
library(xlsx)


setwd("C:/Users/Deren/Desktop/spotifydata_analysis")

df <- read.csv("data_with_artist_id.csv")

q<-df %>% distinct(Track_id)

b<-df %>% distinct(Artist_id)


get_artist_genres <- function(df){

n <- nrow(df)

track_url <- "https://api.spotify.com/v1/artists/"

b <- tribble(~Artist_Genres, ~Artist_Name)

json_parse <- function(data){
  
  text <- content(data, as="text", encoding = "UTF-8")
  if(identical(text,"")) warn ("No output to parse")
  fromJSON(text)
}

for(i in 1:n){
  
  get_data <- GET(url=paste0(track_url,df$Artist_id[i],"/" ),
                  add_headers(Authorization="Bearer BQDzTReNXNv2BtYg54bn3ysMotuYmb6G_oNqm__49OcK7n23oENjxgNdqKcMCBdOvg9rMF4i3GvJQXtcB8PlxRVkJQ5LpJKm6b9g8PTPiQIAuAVO_CzWAf6D7jRMxx9P9aNAPAvPFj8p_JQAkRVS6HtHc7Ct0LMY61CIkjoc87S_UQUbMtIToQxeV08FeMHRJi_Q0XILfSuk-2AYhTbZa0SJKQ1_Qwl6JFr_UMxugsQfIYtc3A7-ha0aafbJtr01UQU40HH_uaQs6swXUf1iBrREaj5RDKCdpRg"))
  
  a<-json_parse(get_data)$genres[1]
  
  if(length(a)==0){a="NA"}
  
  b %<>% add_row(Artist_Genres=a)
  print(a)
  print(i)
}

y <- cbind(df,b) %>% select(Artist,Artist_Genres)

return(y)

}

y$Artist_Genres <- vapply(y$Artist_Genres, paste, collapse = ", ", character(1L))
y$Artist_Name <- vapply(y$Artist_Name, paste, collapse = ", ", character(1L))

write.csv(y, "data_with_artist_genres.csv")



