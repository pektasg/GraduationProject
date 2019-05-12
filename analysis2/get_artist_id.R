
library(magrittr)
library(rjson)
library(httr)
library(dplyr)
library(stringr)
library(tidyr)
library(xlsx)


setwd("C:/Users/Deren/Desktop/spotifydata_analysis")

data <- read.csv("data.csv")

data %<>% mutate(URL=sapply(strsplit(as.character(data$URL),"/"), "[", 5)) %>% rename(Track_id=URL)

data %<>% distinct(Track_id, .keep_all = TRUE)  



get_artist_id <- function(df){

n <- nrow(data)

track_url <- "https://api.spotify.com/v1/tracks/"

a <- array()

json_parse <- function(data){
  
  text <- content(data, as="text", encoding = "UTF-8")
  if(identical(text,"")) warn ("No output to parse")
  fromJSON(text)
}

for(i in 1:n){
  
get_data <- GET(url=paste0(track_url,data$Track_id[i],"/" ),
             add_headers(Authorization=" Bearer BQAjCPD1AO239hXHkV8-xKvo_GHol5yaqOFypGgYOAYFHiSkzel8mwGUQCmBJjKywrULDsDGtA9gDqqQY9Lj76ZRZli3tVyelQtm_JXp5_8CUiIZt0l1Qt3jo4pxdo3IQFa9hV-lqVcbPjVEm_qDoADgj9_r6gkX6cUv43oekl0os2bfmyvlRgyC-QRTt7z2uRrnamqLBxEAW1EF1gP670vEy7rWm23OEgpBxU9jU8g4n_AtlTtDZgW04pj8i4DBbe4Sf_aA2nQCmz603oBsfd2G9gueGK6QVgM"))

b<- json_parse(get_data %>% as.character())$artists[[1]]$id

if(length(b)==0){b="NA"}

a[i,1] <- b
print(i)
print(b)
}

a %<>%as_tibble() %>% rename(Artist_id=value)

x <- cbind(data, a)

x %<>%arrange(Region) %>% select(-Position) 

return(x)

}

z <- get_artist_id(data)

write.csv(z, "data_with_artist_id.csv")



