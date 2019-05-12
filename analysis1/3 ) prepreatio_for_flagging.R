
library(dplyr)
library(xlsx)
library(magrittr)
library(tidyr)
library(purrr)

setwd("C:/Users/Deren/Desktop/spotifydata_analysis/data")

data <- read.csv("with_track_language.csv") %>% select(-X)

language_code_to_language <- read.xlsx("language_code_to_language.xlsx",1) %>%  rename(Track_Language=Language_Code)

data %<>%left_join(language_code_to_language, by="Track_Language") %>%
  select(-Track_Language) %>% rename(Track_Language=Language_Name) %>% filter(Track_Name!="") %>% 
  select(Date, Region_Name, Region_Language, Track_Name,Track_Language,Language_Score,Artist,Streams)

data1 <- data %>%
  mutate(Track_Language = sapply(Track_Language, toString)) %>% 
  mutate(Track_Language = sapply(strsplit(Track_Language, ","), "[", 1)) %>% 
  mutate(flag="")
  

write.csv(data1,"with_track_language.csv")





