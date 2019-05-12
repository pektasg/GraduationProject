

setwd("C:/Users/Deren/Desktop/graduation_project")

library(magrittr)
library(lubridate)
library(xlsx)
library(stringr)
library(tidyr)
library(dplyr)

df <- read.csv("data/data.csv") %>% select(-X.1, -X)

language_code <- read.xlsx("data/country_to_language_code.xlsx",1) %>%
  lapply(gsub, pattern=c("?","NA","?"), replacement='') %>% as_tibble()

temp_df <- right_join(language_code,df,by=c("Region_Code"="Region")) %>% filter(Region_Code!="global")

language_code  %<>% separate_rows(Language_Code, convert = TRUE ) %>% arrange(Region_Name)

languages <- read.xlsx("data/language_code_to_language.xlsx",1)

join_language_features <- right_join(languages,language_code,by="Language_Code") %>% 
  group_by(Region_Name) %>% summarise(Language_Name = paste(Language_Name, collapse=", "))

df_languages <- right_join(join_language_features,temp_df, by="Region_Name") %>% 
  select(Date,Region_Name,Language_Name,Track.Name,Artist,Streams) %>% 
  arrange(Date,Region_Name) %>%
  rename(Track_Name=Track.Name, Region_Language=Language_Name)

write.csv(df_languages, "spotify_languages.csv")




