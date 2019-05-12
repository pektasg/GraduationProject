

library(dplyr)
library(xlsx)
library(magrittr)

setwd("C:/Users/Deren/Desktop/graduation_project/data")

df <- read.csv("flagged_track_language.csv") %>% select(-"X",-"Unnamed..0")


df %<>% mutate(category=case_when(flag==1~ "same_language",
                                  flag==0 ~ "different_language"))

different_language_ratio <- aggregate(flag~ Region_Name+category,data=df ,FUN = length) %>% group_by(Region_Name) %>% 
  mutate(sum=sum(flag)) %>% 
  ungroup() %>% 
  filter(category=="different_language") %>% 
  group_by(Region_Name) %>% 
  mutate(different_language_ratio=flag/sum*100) %>% 
  select(Region_Name,different_language_ratio)


language_knowledge <- read.xlsx("language_knowledge.xlsx",1)  

data <- inner_join(different_language_ratio,language_knowledge,by="Region_Name")
cor(data$different_language_ratio,data$Language_Knowledge)

