# May 2024 - Salmon Prize EDA 
# Haley Oleynik & Josh Zahner 

require(tidyverse)
require(readr)
require(ggplot2)

# read data 
df <- read_csv("Bristol_Columbia_Fraser_combined.csv")

long.df <- df %>% 
  pivot_longer(cols = AgeClass_0.1:AgeClass_3.4, names_to = "Age", values_to = "Number")

# plot relative numbers of each age class 
long.df %>%
  group_by(Age) %>%
  summarise(Number = sum(Number,na.rm=T)) %>%
  arrange(Number) %>%
  ggplot() +
  geom_bar(aes(x=reorder(Age, Number),y=Number), stat = "identity") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# by system 
long.df %>%
  group_by(Age,System) %>%
  summarise(Number = sum(Number,na.rm=T)) %>%
  arrange(Number) %>%
  ggplot() +
  geom_bar(aes(x=reorder(Age, Number),y=Number), stat = "identity") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_grid(vars(System),scales = "free")

# plot numbers over time for top 4 age classes  - can't see much 
long.df %>%
  filter(Number > 0) %>%
  filter(Age %in% c("AgeClass_2.3", "AgeClass_1.2", "AgeClass_1.3", "AgeClass_2.2")) %>%
  ggplot(aes(x = BroodYear, y = Number, color = Age)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = F, alpha = 0.5)

# Calculate total returns 
try <- df %>% 
  rowwise() %>%
  mutate(Age1 = AgeClass_0.1,
         Age2 = sum(AgeClass_0.2, AgeClass_1.1,na.rm=T),
         Age3 = sum(AgeClass_0.3, AgeClass_1.2, AgeClass_2.1,na.rm=T),
         Age4 = sum(AgeClass_0.4, AgeClass_1.3, AgeClass_2.2, AgeClass3.1,na.rm=T),
         Age5 = sum(AgeClass_0.5, AgeClass_1.4, AgeClass_2.3, AgeClass_3.2,na.rm=T),
         Age6 = sum(AgeClass_1.5, AgeClass_2.4, AgeClass_3.3,na.rm=T),
         Age7 = AgeClass_3.4) %>%
  select(System, Stock,BroodYear,Age1,Age2,Age3,Age4,Age5,Age6,Age7) %>%
  group_by(Stock) %>%
  mutate(Age1 = lag(Age1, 1),
         Age2 = lag(Age2, 2),
         Age3 = lag(Age3, 3),
         Age4 = lag(Age4, 4),
         Age5 = lag(Age5, 5),
         Age6 = lag(Age6, 6),
         Age7 = lag(Age7, 7)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(TotalReturns = sum(Age1,Age2,Age3,Age4,Age5,Age6,Age7,na.rm=T))

