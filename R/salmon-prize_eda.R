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

