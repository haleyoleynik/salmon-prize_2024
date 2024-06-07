library(tidyverse)

data_file <- "Bristol_Columbia_Fraser_combined.csv"

data <- read_csv(data_file)
data

ggplot(data, aes(x=BroodYear, y=Recruits, group=interaction(System, Stock)))+
    geom_line(aes(color=Stock))+
    coord_cartesian(expand=0)+
    facet_wrap(~System, scales="free_y")+
    theme_bw()

ggplot(data)+
    geom_point(aes(x=Escapement, y=Recruits, color=Stock))+
    coord_cartesian(expand=0)+
    facet_wrap(~System, scales="free")+
    theme_bw()

data %>% 
    pivot_longer(c(AgeClass_0.1:AgeClass_3.4), names_to = "AgeClass", values_to="Rec1") %>%
    mutate(Rec1 = ifelse(Rec1 < 1e-5, 0, Rec1)) %>%
    separate(AgeClass, into=c("name", "ac"), sep="_") %>%
    filter(!is.na(ac)) %>%
    separate(ac, into=c("fresh", "salt"), sep="\\.") %>%
    mutate(
        fresh = as.numeric(fresh),
        salt = as.numeric(salt),
        total=fresh+salt, 
        ageclass_id = as.factor(paste0(fresh, ".", salt))
    ) %>%

    ggplot(aes(x=BroodYear, y=Rec1, color=ageclass_id, group=interaction(Stock, ageclass_id))) +
        geom_line() +
        #facet_grid(rows=vars(System), cols=vars(Stock), scales="free_y")
        facet_wrap(~interaction(System, Stock), scales="free_y")

data %>% 
    pivot_longer(c(AgeClass_0.1:AgeClass_3.4), names_to = "AgeClass", values_to="Rec1") %>%
    mutate(Rec1 = ifelse(Rec1 < 1e-5, 0, Rec1)) %>%
    separate(AgeClass, into=c("name", "ac"), sep="_") %>%
    filter(!is.na(ac)) %>%
    separate(ac, into=c("fresh", "salt"), sep="\\.") %>%
    mutate(
        fresh = as.numeric(fresh),
        salt = as.numeric(salt),
        total=fresh+salt, 
        ageclass_id = as.factor(paste0(fresh, ".", salt))
    ) %>%
    filter(System=="Bristol_Bay", Rec1 != 0) %>%

    ggplot(aes(x=BroodYear, y=Rec1, color=ageclass_id, group=interaction(Stock, ageclass_id))) +
        geom_line() +
        facet_wrap(~Stock, scales="free_y")

data %>% 
    pivot_longer(c(AgeClass_0.1:AgeClass_3.4), names_to = "AgeClass", values_to="Rec1") %>%
    mutate(Rec1 = ifelse(Rec1 < 1e-5, 0, Rec1)) %>%
    separate(AgeClass, into=c("name", "ac"), sep="_") %>%
    filter(!is.na(ac)) %>%
    separate(ac, into=c("fresh", "salt"), sep="\\.") %>%
    mutate(
        fresh = as.numeric(fresh),
        salt = as.numeric(salt),
        total=fresh+salt, 
        ageclass_id = as.factor(paste0(fresh, ".", salt))
    ) %>%
    filter(!is.na(Recruits)) %>%
    mutate(
        system_stock = paste0(System, ".", Stock),
        rec_prop = Rec1/Recruits
    ) %>%
    select(system_stock, BroodYear, ageclass_id, rec_prop) %>%
    group_by(system_stock, ageclass_id) %>%
    summarise(rec_prop=mean(rec_prop, na.rm=TRUE)) %>%
    mutate(rec_prop=ifelse(rec_prop < 1e-2, 0, rec_prop)) %>%
    pivot_wider(names_from="ageclass_id", values_from="rec_prop") %>%
    write_csv(file="ageclass_props.csv")

