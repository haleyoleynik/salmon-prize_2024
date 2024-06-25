require(tidyverse)
require(readr)
require(ggplot2)
require(forecast)
require(broom)

# read data 
df <- read_csv("Bristol_Columbia_Fraser_combined.csv")

total_returns <- read_csv("updated_data/Bristol_Columbia_Fraser_Returns_combined.csv") %>%
  rowwise() %>%
  mutate(Age2 = AgeClass_0.1,
         Age3 = sum(AgeClass_0.2, AgeClass_1.1,na.rm=T),
         Age4 = sum(AgeClass_0.3, AgeClass_1.2, AgeClass_2.1,na.rm=T),
         Age5 = sum(AgeClass_0.4, AgeClass_1.3, AgeClass_2.2, AgeClass_3.1,na.rm=T),
         Age6 = sum(AgeClass_0.5, AgeClass_1.4, AgeClass_2.3, AgeClass_3.2,na.rm=T),
         Age7 = sum(AgeClass_1.5, AgeClass_2.4, AgeClass_3.3,na.rm=T),
         Age8 = AgeClass_3.4) %>%
  mutate(TotalReturns = sum(Age2,Age3,Age4,Age5,Age6,Age7,Age8, na.rm=T)) %>%
  select(System,River,ReturnYear,Age2,Age3,Age4,Age5,Age6,Age7,Age8,TotalReturns,Total_Returns)

## Calculate proportional returns by age
brood_table <- read_csv("updated_data/Bristol_Columbia_Fraser_BroodT_combined.csv") %>%
  mutate(
    Stock = case_when(River == "Late Stuart" ~ "LateStuart",
                       River == "Bonneville Lock & Dam" ~ "Bonneville",
                       TRUE ~ River)
  ) %>%
  group_by(River) %>%
  complete(BroodYear = 1948:2025) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(Age2 = AgeClass_0.1,
         Age3 = sum(AgeClass_0.2, AgeClass_1.1,na.rm=T),
         Age4 = sum(AgeClass_0.3, AgeClass_1.2, AgeClass_2.1,na.rm=T),
         Age5 = sum(AgeClass_0.4, AgeClass_1.3, AgeClass_2.2, AgeClass_3.1,na.rm=T),
         Age6 = sum(AgeClass_0.5, AgeClass_1.4, AgeClass_2.3, AgeClass_3.2,na.rm=T),
         Age7 = sum(AgeClass_1.5, AgeClass_2.4, AgeClass_3.3,na.rm=T),
         Age8 = AgeClass_3.4) %>%
  select(System,Stock,BroodYear,Age2,Age3,Age4,Age5,Age6,Age7,Age8) %>%
  rowwise() %>%
  mutate(total = sum(c_across(Age2:Age8),na.rm=T)) %>%
  mutate(
    prop2 = Age2 / total,
    prop3 = Age3 / total,
    prop4 = Age4 / total,
    prop5 = Age5 / total,
    prop6 = Age6 / total,
    prop7 = Age7 / total,
    prop8 = Age8 / total
  ) %>%
  group_by(Stock) %>%
  mutate(
    prop2 = lag(prop2, 2),
    prop3 = lag(prop3, 3),
    prop4 = lag(prop4, 4),
    prop5 = lag(prop5, 5),
    prop6 = lag(prop6, 6),
    prop7 = lag(prop7, 7),
    prop8 = lag(prop8, 8)
  ) %>%
  select(ReturnYear = BroodYear,prop2,prop3,prop4,prop5,prop6,prop7,prop8, total)

# Forecast proportions
stocks <- brood_table %>% filter(!is.na(Stock)) %>% pull(Stock) %>% unique
arima_objects <- vector(mode="list", length=length(stocks))
forecast_objects <- vector(mode="list", length=length(stocks))
i=1
for(st in stocks){
  par(mfrow=c(3, 3))
  
  stock_proportions <- brood_table %>% filter(Stock == st, if_any(prop2:prop7, ~ . != 0), !ReturnYear %in% c(2022, 2023, 2024, 2025))

  arima_age_objects <- vector(mode="list", length=6)
  forecast_age_objects <- vector(mode="list", length=6)
  for(age in 2:7){
    col_name <- paste0("prop",age)
    stock_prop <- stock_proportions %>% pull(col_name, name=ReturnYear) 
    arima_out <- auto.arima(stock_prop)
    f <- forecast(arima_out)
    # plot(f, main=paste0(st, ": Age ",age), ylim=c(0, 1))

    arima_age_objects[[age-1]] <- arima_out
    forecast_age_objects[[age-1]] <- f

  }
  names(arima_age_objects) <- paste0("Age",2:7)
  names(forecast_age_objects) <- paste0("Age",2:7)
  arima_objects[[i]] <- arima_age_objects
  forecast_objects[[i]] <- forecast_age_objects
  i = i+1

}

names(arima_objects) <- stocks
names(forecast_objects) <- stocks

proportions_forecast_df <- data.frame(lapply(
  forecast_objects,
  function(s){
    data.frame(lapply(
      s,
      function(x){
        as.data.frame(x)[,1]
      }
    )) 
  }
)) %>% as_tibble() %>%
  pivot_longer(everything(), names_to="stock_age", values_to="prop") %>%
  separate(stock_age, into=c("stock", "age"), "\\.",  extra="merge") %>%
  mutate(foreyear=rep(rep(c(1:10), each=(length(stocks))*6))) %>%
  pivot_wider(names_from="age", values_from="prop") %>%
  arrange(stock, foreyear) %>%
  replace_na(replace=list(0))

proportions_forecast_df %>% pivot_longer(Age2:Age7, names_to="age", values_to="prop") %>%
  ggplot() +
    geom_line(aes(x=foreyear, y=prop, color=age))+
    scale_y_continuous(limits=c(0, 1))+
    theme_bw()+
    facet_wrap(~stock)


# Make ln(R/S) time series 
lnrs <- df %>%
  select(System, Stock, BroodYear, Spawners = Escapement, Recruits) %>%
  mutate(lnrs = log(Recruits/Spawners)) %>%
  filter(!is.infinite(lnrs)) %>%
  select(-Recruits)

ricker.params <- df %>% 
  select(Stock, BroodYear, Escapement, Recruits) %>%
  mutate(lnrs = log(Recruits/Escapement)) %>%
  filter(!is.infinite(lnrs)) %>%
  nest_by(Stock) %>% 
  mutate(model = list(lm(lnrs ~ Escapement, data, na.action = na.omit))) %>% 
  summarise(broom::tidy(model)) %>% 
  ungroup()


ricker.params <- read_csv("ricker_params.csv")
lnrs_residuals <- lnrs %>%
  mutate(Stock = case_when(Stock == "Late Stuart" ~ "Late_Stuart",
                   TRUE ~ Stock)) %>%
  left_join(ricker.params, by = "Stock") %>%
  mutate(Stock = case_when(Stock == "Late_Stuart" ~ "LateStuart",
                   TRUE ~ Stock)) %>%
  mutate(wt = ifelse(is.na(lnrs), NA, lnrs-(a-b*Spawners)))
#   group_by(Stock)
#   mutate(pred_rec = Spawners*exp(a-b*Spawners+wt)) 


stocks <- lnrs_residuals %>% pull(Stock) %>% unique
arima_objects <- vector(mode="list", length=length(stocks))
forecast_objects <- vector(mode="list", length=length(stocks))
i=1

par(mfrow=c(4, 4))
for(st in stocks){
  
  stock_proportions <- lnrs_residuals %>% filter(Stock == st, !is.na(wt))

  stock_prop <- stock_proportions %>% pull(wt, name=BroodYear) 
  arima_out <- auto.arima(stock_prop)
  f <- forecast(arima_out)
  plot(f, main=st, ylim=c(-4, 4))

  arima_objects[[i]] <- arima_out
  forecast_objects[[i]] <- f
  i = i+1

}

names(arima_objects) <- stocks
names(forecast_objects) <- stocks

residual_forecast_df <- data.frame(lapply(
  forecast_objects,
  function(s){
    data.frame(s)[,1] 
  }
)) %>% as_tibble() %>%
  pivot_longer(everything(), names_to="Stock", values_to="pred_res") %>%
  mutate(System = case_when(Stock %in% c("Chilko", "LateStuart", "Quesnel", "Raft", "Stellako") ~ "Fraser",
                            Stock %in% c("Bonneville") ~ "Columbia",
                            TRUE ~ "Bristol")
  ) %>%
  mutate(foreyear=rep(rep(c(1:10), each=(length(stocks))))) %>%
  mutate(foreyear = case_when(System == "Bristol" ~ foreyear+2015,
                              System == "Columbia" ~ foreyear+2017,
                              System == "Fraser" ~ foreyear+2019)) %>%
#   pivot_wider(names_from="Stock", values_from="pred_res") %>%
  arrange(foreyear) %>%
  replace_na(replace=list(0))


lnrs_residuals <- lnrs_residuals %>% left_join(
    residual_forecast_df %>% select(Stock, pred_res, foreyear),
    by = c("Stock"="Stock", "BroodYear"="foreyear")
 ) %>%
 rowwise() %>%
 mutate(
    pred_res = ifelse(is.na(pred_res), 0, pred_res),
    wt = ifelse(is.na(wt), 0, wt),
    wt = sum(c_across(c("wt", "pred_res")))
 ) %>%
 select(-pred_res) %>%
 mutate(pred_rec = Spawners*exp(a-b*Spawners+wt))

# Predict 2024
props_summary <- proportions_forecast_df %>% 
  filter(foreyear == 1) %>% 
  select(stock, Age2:Age7) %>%
  replace_na(list(Age1=0, Age2=0, Age3=0, Age4=0, Age5=0, Age6=0, Age7=0)) %>%
  mutate(across(Age2:Age7, ~ ifelse(.x < 0, 0, .x)))

pred_2024 <- lnrs_residuals %>%
    group_by(Stock) %>%
    filter(BroodYear > max(BroodYear)-7) %>%
    reframe(
        st = Stock,
        return_2024 = sum(pred_rec * rev(props %>% filter(stock == st) %>% select(Age2:Age7) %>% as.matrix))
    ) %>%
    distinct() %>%
    mutate(return_2024 = ifelse(Stock == "Bonneville", 561941.5, return_2024)) %>%
    mutate(Stock = ifelse(Stock == "Bonneville", "Bonneville Lock & Dam", Stock)) %>%
    print(n=100)

total_returns %>% 
    mutate(Stock = case_when(River == "Late Stuart" ~ "LateStuart",
           TRUE ~ River)) %>%

ggplot()+
  geom_line(aes(x=ReturnYear, y=Total_Returns)) +
  geom_point(data=pred_2024, aes(x=2024, y = return_2024), color="blue", size=4) + 
#   scale_x_continuous(limits=c(2015, 2025), breaks=seq(2015, 2025, 3))+
  facet_wrap(~Stock, scales="free_y")+
  theme_bw()

total_returns %>% filter(River == "Bonneville Lock & Dam") %>%
    pull(TotalReturns) %>%
    auto.arima %>%
    forecast
