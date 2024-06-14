# May 2024 - Salmon Prize EDA 
# Haley Oleynik & Josh Zahner 

require(tidyverse)
require(readr)
require(ggplot2)
require(forecast)

# read data 
df <- read_csv("Bristol_Columbia_Fraser_combined.csv")

# Make ln(R/S) time series 
lnrs <- df %>%
  select(System, Stock, BroodYear, Spawners = Escapement, Recruits) %>%
  mutate(lnrs = log(Recruits/Spawners))

# plot stocks through time 
ggplot(lnrs, aes(x=BroodYear, y=lnrs)) + 
  geom_line() +
  facet_wrap(vars(Stock)) +
  theme_classic()

# long format to calculate returns 
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

###
# USE THIS !!!!! calculate total returns and add years onto end of time series 
total_returns <- df %>%
  group_by(Stock) %>%
  complete(BroodYear = 1948:2025) %>%
  ungroup() %>%
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


st <- "Wood"

returns <- lnrs %>% filter(Stock == st, !(BroodYear %in% c(2002:2007))) %>% select(BroodYear, lnrs=lnrs) %>% na.omit

totret_mat <- try %>% filter(Stock == st, BroodYear %in% returns$BroodYear) %>% pull(TotalReturns) %>% as.matrix()
age_returns <- try %>% filter(Stock == st, BroodYear %in% returns$BroodYear) %>%
  select(starts_with("Age")) %>%
  as.matrix

xreg <- t(apply(as.matrix(1:nrow(age_returns)), 1, \(i) age_returns[i,]/totret_mat[i,1]))
# xreg <- age_returns

#arima_out <- Arima(returns$TotalReturns[1:59], order=c(8, 1, 0))#, xreg=xreg[1:59,3:5])
arima_out <- auto.arima(returns$lnrs, xreg=xreg[,3:5])
f <- forecast(arima_out, xreg=xreg[60:61, 3:5])
plot(f, ylim=c(0, 3))

plot(f$fitted)


# ARIMA models with lnrs time series --------------------
quesnel_totalreturns <- lnrs %>% filter(Stock == "Wood") %>%
  filter(!is.na(lnrs), !(BroodYear %in% c(2002:2007))) %>%
  pull(lnrs)

arima_out <- auto.arima(quesnel_totalreturns)
f <- forecast(arima_out)

par(mfrow = c(1,1))
plot(f)

pred <- quesnel_totalreturns+arima_out$residuals
plot(quesnel_totalreturns, type="l", ylim = c(-3,6))
lines(pred, col="red")

# can you bound it at 0 ????

## Pink salmon as a covariate --------
np.pink <- readr::read_csv("/Users/haleyoleynik/Documents/GitHub/salmon-prize_2024/covariate_data/NP_pink.csv")

lnrs.pink <- lnrs %>%
  rename(year = BroodYear) %>%
  left_join(np.pink, by = "year")

quesnel_lnrs_pink <- lnrs.pink %>% filter(Stock == "Late Stuart") %>%
  filter(lnrs > 0) %>%
  select(year, lnrs, pink)

arima_out <- auto.arima(quesnel_lnrs_pink$lnrs, xreg=quesnel_lnrs_pink$pink)
f <- forecast(arima_out, xreg=c(0.3862020,0.6980289))
plot(f)

# plot the fit 
plot(f$fitted, ylim = c(0,5)) # what is this?? 
lines(quesnel_lnrs_pink$lnrs, col="red")

lnrs.pink.naomit <- lnrs.pink %>% 
  na.omit()

ccf(lnrs.pink.naomit$pink, lnrs.pink.naomit$lnrs)
plot(lnrs.pink.naomit$pink, lnrs.pink.naomit$lnrs)

## all stocks ----------- 

# filter out Bonneville, missing data 
lnrs.pink <- lnrs.pink %>% filter(Stock != "Bonneville")

par(mfrow=c(3, 5))
stocks <- lnrs.pink %>% pull(Stock) %>% unique
for(s in stocks){
  tseries <- lnrs.pink %>% filter(Stock == s) %>% filter(lnrs > 0) %>% pull(lnrs) %>% na.omit() 
  acf(tseries, main=s)
}


par(mfrow=c(3, 5))
stocks <- lnrs.pink %>% pull(Stock) %>% unique
for(s in stocks){
  tseries <- lnrs.pink %>% filter(Stock == s) %>% filter(lnrs > 0) %>% pull(lnrs) %>% na.omit() 
  age4 <- try %>% filter(Stock == s) %>% pull(Age5)
  
  totret_mat <- returns$TotalReturns %>% as.matrix()
  age_returns <- try %>% filter(Stock == s) %>%
    select(starts_with("Age")) %>%
    as.matrix
  
  xreg <- t(apply(as.matrix(1:nrow(age_returns)), 1, \(i) age_returns[i,]/totret_mat[i,1]))
  
  
  plot(xreg[,2], tseries, main=s)
  # c <- cor.test(tseries, age4)
}


#######
#######
# How to do the linear model residual fits
#######
#######

alagnak_returns <- lnrs %>% filter(Stock == "Wood") %>% select(BroodYear, lnrs) %>% na.omit()

model <- lm(lnrs ~ BroodYear, data=alagnak_returns)
preds <- predict(model, newdata=data.frame(BroodYear=1963:2024))
summary(model)

plot(1:nrow(alagnak_returns), alagnak_returns$lnrs, type="l")
lines(1:length(preds), preds, col="red")
alagnak_returns

ar_out <- auto.arima(model$residuals)
ar_out

plot(model$residuals)
points(ar_out$residuals, col="red")
f1 <- forecast(ar_out)
f1
plot(f1)

#             Point Forecast      Lo 80     Hi 80      Lo 95    Hi 95
# 2024 = 62   4.464680e-06 -0.6893410   0.6893499   -1.0542584  1.054267


# Back calculate lnrs from residuals --- 
# Get the forecasted residuals
forecasted_residuals <- f1$mean

# Combine the linear model predictions with the forecasted residuals
forecasted_lnrs <- preds[54:62] + forecasted_residuals[1:9]

# Plot the original lnrs data, the linear model predictions, and the final forecasted lnrs values
plot(1:nrow(alagnak_returns), alagnak_returns$lnrs, type="l", ylim=range(c(alagnak_returns$lnrs, preds, forecasted_lnrs), finite=TRUE))
#lines(1:length(preds), preds, col="red")
lines((nrow(alagnak_returns) + 1):(nrow(alagnak_returns) + length(forecasted_lnrs)), forecasted_lnrs, col="blue")

## Back calculate recruits from lnrs ---

# calculate recruits from lnrs and Spawners
# R = exp(lnrs)*Sp
# in year 1, R+4 = exp(lnrs)*Sp
# so for 2024, we want to know the lnrs for 2020 (when those recruits are spawned)
exp(lnrs)*Sp 

# Calculate proportions for return years ---------- 

df <- df %>%
  mutate(Stock = ifelse(Stock == "Late Stuart", "LateStuart", Stock))

new.df <- df %>%
  group_by(Stock) %>%
  complete(BroodYear = 1948:2025) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(Age1 = AgeClass_0.1,
         Age2 = sum(AgeClass_0.2, AgeClass_1.1,na.rm=T),
         Age3 = sum(AgeClass_0.3, AgeClass_1.2, AgeClass_2.1,na.rm=T),
         Age4 = sum(AgeClass_0.4, AgeClass_1.3, AgeClass_2.2, AgeClass3.1,na.rm=T),
         Age5 = sum(AgeClass_0.5, AgeClass_1.4, AgeClass_2.3, AgeClass_3.2,na.rm=T),
         Age6 = sum(AgeClass_1.5, AgeClass_2.4, AgeClass_3.3,na.rm=T),
         Age7 = AgeClass_3.4) %>%
  select(System, Stock,BroodYear,Age1,Age2,Age3,Age4,Age5,Age6,Age7) 


proportions <- new.df %>%
  rowwise() %>%
  mutate(total = sum(c_across(Age1:Age7),na.rm=T)) %>%
  mutate(
    prop1 = Age1 / total,
    prop2 = Age2 / total,
    prop3 = Age3 / total,
    prop4 = Age4 / total,
    prop5 = Age5 / total,
    prop6 = Age6 / total,
    prop7 = Age7 / total
  ) %>%
  group_by(Stock) %>%
  mutate(
    prop1 = lag(prop1, 1),
    prop2 = lag(prop2, 2),
    prop3 = lag(prop3, 3),
    prop4 = lag(prop4, 4),
    prop5 = lag(prop5, 5),
    prop6 = lag(prop6, 6),
    prop7 = lag(prop7, 7)
  ) %>%
  select(ReturnYear = BroodYear, prop1,prop2,prop3,prop4,prop5,prop6,prop7)

#write_csv(proportions, "return_proportions.csv")

stocks <- df %>% pull(Stock) %>% unique
arima_objects <- vector(mode="list", length=length(stocks))
forecast_objects <- vector(mode="list", length=length(stocks))
i=1
for(st in stocks){
  par(mfrow=c(3, 3))
  
  stock_proportions <- proportions %>% filter(Stock == st, if_any(prop1:prop7, ~ . != 0), !ReturnYear %in% c(2022, 2023, 2024, 2025))

  arima_age_objects <- vector(mode="list", length=7)
  forecast_age_objects <- vector(mode="list", length=7)
  for(age in 1:7){
    col_name <- paste0("prop",age)
    stock_prop <- stock_proportions %>% pull(col_name, name=ReturnYear) 
    arima_out <- auto.arima(stock_prop)
    f <- forecast(arima_out)
    plot(f, main=paste0(st, ": Age ",age), ylim=c(0, 1))

    arima_age_objects[[age]] <- arima_out
    forecast_age_objects[[age]] <- f

  }
  names(arima_age_objects) <- paste0("Age",1:7)
  names(forecast_age_objects) <- paste0("Age",1:7)
  arima_objects[[i]] <- arima_age_objects
  forecast_objects[[i]] <- forecast_age_objects
  i = i+1

}

names(arima_objects) <- stocks
names(forecast_objects) <- stocks

arima_objects[["Wood"]]$Age7
as.matrix(forecast_objects[["Wood"]]$Age1[[4]])

## Pull out point forecasts
forecast_df <- data.frame(lapply(
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
  mutate(foreyear=rep(rep(c(1:10), each=length(stocks)*7))) %>%
  pivot_wider(names_from="age", values_from="prop") %>%
  arrange(stock, foreyear) %>%
  replace_na(replace=list(0))
  print(n=100)
  

forecast_df %>% pivot_longer(Age1:Age7, names_to="age", values_to="prop") %>%
  ggplot() +
    geom_line(aes(x=foreyear, y=prop, color=age))+
    scale_y_continuous(limits=c(0, 1))+
    theme_bw()+
    facet_wrap(~stock)




stock_proportions %>% print(n=100)

rbind(forecast_objects[["Stellako"]]$Age1, forecast_objects[["Stellako"]]$Age2)

tsibble(forecast_objects[["Stellako"]]$Age1[[4]])

forecast_objects[["Stellako"]]$Age4 %>% as.data.frame
