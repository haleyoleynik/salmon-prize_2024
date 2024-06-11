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



