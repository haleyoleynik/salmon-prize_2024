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


columbia_totalreturns <- try %>% filter(Stock == "Bonneville") %>%
  pull(TotalReturns)

arima_out <- auto.arima(columbia_totalreturns, start.Q=2)
f <- forecast(arima_out)

plot(f, ylim=c(0, 1e6))

pred <- columbia_totalreturns+arima_out$residuals

plot(columbia_totalreturns, type="l")
lines(pred, col="red")


quesnel_totalreturns <- try %>% filter(Stock == "Late Stuart") %>%
  pull(TotalReturns)

arima_out <- auto.arima(quesnel_totalreturns, start.Q=1)
f <- forecast(arima_out)

plot(f, ylim=c(0, 6e6))

pred <- quesnel_totalreturns+arima_out$residuals
plot(quesnel_totalreturns, type="l")
lines(pred, col="red")



quesnel_totalreturns <- try %>% filter(Stock == "Ugashik") %>% filter(TotalReturns > 0) %>%
  pull(TotalReturns)

# quesnel_totalreturns[1:25] <- NA

arima_out <- auto.arima(quesnel_totalreturns)
f <- forecast(arima_out)

plot(f, ylim=c(0, 2e7))

pred <- quesnel_totalreturns+arima_out$residuals
plot(quesnel_totalreturns, type="l")
lines(pred, col="red")



raft_totalreturns <- try %>% filter(Stock == "Raft") %>%
  pull(TotalReturns)

arima_out <- auto.arima(raft_totalreturns, start.Q=1)
f <- forecast(arima_out)

plot(f, ylim=c(0, 1e5))

pred <- raft_totalreturns+arima_out$residuals
plot(raft_totalreturns, type="l")
lines(pred, col="red")




par(mfrow=c(1, 1))
returns <- try %>% 
  filter(Stock == "Nushagak", TotalReturns != 0) %>% 
  select(BroodYear, Age=Age4, TotalReturns=TotalReturns)

plot(returns$TotalReturns, type="l")

arima_out <- Arima(returns$TotalReturns, order=c(2, 1, 1))
f <- forecast(arima_out)
plot(f)

pred <- totalreturns+arima_out$residuals
plot(totalreturns, type="l")
lines(pred, col="red")

returns
returns %>% mutate(prop=Age/TotalReturns) %>% summary(prop)

st = "Egegik"

quesnel_totalreturns <- try %>% filter(Stock == st) %>% filter(TotalReturns > 0) %>%
  pull(TotalReturns)

# quesnel_totalreturns[55:61] <- NA

arima_out <- auto.arima(quesnel_totalreturns)
f <- forecast(arima_out)

par(mfrow=c(2, 1))
plot(f, ylim=c(0, 2e7))

returns <- try %>% filter(Stock == st, TotalReturns != 0) %>% select(BroodYear, TotalReturns=TotalReturns)

totret_mat <- returns$TotalReturns %>% as.matrix()
age_returns <- try %>% filter(Stock == st, TotalReturns != 0) %>%
  select(starts_with("Age")) %>%
  as.matrix

xreg <- t(apply(as.matrix(1:nrow(age_returns)), 1, \(i) age_returns[i,]/totret_mat[i,1]))
# xreg <- age_returns

#arima_out <- Arima(returns$TotalReturns[1:59], order=c(8, 1, 0))#, xreg=xreg[1:59,3:5])
arima_out <- auto.arima(returns$TotalReturns, xreg=xreg[,3:5])
f <- forecast(arima_out, xreg=xreg[60:61, 3:5])
plot(f, ylim=c(0, 2e7))

plot(f$fitted)
lines(returns, col="red")

lines(returns, col="red")


par(mfrow=c(3, 5))
stocks <- try %>% pull(Stock) %>% unique
for(s in stocks){
  tseries <- try %>% filter(Stock == s) %>% pull(TotalReturns)
  acf(tseries, main=s)
}


par(mfrow=c(3, 5))
stocks <- try %>% pull(Stock) %>% unique
for(s in stocks){
  tseries <- try %>% filter(Stock == s) %>% pull(TotalReturns)
  age4 <- try %>% filter(Stock == s) %>% pull(Age5)

  totret_mat <- returns$TotalReturns %>% as.matrix()
  age_returns <- try %>% filter(Stock == s) %>%
    select(starts_with("Age")) %>%
    as.matrix

  xreg <- t(apply(as.matrix(1:nrow(age_returns)), 1, \(i) age_returns[i,]/totret_mat[i,1]))


  plot(xreg[,2], tseries, main=s)
  # c <- cor.test(tseries, age4)
}

try %>% split(try$Stock) %>%
  map(\(df){
      cor.test(df$TotalReturns, df$Age3)$estimate 
  })




#######
#######
# How to do the linear model residual fits
#######
#######

alagnak_returns <- try %>% filter(Stock == "Alagnak", TotalReturns > 0) %>% select(BroodYear, TotalReturns)

model <- lm(TotalReturns ~ BroodYear, data=alagnak_returns)
preds <- predict(model, newdata=data.frame(BroodYear=1955:2023))
summary(model)

plot(1:nrow(alagnak_returns), alagnak_returns$TotalReturns, type="l")
lines(1:length(preds), preds, col="red")
alagnak_returns

ar_out <- Arima(model$residuals, order=c(4, 0, 1))
ar_out

plot(model$residuals)
points(ar_out$residuals, col="red")
f1 <- forecast(ar_out)
f1

plot(f1)
