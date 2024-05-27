
arimas <- try %>% split(try$Stock) %>%
  map(\(df){
      arima_out <- auto.arima(df$TotalReturns, start.Q=1)$residuals 
      df$TotalReturns + arima_out
  })

pad_length <- (max_year - min_year - lengths(arimas)+1)

predictions_df <- lapply(1:length(arimas), function(i){
   padded <- c(rep(NA, pad_length[i]), arimas[[i]])
  #  names(padded) <- rep(names(arimas)[i], length(padded))
   return(padded)
}) %>%
  as.data.frame(col.names = names(arimas)) %>%
  mutate(PredYear = as.double(min_year:max_year)) %>%
  pivot_longer(
    -c(PredYear),
    names_to = "Stock",
    values_to = "Prediction"
  ) %>%
  filter(!is.na(Prediction)) %>%
  mutate(Stock = ifelse(Stock == "Late.Stuart", "Late Stuart", Stock))
  
try %>%
  left_join(
      predictions_df,
      by = c("Stock", "BroodYear"="PredYear")
  ) %>%
    ggplot(., aes(x=BroodYear, y=TotalReturns))+
        geom_line(color="black")+
        geom_line(aes(y=Prediction), color="red")+
        coord_cartesian(expand=0) + 
        theme_bw() + 
        facet_wrap(~Stock, scales="free_y")




forecasts <- try %>% split(try$Stock) %>%
  map(\(df){
      arima_out <- auto.arima(df$TotalReturns)
      f <- forecast(arima_out)$mean
  })

start_years <- max_year - lengths(arimas)

predictions_df <- forecasts %>%
  bind_rows %>%
  mutate(ForeYear = 2023:2032) %>%
  pivot_longer(
    -c(ForeYear),
    names_to = "Stock",
    values_to = "Forecast"
  ) %>%
  filter(!is.na(Forecast))

try %>%
  left_join(
      predictions_df,
      by = c("Stock", "BroodYear"="ForeYear")
  ) %>%
  filter(TotalReturns != 0) %>%
  print(n=200)

ggplot(try, aes(x=BroodYear, y=TotalReturns))+
  geom_line(color="black")+
  geom_line(data=predictions_df, aes(x=ForeYear, y=Forecast), color="red")+
  facet_wrap(~Stock, scales="free_y")
