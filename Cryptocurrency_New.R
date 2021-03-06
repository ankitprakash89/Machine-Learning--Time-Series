library(data.table) 
library(dplyr)
library(plyr)
library(tseries)
library(forecast)
library(caret)
library(ggplot2)
library(mice)
library(zoo)

url <- "C:/Users/Administrator/Desktop/Machine Learning/Datasets/CryptoDataset/matrix_one_file/price_data.csv"

crypto <- fread(url, header = TRUE)

crypto_main <- crypto[,c(1:17,20,25,34,37)]
View(crypto_main)
crypto_loop <- crypto_main[,3:4]
name <- names(crypto_loop)
for( i in name){
  
  crypto_work <- crypto_main %>% select(time,i)
  names(crypto_work) <- c("Time", "Price")
  crypto_work$Time <- as.POSIXct(crypto_work$Time, format = "%Y-%m-%d %H:%M:%S")
  d<- colnames(crypto_work)[2]
  
    # to get the data for time series
  crypto_work1 <- crypto_work %>% filter(Time <= "2018-01-24 11:55:00",Time >= "2018-01-18 00:00:00")
  Time <- seq(ISOdatetime(2018,1,18,00,0,0), ISOdatetime(2018, 1, 24,11,55,0), by= (60*5))
  df <- data.frame(Time)
  crypto_temp <- join(df, crypto_work1, by = "Time")
  crypto_temp$Price <- na.approx(crypto_temp$Price)
  
  # to get the original value from 25th Jan to 29th Jan
  crypto_orignal_value <- crypto_work %>% filter(Time <= "2018-01-29 11:55:00",Time >= "2018-01-25 00:00:00")
  Time <- seq(ISOdatetime(2018,1,25,00,0,0), ISOdatetime(2018, 1, 29,11,55,0), by= (60*5))
  df1 <- data.frame(Time)
  crypto_temp1 <- join(df1, crypto_orignal_value ,by = "Time")
  crypto_temp1$Price <- na.approx(crypto_temp1$Price)
  
  df_new <- data.frame()
  new_df <- data.frame()
  value <- c()
  start <- 1
  
  for(j in 1:5){
    
    for(k in 1:288){
      
      crypto_price <- ts(crypto_temp$Price, start = c(1,1), frequency = 288)
      
      fit1 <- ets(crypto_price)
      
      a <- forecast(fit1, h=1)
      value <- append(value,a$mean)
      
      df_new<- data.frame(crypto_temp1$Time[start], a$mean)
      names(df_new) <- c("Time","Price")
      
      crypto_temp <- rbind(crypto_temp, df_new)
      start <- start+1
      
    }
    output_file <- crypto_temp[(1873+(start-k)):nrow(crypto_temp),]
    rownames(output_file) <- c()
    name <- paste(i,d,"(",j,")",".csv",sep = "")
    write.csv(output_file,name)
    }
 }
 

  