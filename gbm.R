##packages and library 
#install.packages("sde")
#install.packages("ggplot2")
#install.packages("MLmetrics")
library(MLmetrics)
library(dplyr)
library(sde)
library(plotly)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(stringr)
library(tidyverse)
library(Metrics)
library(ggplot2)
library(lubridate) #use to make date as chr to date format

######import data
set.seed(4238)

N50 <- read.csv("C:/Users/prati/Downloads/gbm_train.csv", header = TRUE)
#View(N50)
actual<- read.csv("C:/Users/prati/Downloads/gbm_test.csv", header = TRUE)
#View(actual)
#####visualisation of the data

# Creating column for Trends - Increasing and Decreasing
for (i in 1:length(N50[,1])){
  if (N50$Close[i] >= N50$Open[i]){N50$Direction[i] = "Increasing"}
  else {N50$Direction[i] = "Decreasing"}
}
#View(N50)
# Creating color lists for Bullish and Bearish Candle
i <- list(line = list(color = 'green'))
d <- list(line = list(color = 'pink'))

# Creating basic candle stick diagram to visualize share price - Point 2
candleStickGraph <- N50 %>%
  # Creating Plot and assigning appropriate columns of data
 plot_ly(x = ~Date, type = "candlestick",
          open = ~Open, close = ~Close,
          high = ~High, low = ~Low,
          increasing = i, decreasing = d) %>%
  # Layout Creation
  layout(title = "Sensex Price: Behaviour and Performance",
         xaxis = list(rangeslider = list(visible = F), title = "Date"),
         yaxis = list(title = "Share Price (Open, Close, High, Low)"))
# Calling candleStickGraph
candleStickGraph
# Creating basic line diagram to visualize share price closing - Point 2
lineGraph <- N50 %>%
  plot_ly(x = ~Date, y = ~Close, type = "scatter", mode = "lines") %>%
  layout(title = "Sensex Price: Date vs Closing Price",
         xaxis = list(rangeslider = list(visible = F), title = "Date"),
         yaxis = list(title = "Price (Close)"))
lineGraph

#####normality test 


for (i in 2:length(N50[,1])){
  N50$Daily_Return[i-1] <- (N50$Close[i]-N50$Close[i-1])/N50$Close[i-1]
  #print(i)
}

HistGraphDistribution <- N50 %>%
  plot_ly(x = ~Daily_Return, type = "histogram", histnorm = "probability") %>%
  layout(title = "Distribution of Daily Returns",
         xaxis = list(title = "Daily Returns"))
HistGraphDistribution
#mu <- mean(log(N50$Daily_Return))
#mu
qqnorm(N50$Daily_Return)
qqline(N50$Daily_Return)


#####gbm using drift and volitality

##1
#Given that our test data is over a period of 6 months (0.5 years), our tau = 0.5
#We first find u_i, our log of daily return on stock
N50['u'] <- 0
for (i in 2:1175){
  N50$u[i] = log(N50$Close[i]/N50$Close[i-1]) 
}
u_bar = sum(N50$u)/1174
u_bar
v = numeric(1174)
for (i in 2:1174){
  v[i-1] = N50$u[i] - u_bar
}
v = sqrt(sum(v^2)/1173)
v
sigma_star = v/sqrt(0.5)
sigma_star
#volatily: it is the spread or variance, from our output we have 1% which is also a big volatily
drift = u_bar/0.5 + 0.5*(sigma_star)^2
drift
#drift: this shows us the moment of the market direction since we have positive number but small this means that the market will be going up but slowly
#With the sigma_star and drift values, we can then calculate again the corresponding CI.

new_final_stock_price = numeric(247)
#new_final_stock_price
S_0 = N50$Close[1175] #intial value
S_t <- GBM(S_0,r=drift,sigma=sigma_star,T=247,N=247)
new_final_stock_price[1] = S_t[length(S_t)]
plot(seq(0,247,by=1),S_t,type="l", main = "Geometric Brownian Motion Prediction of Price with Updated Parameters")  #run multiple times 
S_t
hist(S_t, main = "Forecasted Histogram")
forecast <- as.data.frame(S_t)
forecast

#gbm simulation 
for (i in 2:248){
  S_t_sim <- GBM(S_0,r=drift,sigma=sigma_star,T=27,N=247)
 lines(seq(0,247,by=1),S_t_sim,type="l")
  new_final_stock_price[i] = S_t_sim[length(S_t_sim)]
}
hist(new_final_stock_price, main = "GBM Histogram :Price with drift")
S_t_sim
q<-quantile(S_t_sim,probs<-c(0.005,0.025,0.25,0.5,0.75,0.975,0.995), na.rm = F )
q
summary(q)
str(S_t_sim)

#Again, this is lognormal so we can normalize it to find the corresponding CI.
#not to be done 
new_log_fsp = log(new_final_stock_price)
new_avg_log_fsp = mean(new_log_fsp)
new_sd_log_fsp = sd(new_log_fsp)/sqrt(length(new_log_fsp))
new_lower_log_fsp = mean(new_log_fsp) - qnorm(0.975)*new_sd_log_fsp
new_upper_log_fsp = mean(new_log_fsp) + qnorm(0.975)*new_sd_log_fsp
new_lower_fsp = exp(new_lower_log_fsp)
new_upper_fsp = exp(new_upper_log_fsp)




##forecasted graph

plot(seq(0,247,by=1),S_t,type="l", main = "Geometric Brownian Motion Prediction of Price with Updated Parameters")  #run multiple times 
S_t

##actual v/s forecasted graph
actual["Time Interval"] = c(1:nrow(actual))
forecast["Time Interval"] = c(1:nrow(forecast))

p = ggplot() + 
  geom_line(data<-forecast, mapping= aes(x = forecast$`Time Interval`, y = forecast$x), color = "blue") +
  geom_line(data = actual, mapping= aes(x = actual$`Time Interval`, y = actual$Close), color = "red") +
  xlab('Dates') + ylab('Price')

print(p)

##drift doesnt take in consideration the emotional and news influence on the stock market which can be seen in the price difference 
##corelation 
c <- cor(actual$Close, forecast$x)  #general code 
c
#this will show us the relation between the forecasted and actual value for the prediction time 


##mape
#load MLmetrics package

#calculate MAPE
MAPE(forecast$x , actual$Close)

write.csv(x=forecast, file="C:/Users/prati/OneDrive/Desktop/project/output1")


actual['u'] <- 0
for (i in 2:248){
  actual$u[i] = log(actual$Close[i]/actual$Close[i-1]) 
}
u_bar = sum(actual$u)/248
u_bar
v = numeric(248)
for (i in 2:248){
  v[i-1] = actual$u[i] - u_bar
}
v = sqrt(sum(v^2)/248)
v
sigma_star_a = v/sqrt(0.5)
sigma_star_a
#volatily: it is the spread or variance, from our output we have 1% which is also a big volatily
drift_a = u_bar/0.5 + 0.5*(sigma_star)^2
drift_a


forecast['u'] <- 0
for (i in 2:248){
  forecast$u[i] = log(forecast$x[i]/forecast$x[i-1]) 
}
u_bar = sum(forecast$u)/248
u_bar
v = numeric(248)
for (i in 2:248){
  v[i-1] = forecast$u[i] - u_bar
}
v = sqrt(sum(v^2)/248)
v
sigma_star_f = v/sqrt(0.5)
sigma_star_f
drift_f = u_bar/0.5 + 0.5*(sigma_star)^2
drift_f
