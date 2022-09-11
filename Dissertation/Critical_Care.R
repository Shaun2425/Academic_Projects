



Critical_Care <- read_excel("C:/Users/ASUS/Downloads/RSV-Data-Aggregates.xlsx", sheet = "RSV Spells By Is Critical Care")

Critical_Care


Critical_Care$`Row Labels` = as.Date(as.yearmon(Critical_Care$`Row Labels`)) # converts to proper date format

Critical_Care$`Row Labels`



#----Including missing months-------


df4 =  Critical_Care%>%
  mutate(`Row Labels` = as.Date(`Row Labels`), 
         Row_Lables = as.Date(format(`Row Labels`, "%Y-%m-01"))) %>%
  tidyr::complete(Row_Lables = seq(min(Row_Lables), max(Row_Lables), "1 month"))

df4

data.frame(df4)

d_d = df4

d_d

df4 = subset(df4, select= -(`Row Labels`))

df4[is.na(df4)] <- 0

df4 = data.frame(df4)

df4

ts.plot(df4$Grand.Total)

#-----Valleys and Peaks-----

library(ggpmisc)

ggplot(df4, aes(x = Row_Lables , y = Grand.Total )) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red",angle = 90, 
             vjust = 1.5, x.label.fmt = "%b") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 90,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%b") + theme_bw()


 
df4 = subset(df4, select= -(True))

df4 = subset(df4, select= -(False))

df4



#---Plotting the time series-----


p4 = ggplot(data = df4, aes(x = Row_Lables, y = Grand.Total))+ geom_line() + scale_x_date(date_labels = "%b %Y") 

p4  + stat_smooth(color = "#FC4E07",method = "loess") + theme_bw()


df_ts4<- df4%>% column_to_rownames(., var = 'Row_Lables') # Changing into index


df_ts4 =  ts(df_ts4, start=c(2017, 6), end=c(2022, 6), frequency=12) # converting to time series

plot(decompose(df_ts4, type = 'multiplicative'))

acf(df_ts4)

pacf(df_ts4)

Box.test(df_ts4, lag=1.4, type="Ljung-Box")


#------Model fitting-------


#Create samples

training4 = window(df_ts4, start = c(2017,6), end = c(2021,12))

validation4 = window(df_ts4, start = c(2022,1))

 

#-------SARIMA---------

arima_optimal4 = auto.arima(training4,stepwise=FALSE, approximation=FALSE)

arima_optimal4


sarima_forecast4 = sarima.for(training4, n.ahead=length(validation4),
                             p=1,d=0,q=1,P=1,D=0,Q=0,S=12)

smape(sarima_forecast4$pred, validation4) * 100


f_sa4 = forecast(arima_optimal4,length(validation4))

autoplot(f_sa4)



plot(df_ts4, col="blue", type='l')

lines(sarima_forecast4$pred, col="red", lwd=2)

sarima_forecast4$pred

validation4

r4 = data.frame(sarima_forecast4$pred)

r4


#-----------------------------------------------------------------------------


df5 = d_d

df5


df5 = subset(df5, select= -(`Row Labels`))

df5 = subset(df5, select= -(True))

df5 = subset(df5, select= -(`Grand Total`))

df5


df5[is.na(df5)] <- 0

df5 = data.frame(df5)

df5

ts.plot(df5$False)


df_ts5<- df5%>% column_to_rownames(., var = 'Row_Lables') # Changing into index


df_ts5 =  ts(df_ts5, start=c(2017, 6), end=c(2022, 6), frequency=12) # converting to time series



#------Model fitting----------


#Create samples

training5 = window(df_ts5, start = c(2017,6), end = c(2021,12))

validation5 = window(df_ts5, start = c(2022,1))



#-------SARIMA--------

arima_optimal5 = auto.arima(training5,stepwise=FALSE, approximation=FALSE)

arima_optimal5



sarima_forecast5 = sarima.for(training5, n.ahead=length(validation5),
                              p=1,d=0,q=1,P=1,D=0,Q=0,S=12)

smape(sarima_forecast5$pred, validation5) * 100


f_sa5 = forecast(arima_optimal5,length(validation5))

autoplot(f_sa5)



plot(df_ts5, col="blue", type='l')

lines(sarima_forecast5$pred, col="red", lwd=2)

sarima_forecast5$pred

r5 = data.frame(sarima_forecast5$pred)

r5

r4 - r5

tail(d_d,6)

#-------------------Future forecasting-----------------------------

arima_optimal_f1 = auto.arima(df_ts4,stepwise=FALSE, approximation=FALSE)

arima_optimal_f1


sarima_forecast_f1 = sarima.for(df_ts4, n.ahead = 7,
                              p=1,d=0,q=1,P=1,D=0,Q=0,S=12)

f_f1 = forecast(arima_optimal_f1,7)

f_f1

autoplot(f_f1, conf.int.fill='blue') + xlab('Time') + ylab('Grand.Total') + theme_bw()

 

r_f1 = data.frame(sarima_forecast_f1$pred)

r_f1



arima_optimal_f2 = auto.arima(df_ts5,stepwise=FALSE, approximation=FALSE)

arima_optimal_f2


sarima_forecast_f2 = sarima.for(df_ts5, n.ahead = 7,
                                p=1,d=0,q=1,P=1,D=0,Q=0,S=12)

f_f2 = forecast(arima_optimal_f2,7)

f_f2

autoplot(f_f2,conf.int.fill='blue') + xlab('Time') + ylab('False') + theme_bw()

r_f2 = data.frame(sarima_forecast_f2$pred)

r_f2


r_f1 - r_f2

#-------------------------------------------------------------------------------


#----COVID adjustment for Grand Total---------

# Replace the dataset that is impacted by COVID-19 with forecast values of that period.

dataset6 = window(df_ts4, start = c(2017,6), end = c(2020,3))

dataset6

train6 = window(df_ts4, start = c(2017,6), end = c(2019,7))

train6

test6 = window(df_ts4, start = c(2019,8), end = c(2020,3))

test6


#--Sarima--------

arima_optimal6 = auto.arima(train6,stepwise=FALSE, approximation=FALSE)

arima_optimal6


sarima_forecast6 = sarima.for(train6, n.ahead=length(test6),
                              p=1,d=1,q=1,P=0,D=1,Q=0,S=12)

smape(sarima_forecast6$pred, test6) * 100


plot(dataset6, col="blue", type='l')

lines(sarima_forecast6$pred, col="red", lwd=2)


#-----------------------------------------------


#Train a neural network model

acf(train6)

pacf(train6)

set.seed(20)


neuralnet6 <- nnetar(train6,p = 1, P= 1, hidden = c(2),linear.output=T, lambda = 1)


#Generate forecasts with the model

neuralnet6.forecast<- forecast(neuralnet6, h=length(test6))



#Check the MAPE

smape(neuralnet6.forecast$mean, test6) * 100


neuralnet6.forecast$mean

autoplot(neuralnet6.forecast)


plot(dataset6, col="blue", type='l')

lines(neuralnet6.forecast$mean, col="red", lwd=2)


# #------TBATS-------
# 
# tbats_model6 = tbats(train6)
# 
# tbats_forecast6 = forecast(tbats_model6, h=length(test6))
# 
# autoplot(tbats_forecast6)
# 
# smape(tbats_forecast6$mean, test6) * 100
# 
# 
# #---------------------------------
# 
# ets_model6 = ets(train6,model="ZZZ" ,allow.multiplicative.trend = FALSE)
# 
# summary(ets_model6)
# 
# ets_forecast6 = forecast(ets_model6, h=length(test6))
# 
# smape(ets_forecast6$mean, test6) *100
# 
# #--------------------------------------------------
# 
# stl_forecast6 = stlf(train6, h=length(test6))
# 
# autoplot(stl_forecast6)
# 
# smape(stl_forecast6$mean, test6) *100




#--------------Replacement for Grand Total------------------------------------------------------


#Train a neural network model

set.seed(20)


neuralnet7 <- nnetar(dataset6,p = 1, P= 1, hidden = c(2), lambda = 1)


#Generate forecasts with the model

neuralnet7.forecast<- forecast(neuralnet7, h= 11)

autoplot(neuralnet7.forecast)

neuralnet7.forecast

dataset6

first6 = ts(dataset6)

include6 = ts(c(7.076311,6.658816,6.177302,5.299063,6.102009,8.433851,73.375232,171.600633,198.204859,156.562782,44.954398),
             start = c(2020,4), end = c(2021,2), frequency = 12)

# include6 = ts(c(6.380467,6.214034,5.427229,6.177384,8.344444,71.884613,170.409274,199.791609,154.454340,37.472568),
#               start = c(2020,5), end = c(2021,2), frequency = 12)

first6

include6

merged_ts6 <- ts(c(first6, include6),               
                start = c(2017,6),
                end = c(2021,2),
                frequency = 12)

# print merged time series
merged_ts6

autoplot(merged_ts6)


final6 = window(df_ts4, start = c(2021,3), end = c(2022,6))

merged_final6 = ts(c(merged_ts6, final6),               
                  start = c(2017,6),
                  end = c(2022,6),
                  frequency = 12)


plot(merged_final6, ylab = 'Grand.Total')


 

#-------------------------------------------------------------------------------


#Create samples

training6= window(merged_final6, start = c(2017,6),
                  end = c(2021,2),
                  frequency = 12)

validation6= window(merged_final6, start = c(2021,3), end = c(2022,6))

training6

validation6


#-------------------------------------------------------------------------------

#-------SARIMA-----



arima_optimal8 = auto.arima(training6,stepwise=FALSE, approximation=FALSE)

arima_optimal8


sarima_forecast8 = sarima.for(training6, n.ahead=length(validation6),
                              p=0,d=0,q=1,P=0,D=1,Q=0,S=12)

smape(sarima_forecast8$pred, validation6) * 100


f_sa8 = forecast(arima_optimal8,length(validation6))

autoplot(f_sa8)



plot(merged_final6, col="blue", type='l')

lines(sarima_forecast8$pred, col="red", lwd=2)


#-------------------------------------------------------------------------------



#Train a neural network model

acf(training6)

pacf(training6)



set.seed(20)


neuralnet8 <- nnetar(training6)


#Generate forecasts with the model

neuralnet8.forecast<- forecast(neuralnet8, h=length(validation6))

autoplot(neuralnet8.forecast)


#Check the MAPE

smape(neuralnet8.forecast$mean, validation6) * 100

neuralnet8.forecast$mean

plot(merged_final6, col="blue", type='l')

lines(neuralnet8.forecast$mean, col="red", lwd=2)




#------TBATS-------

tbats_model8 = tbats(training6)

tbats_forecast8 = forecast(tbats_model8, h=length(validation6))

autoplot(tbats_forecast8)

smape(tbats_forecast8$mean, validation6) * 100



plot(merged_final6, col="blue", type='l')

lines(tbats_forecast8$mean, col="red", lwd=2)



tbats_forecast8$mean

validation6


#-------------------------------------------------------------------------------

ets_model8 = ets(training6,model="ZZZ" ,allow.multiplicative.trend = TRUE)

summary(ets_model8)

ets_forecast8 = forecast(ets_model8, h=length(validation6))

smape(ets_forecast8$mean, validation6) *100


plot(merged_final6, col="blue", type='l')

lines(ets_forecast8$mean, col="red", lwd=2)

#-------------------------------------------------------------------------------

stl_forecast8 = stlf(training6, h=length(validation6))

autoplot(stl_forecast8)

smape(stl_forecast8$mean, validation6) *100


plot(merged_final6, col="blue", type='l')

lines(stl_forecast8$mean, col="red", lwd=2)

#-------------------------------------------------------------------------------



#---Forecasting using STL--------------------

stl_forecast9 = stlf(merged_final6, h= 7)

stl_forecast9

autoplot(stl_forecast9,conf.int.fill='blue') + xlab('Time') + ylab('Grand.Total') + theme_bw()


r9 = data.frame(stl_forecast9$mean)

r9

#-------------------------------------------------------------------------------

# set.seed(20)
# 
# 
# neuralnet9 <- nnetar(merged_final6)
# 
# 
# #Generate forecasts with the model
# 
# neuralnet9.forecast<- forecast(neuralnet9, h=36)
# 
# autoplot(neuralnet9.forecast)
# 
# 
# #-------------------------------------------------------------------------------
# 
# 
# 
# arima_optimal9 = auto.arima(merged_final6,stepwise=FALSE, approximation=FALSE)
# 
# arima_optimal9
# 
# 
# f_sa9 = forecast(arima_optimal9,length = 36)
# 
# autoplot(f_sa9)


#-------------------------------------------------------------------------------


#----COVID adjustment for False---------

# Replace the dataset that is impacted by COVID-19 with forecast values of that period.

dataset10 = window(df_ts5, start = c(2017,6), end = c(2020,3))

dataset10

train10 = window(df_ts5, start = c(2017,6), end = c(2019,7))

train10

test10 = window(df_ts5, start = c(2019,8), end = c(2020,3))

test10


#--Sarima---------------

arima_optimal10 = auto.arima(train10,stepwise=FALSE, approximation=FALSE)

arima_optimal10


sarima_forecast10 = sarima.for(train10, n.ahead=length(test10),
                              p=1,d=1,q=1,P=0,D=1,Q=0,S=12)

smape(sarima_forecast10$pred, test10) * 100


plot(dataset10, col="blue", type='l')

lines(sarima_forecast10$pred, col="red", lwd=2)


#-----------------------------------------------


#Train a neural network model

acf(train10)

pacf(train10)

set.seed(20)


neuralnet10 <- nnetar(train10,p = 1, P= 1, hidden = c(2),linear.output=T, lambda = 1)


#Generate forecasts with the model

neuralnet10.forecast<- forecast(neuralnet10, h=length(test10))



#Check the MAPE

smape(neuralnet10.forecast$mean, test10) * 100


neuralnet10.forecast$mean

autoplot(neuralnet10.forecast)


plot(dataset10, col="blue", type='l')

lines(neuralnet10.forecast$mean, col="red", lwd=2)

#-------------------------------------------------------------------------------


 
#--------------Replacement for False------------------------------------------------------


#Train a neural network model

set.seed(20)


neuralnet11 <- nnetar(dataset10,p = 1, P= 1, hidden = c(2),linear.output=T, lambda = 1)


#Generate forecasts with the model

neuralnet11.forecast<- forecast(neuralnet11, h= 11)

autoplot(neuralnet11.forecast)

neuralnet11.forecast

dataset10

first10 = ts(dataset10)

include10 = ts(c(5.173036,4.860786,4.319975,3.805100,4.808491,6.376009,62.976658,157.883664,177.646070,143.989498,46.057907),
              start = c(2020,4), end = c(2021,2), frequency = 12)

# include10 = ts(c(4.002144,3.657815,3.128077,4.157920,5.783960,62.655314,157.323024,175.445760,146.807119,50.785130),
#                start = c(2020,5), end = c(2021,2), frequency = 12)

first10

include10

merged_ts10 <- ts(c(first10, include10),               
                 start = c(2017,6),
                 end = c(2021,2),
                 frequency = 12)

# print merged time series
merged_ts10

autoplot(merged_ts10)


final10 = window(df_ts5, start = c(2021,3), end = c(2022,6))

merged_final10 = ts(c(merged_ts10, final10),               
                   start = c(2017,6),
                   end = c(2022,6),
                   frequency = 12)


autoplot(merged_final10, y = 'False')



#-------------------------------------------------------------------------------


#Create samples

training10 = window(merged_final10, start = c(2017,6),
                  end = c(2021,2),
                  frequency = 12)

validation10 = window(merged_final10, start = c(2021,3), end = c(2022,6))

training10

validation10


#-------------------------------------------------------------------------------

#-------SARIMA-----



arima_optimal12 = auto.arima(training10,stepwise=FALSE, approximation=FALSE)

arima_optimal12


sarima_forecast12 = sarima.for(training10, n.ahead=length(validation10),
                              p=0,d=0,q=1,P=0,D=1,Q=0,S=12)

smape(sarima_forecast12$pred, validation10) * 100



f_sa12 = forecast(arima_optimal12,length(validation10))

autoplot(f_sa12)



plot(merged_final10, col="blue", type='l')

lines(sarima_forecast12$pred, col="red", lwd=2)

#-------------------------------------------------------------------------------



#Train a neural network model

acf(training10)

pacf(training10)



set.seed(20)


neuralnet12 <- nnetar(training10)


#Generate forecasts with the model

neuralnet12.forecast<- forecast(neuralnet12, h=length(validation10))

autoplot(neuralnet12.forecast)


#Check the MAPE

smape(neuralnet8.forecast$mean, validation6) * 100

neuralnet12.forecast$mean

plot(merged_final10, col="blue", type='l')

lines(neuralnet12.forecast$mean, col="red", lwd=2)




#------TBATS-------

tbats_model12 = tbats(training10)

tbats_forecast12 = forecast(tbats_model12, h=length(validation10))

autoplot(tbats_forecast12)

smape(tbats_forecast12$mean, validation10) * 100



plot(merged_final10, col="blue", type='l')

lines(tbats_forecast12$mean, col="red", lwd=2)



tbats_forecast12$mean

validation10


#-------------------------------------------------------------------------------

ets_model12 = ets(training10,model="ZZZ" ,allow.multiplicative.trend = TRUE)

summary(ets_model12)

ets_forecast12 = forecast(ets_model12, h=length(validation10))

smape(ets_forecast12$mean, validation10) *100


plot(merged_final10, col="blue", type='l')

lines(ets_forecast12$mean, col="red", lwd=2)

#-------------------------------------------------------------------------------

stl_forecast12 = stlf(training10, h=length(validation10))

autoplot(stl_forecast12)

smape(stl_forecast12$mean, validation10) *100


plot(merged_final10, col="blue", type='l')

lines(stl_forecast12$mean, col="red", lwd=2)

#-------------------------------------------------------------------------------



#---Forecasting using STL--------------------


stl_forecast14 = stlf(merged_final10, h= 7)

(stl_forecast14$)

autoplot(stl_forecast14,conf.int.fill='blue') + xlab('Time') + ylab('False') + theme_bw()


r14 = data.frame(stl_forecast14$mean)

r14

r9

True = r9 - r14

True
#-------------------------------------------------------------------------------

# set.seed(20)
# 
# 
# neuralnet9 <- nnetar(merged_final6)
# 
# 
# #Generate forecasts with the model
# 
# neuralnet9.forecast<- forecast(neuralnet9, h=36)
# 
# autoplot(neuralnet9.forecast)
# 
# 
# #-------------------------------------------------------------------------------
# 
# 
# 
# arima_optimal9 = auto.arima(merged_final6,stepwise=FALSE, approximation=FALSE)
# 
# arima_optimal9
# 
# 
# f_sa9 = forecast(arima_optimal9,length = 36)
# 
# autoplot(f_sa9)
# 

#-------------------------------------------------------------------------------











