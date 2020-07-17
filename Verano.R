#instalación de paquetes
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("PerformanceAnalytics")

#cargar paquetes
library("readr")
library("tidyverse")
library(PerformanceAnalytics)

#cargar ruta
ruta<-choose.files()
#cargar archivo
datos<-read.csv(ruta)


#separar registros de las 12 hrs
datosdoce<-datos %>% filter(hour==12)
 

datosdoce1<- datosdoce %>% select(year,month,dayofweek,dayofmonth,hour,CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)
chart.Correlation(datosdoce1, histogram = TRUE, pch="+")
