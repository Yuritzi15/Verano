#instalación de paquetes
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("PerformanceAnalytics")
#install.packages("psych")
#cargar paquetes
library("readr")
library("tidyverse")
library(PerformanceAnalytics)
library(lubridate)
library(psych)
library(GGally)

#cargar ruta
ruta<-choose.files()

#cargar archivo
datos<-read.csv(ruta)

#separar registros de las 12 hrs y hacer los campos en fecha
datosdoce<-datos %>% filter(hour==12) %>% mutate(fecha =make_date(year,month, dayofmonth))

#Selección de variables para correlación
datosdoce1<- datosdoce %>% select(CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)

chart.Correlation(datosdoce1, histogram = TRUE, pch=21, col=4)

#Lunes
datoslunes<- datosdoce1 %>% filter(dayofweek==0)
chart.Correlation(datoslunes,bg=seq(1:5), histogram = TRUE, pch=21)
pairs.panels(datoslunes, bg="yellow",pch=21,main="Lunes")

#Martes
datosmartes<- datosdoce1 %>% filter(dayofweek==1)
pairs.panels(datosmartes, bg="yellow",pch=21,main="Martes")

#miercoles
datosmiercoles<- datosdoce1 %>% filter(dayofweek==2)
pairs.panels(datosmiercoles, bg="yellow",pch=21,main="Miercoles")

#jueves
datosjueves<- datosdoce1 %>% filter(dayofweek==3)
pairs.panels(datosjueves, bg="yellow",pch=21,main="Jueves")

#viernes
datosviernes<- datosdoce1 %>% filter(dayofweek==4)
pairs.panels(datosviernes, bg="yellow",pch=21,main="Viernes")

#sabado
datossabado<- datosdoce1 %>% filter(dayofweek==5)
pairs.panels(datossabado, bg="yellow",pch=21,stars=TRUE,main="Sábado a las 12 horas", cex.labels=)

#domingo
datosdomingo<- datosdoce %>% filter(dayofweek==6)
