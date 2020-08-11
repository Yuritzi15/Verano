#instalación de paquetes
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("psych")
#install.packages("ggplot2")

#cargar paquetes
library("readr")
library("tidyverse")
library(lubridate)
library(psych)
library(GGally)
library(ggplot2)

#cargar ruta
ruta<-choose.files()

#cargar archivo
datos<-read.csv(ruta)

#separar registros de las 12 hrs y hacer los campos en fecha
datosdoce<-datos %>% filter(hour==12) %>% mutate(fecha =make_date(year,month, dayofmonth))

#Selección de variables para correlación
datosdoce1<- datosdoce %>% select(dayofweek,CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)
datosdoce2<- datosdoce %>% select(CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)

ggpairs(datosdoce2 ,title="Todos los días a las 12",
        lower = list(continuous = wrap("points", color = "red", alpha = 0.5)))

#Lunes
datoslunes<- datosdoce1 %>% filter(dayofweek==0) %>% select(CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction) 
ggpairs(datoslunes ,title="Lunes a las 12",
        lower = list(continuous = wrap("points", color = "red", alpha = 0.5)), 
                     combo = wrap("box", color = "orange", alpha = 0.3))
    
#Martes
datosmartes<- datosdoce1 %>% filter(dayofweek==1) %>% select(CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)
ggpairs(datosmartes ,title="Martes a las 12",
        lower = list(continuous = wrap("points", color = "red", alpha = 0.5)), 
        combo = wrap("box", color = "orange", alpha = 0.3))

#miercoles
datosmiercoles<- datosdoce1 %>% filter(dayofweek==2)%>% select(CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)
ggpairs(datosmiercoles ,title="Miércoles a las 12",
        lower = list(continuous = wrap("points", color = "red", alpha = 0.5)), 
        combo = wrap("box", color = "orange", alpha = 0.3))

#jueves
datosjueves<- datosdoce %>% filter(dayofweek==3)%>% select(CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)
ggpairs(datosjueves ,title="Jueves a las 12",
        lower = list(continuous = wrap("points", color = "red", alpha = 0.5)), 
        combo = wrap("box", color = "orange", alpha = 0.3))

#viernes
datosviernes<- datosdoce %>% filter(dayofweek==4)%>% select(CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)
ggpairs(datosviernes ,title="Viernes a las 12",
        lower = list(continuous = wrap("points", color = "red", alpha = 0.5)), 
        combo = wrap("box", color = "orange", alpha = 0.3))

#sabado 
datossabado<- datosdoce %>% filter(dayofweek==5)%>% select(CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)
ggpairs(datossabado,title="Sábado a las 12",
        lower = list(continuous = wrap("points", color = "red", alpha = 0.5)), 
        combo = wrap("box", color = "orange", alpha = 0.3))


#domingo
datosdomingo<- datosdoce %>% filter(dayofweek==6)%>% select(CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)
ggpairs(datosdomingo ,title="Domingo a las 12",
        lower = list(continuous = wrap("points", color = "red", alpha = 0.5)), 
        combo = wrap("box", color = "orange", alpha = 0.3))

