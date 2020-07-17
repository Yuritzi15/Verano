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
 
