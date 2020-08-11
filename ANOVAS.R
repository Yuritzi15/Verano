#instalación de paquetes
install.packages("readr")
install.packages("tidyverse")


#cargar paquetes
library("readr")
library("tidyverse")


#cargar ruta
ruta<-choose.files()

#cargar archivo
datos<-read.csv(ruta)

datosanova<- datos %>% select(dayofweek,hour,CO,NO,NO2,NOX,O3,PM10,PM2_5,pressure,rainfall,humidity, SO2,solar,temperature,velocity,direction)
datosanova$dayofweek<-as.factor(datosanova$dayofweek)
datosanova$hour<-as.factor(datosanova$hour)

str(datosanova)

datosanova$dayofweek=factor(datosanova$dayofweek, levels = c(0:6), labels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes","Sábado", "Domingo"))
 

  
datosCO<- datosanova%>% select(dayofweek,hour,CO)
  anova2<-aov(CO~as.factor(dayofweek)*as.factor(hour),data = datosCO)
  res<-anova2$residuals
  summary(anova2)
  TukeyHSD(anova2)
  ggplot(data = datosCO, aes(x = dayofweek, y = CO, colour = hour, group = hour)) + 
    stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line",size = 1.7) + 
    labs(y = "CO", x="Día de la semana")+theme_gray()+  scale_fill_brewer(palette = "Pared")+
    ggtitle("Interacción de CO")
  
datosNO<- datosanova%>% select(dayofweek,hour,NO)
  anova3<-aov(NO~as.factor(dayofweek)*as.factor(hour),data = datosNO)
  res<-anova3$residuals
  summary(anova3)
  TukeyHSD(anova3)
  ggplot(data = datosNO, aes(x = dayofweek, y = NO, colour = hour, group = hour)) + 
    stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line",size = 1.7) + 
    labs(y = "NO", x="Día de la semana")+theme_gray()+
    ggtitle("Interacción de NO")
  
datosNOX<- datosanova%>% select(dayofweek,hour,NOX)
  anova4<-aov(NOX~as.factor(dayofweek)*as.factor(hour),data = datosNOX)
  res<-anova4$residuals
  summary(anova4)
  TukeyHSD(anova4)
  ggplot(data = datosNOX, aes(x = dayofweek, y = NOX, colour = hour, group = hour)) + 
    stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line",size = 1.7) + 
    labs(y = "NOX", x="Día de la semana")+theme_gray()+
    ggtitle("Interacción de NOX")
  
datosNO2<- datosanova%>% select(dayofweek,hour,NO2)
  anova5<-aov(NO2~as.factor(dayofweek)*as.factor(hour),data = datosNO2)
  res<-anova5$residuals
  summary(anova5)
  TukeyHSD(anova5)
  ggplot(data = datosNO2, aes(x = dayofweek, y = NO2, colour = hour, group = hour)) + 
    stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line",size = 1.7) + 
    labs(y = "NO2", x="Día de la semana")+theme_gray()+
    ggtitle("Interacción de NO2")
  
datosPM10<- datosanova%>% select(dayofweek,hour,PM10)
  anova6<-aov(PM10~as.factor(dayofweek)*as.factor(hour),data = datosPM10)
  res<-anova6$residuals
  summary(anova6)
  TukeyHSD(anova6)
  ggplot(data = datosPM10, aes(x = dayofweek, y = PM10, colour = hour, group = hour)) + 
    stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line",size = 1.7) + 
    labs(y = "PM10", x="Día de la semana")+theme_gray()+
    ggtitle("Interacción de PM10")
  
datosO3<- datosanova%>% select(dayofweek,hour,O3)
  anova7<-aov(O3~as.factor(dayofweek)*as.factor(hour),data = datosO3)
  res<-anova7$residuals
  summary(anova7)
  TukeyHSD(anova7)
  ggplot(data = datosO3, aes(x = dayofweek, y = O3, colour = hour, group = hour)) + 
    stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line",size = 1.7) + 
    labs(y = "Ozono", x="Día de la semana")+theme_gray()+
    ggtitle("Interacción de Ozono")
  
datosPM2_5<- datosanova%>% select(dayofweek,hour,PM2_5)
  anova8<-aov(PM2_5~as.factor(dayofweek)*as.factor(hour),data = datosPM2_5)
  res<-anova8$residuals
  summary(anova8)
  TukeyHSD(anova8)
  ggplot(data = datosPM2_5, aes(x = dayofweek, y = PM2_5, colour = hour, group = hour)) + 
    stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line",size = 1.7) + 
    labs(y = "PM2_5", x="Día de la semana")+theme_gray()+
    ggtitle("Interacción de PM2_5")
  
datosPM10<- datosanova%>% select(dayofweek,hour,PM10)
  anova9<-aov(PM10~as.factor(dayofweek)*as.factor(hour),data = datosPM10)
  res<-anova9$residuals
  summary(anova9)
  TukeyHSD(anova9)
  ggplot(data = datosPM2_5, aes(x = dayofweek, y = PM10, colour = hour, group = hour)) + 
    stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line",size = 1.7) + 
    labs(y = "PM10", x="Día de la semana")+theme_gray()+
    ggtitle("Interacción de PM10")
  
  rm(anova9)
  