#install.packages("readr")
#cargar paquete
#library("readr")
#ruta<-choose.files()
#datos<-read.csv(ruta)

#install.packages("ggplot2")
#library(ggplot2)
qplot(pressure, CO, data = datos, ylab = "CO", xlab = "Presion", log="X")
  