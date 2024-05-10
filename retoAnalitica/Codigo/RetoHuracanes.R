## Arturo Barrios Mendoza - A01168331
## Lucio Arturo Reyes Castillo - A01378985
## Leyberth Jaaziel Castillo Guerra - A01749505

## Se leen los datos y se crea una tabla con el csv
getwd()
setwd("/Users/Public/Escritorio/SemanaTecR/retoAnalitica/Codigo")
getwd()
data=read.csv("./CasoHuracanesCSV.csv")
View(data)

## Se muestra un resumen de las 4 variables
summary(data$Wind)
summary(data$Pressure)
summary(data$CO2)
summary(data$Population)

## Se revisa cuántos datos son iguales o menores a 0
sum(data$Wind<=0)
sum(data$Pressure<=0)

## Se eliminan los datos inválidos en una nueva tabla
newHur<-data[(data$Wind>0),]
newHur<-newHur[(newHur$Pressure>0),]

## Se revisa el número de datos inválidos en la nueva tabla
sum(newHur$Wind<=0)
sum(newHur$Pressure<=0)
newHur<-newHur[(newHur$Population>0),]
View(newHur)

## Se muestra de nuevo el resumen de las variables
summary(newHur$Wind)
summary(newHur$Pressure)
summary(newHur$CO2)
summary(newHur$Population)

## Boxplot de viento
boxplot(data$Wind,horizontal = TRUE,col = "skyblue")
title("Viento - Antes de limpieza")
boxplot(newHur$Wind,horizontal = TRUE,col = "lightgreen")
title("Viento - Después de limpieza")

## Boxplot de presión
boxplot(data$Pressure,horizontal = TRUE,col = "skyblue")
title("Presión - Antes de limpieza")
boxplot(newHur$Pressure,horizontal = TRUE,col = "lightgreen")
title("Presión - Después de limpieza")

## Boxplot de CO2
boxplot(data$CO2,horizontal = TRUE,col = "skyblue")
title("CO2 - Antes de limpieza")
boxplot(newHur$CO2,horizontal = TRUE,col = "lightgreen")
title("CO2 - Después de limpieza")

## Boxplot de población
boxplot(data$Population,horizontal = TRUE,col = "skyblue")
title("Población - Antes de limpieza")
boxplot(newHur$Population,horizontal = TRUE,col = "lightgreen")
title("Población - Después de limpieza")

## Se grafica el viento
plot(newHur$Wind,main="Viento",xlab="Indice",ylab="Viento",pch=3,col="blue")

## Se añade la librería de Performance Analytics
library(PerformanceAnalytics)

## Correlación de años y viento
cor(newHur$year,newHur$Wind)
corAV<-data.frame(newHur$year,newHur$Wind)
chart.Correlation(corAV)

## Correlación de años y presión
cor(newHur$year,newHur$Pressure)
corAP<-data.frame(newHur$year,newHur$Pressure)
chart.Correlation(corAP)

## Gráficas de años y viento, y años y presión
plot(newHur$year,newHur$Wind,main="Años y viento",xlab="Años",ylab="Viento",pch=3,col="blue")
plot(newHur$year,newHur$Pressure,main="Años y presión",xlab="Años",ylab="Presión",pch=3,col="blue")

## Gráficas de CO2 y de población
plot(newHur$CO2,main="CO2",xlab="Indice",ylab="CO2",pch=3,col="blue")
plot(newHur$Population,main="Población",xlab="Indice",ylab="Población",pch=3,col="blue")

## Correlación entre CO2 y población
plot(newHur$CO2,newHur$Population,main="CO2 y población",xlab="CO2",ylab="Población",pch=3,col="blue")
cor(newHur$CO2,newHur$Population)
corCP<-data.frame(newHur$CO2,newHur$Population)
chart.Correlation(corCP)

## Correlación entre años y CO2, y años y población
cor(newHur$year,newHur$CO2)
corAC<-data.frame(newHur$year,newHur$CO2)
chart.Correlation(corAC)
cor(newHur$year,newHur$Population)
corAPo<-data.frame(newHur$year,newHur$Population)
chart.Correlation(corAPo)

## Se crea una nueva tabla
newHur2<-newHur

## Se eliminan las columnas no válidas
newHur2$Ocean=NULL
newHur2$Name=NULL
newHur2$Fecha=NULL
newHur2$Month=NULL
newHur2$Status=NULL
newHur2$ID=NULL

View(newHur2)

## Se crea la gráfica de k means
kM3<-(kmeans(newHur2,3))
kM3

## Se añaden los paquetes y librerías para graficar el clúster
install.packages("factoextra")
library(factoextra)
library(ggplot2)
fviz_cluster(kM3,newHur2)

## Se escala la tabla y se grafica el clúster de 3 grupos
newHurEscalado<-scale(as.matrix(newHur2[,1:4]))
km32<-kmeans(newHurEscalado,3)
fviz_cluster(km32,newHurEscalado)

## Se grafica con 2 grupos
km2<-kmeans(newHurEscalado,2)
fviz_cluster(km2,newHurEscalado)

## Se grafica con 4 grupos
km4<-kmeans(newHurEscalado,4)
fviz_cluster(km4,newHurEscalado)

## Se crera la grágfica de correlación entre todas las variables
columnas_numericas <- sapply(newHur, is.numeric)
datos_numericos <- newHur[, columnas_numericas]
correlacionData <- cor(datos_numericos)
ruta_archivo <- "correlacion_hur.csv"
write.csv(correlacionData, file = ruta_archivo)
library(corrplot)
corrplot(correlacionData, method = "ellipse")

## Se crea una versión más corta de la tabla para tener 
## un gráfico de k means más claro
shortHur<-newHur2
View(shortHur)
shortHur$Clave=NULL
shortHur$Latitude=NULL
shortHur$Longitud=NULL
shortHur$Population=NULL
View(shortHur)
shortHur<-shortHur[1:20,]
View(shortHur)

## Se grafica el nuevo clúster
shortkm<-kmeans(shortHur,3)
fviz_cluster(shortkm,shortHur)

## Se crea otra tabla
newHur3<-newHur2

## Se eliminan las columnas que no se usarán
newHur3$Clave=NULL
newHur3$Time=NULL
newHur3$Latitude=NULL
newHur3$Longitud=NULL
newHur3$Population=NULL

View(newHur3)

## Se escala y se grafica con 3 clústers
newHurEscalado2<-scale(as.matrix(newHur3[,1:4]))
newkm3<-kmeans(newHurEscalado2,3)
fviz_cluster(newkm3,newHurEscalado2)

## Se grafica con 2 clústers
newkm2<-kmeans(newHurEscalado2,2)
fviz_cluster(newkm2,newHurEscalado2)

## Se grafica con 4 clústers
newkm4<-kmeans(newHurEscalado2,4)
fviz_cluster(newkm4,newHurEscalado2)

## Se grafican las variables con su rgresión lineal
regHur <- lm(newHur$CO2 ~ newHur$year)
plot(newHur$year, newHur$CO2, col = "blue")
abline(regHur, col = "red")
regHur1 <- lm(newHur$Population ~ newHur$year)
plot(newHur$year, newHur$Population, col = "blue")
abline(regHur1, col = "red")
regHur2 <- lm(newHur$CO2 ~ newHur$Population)
plot(newHur$Population, newHur$CO2, col = "blue")
abline(regHur2, col = "red")

## Se crean otras tablas con categorías específicas de tormentas
## para tormenta tropical, huracán y onda tropical
TShur<-data[data$Status==" TS",]
View(TShur)
HUhur<-data[data$Status==" HU",]
View(HUhur)
WVhur<-data[data$Status==" WV",]
View(WVhur)

## Se eliminan los datos inválidos de las nuevas tablas
TShur<-TShur[(TShur$Wind>0),]
TShur<-TShur[(TShur$Pressure>0),]
TShur<-TShur[(TShur$Population>0),]
View(TShur)

HUhur<-HUhur[(HUhur$Wind>0),]
HUhur<-HUhur[(HUhur$Pressure>0),]
HUhur<-HUhur[(HUhur$Population>0),]
View(HUhur)

WVhur<-WVhur[(WVhur$Wind>0),]
WVhur<-WVhur[(WVhur$Pressure>0),]
WVhur<-WVhur[(WVhur$Population>0),]
View(WVhur)

## Se seleccionan las primeras 10 filas de las 3 tablas
shortHU<-HUhur[1:10,]
View(shortHU)
shortTS<-TShur[1:10,]
View(shortTS)
shortWV<-WVhur[1:10,]
View(shortWV)

## Las 3 tablas se combinan en una nueva tabla
shortComb<-rbind(shortHU,shortTS,shortWV)
View(shortComb)

## Todas las columnas inválidas se eliminan de la nueva tabla
sult<-shortComb
sult$Clave=NULL
sult$Ocean=NULL
sult$ID=NULL
sult$Name=NULL
sult$Fecha=NULL
sult$Month=NULL
sult$Time=NULL
sult$Status=NULL
View(sult)

## Se realiza la nueva gráfica de k means, más clara que las anteriores
sultkm<-kmeans(sult,3)
fviz_cluster(sultkm,sult)
