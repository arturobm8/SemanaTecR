getwd()
setwd("/Users/Public/Escritorio/SemanaTecR/retoAnalitica/Codigo")
getwd()
data=read.csv("./GlobalLandTemperaturesByCountry.csv")
View(data)

sum(is.na(data$AverageTemperature))

newCData<-data[!is.na(data$AverageTemperature),]
sum(is.na(newCData$AverageTemperature))
sum(is.na(newCData$AverageTemperatureUncertainty))
View(newCData)

pais1<-subset(newCData,newCData$Country=="Mexico")
pais2<-subset(newCData,newCData$Country=="Laos")
pais3<-subset(newCData,newCData$Country=="Norway")
summary(pais1$AverageTemperature)
summary(pais2$AverageTemperature)
summary(pais3$AverageTemperature)
View(pais1)
View(pais2)
View(pais3)

hist(pais1$AverageTemperature)
hist(pais2$AverageTemperature)
hist(pais3$AverageTemperature)

max(pais1$AverageTemperature)
min(pais1$AverageTemperature)
max(pais2$AverageTemperature)
min(pais2$AverageTemperature)
max(pais3$AverageTemperature)
min(pais3$AverageTemperature)

boxplot(pais1$AverageTemperature)
boxplot(pais2$AverageTemperature)
boxplot(pais3$AverageTemperature)

max(pais1$AverageTemperature)-min(pais1$AverageTemperature)
max(pais2$AverageTemperature)-min(pais2$AverageTemperature)
max(pais3$AverageTemperature)-min(pais3$AverageTemperature)

var(pais1$AverageTemperature)
var(pais2$AverageTemperature)
var(pais3$AverageTemperature)

pais1$dt <- as.Date(pais1$dt)
plot(pais1$dt, pais1$AverageTemperature, type = "l", 
     xlab = "Fecha", ylab = "Temperatura Promedio", 
     main = "Cambio de Temperatura a lo largo del Tiempo")

pais2$dt <- as.Date(pais2$dt)
plot(pais2$dt, pais2$AverageTemperature, type = "l", 
     xlab = "Fecha", ylab = "Temperatura Promedio", 
     main = "Cambio de Temperatura a lo largo del Tiempo")

pais3$dt <- as.Date(pais3$dt)
plot(pais3$dt, pais3$AverageTemperature, type = "l", 
     xlab = "Fecha", ylab = "Temperatura Promedio", 
     main = "Cambio de Temperatura a lo largo del Tiempo")
