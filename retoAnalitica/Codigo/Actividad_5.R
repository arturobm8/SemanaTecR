library(PerformanceAnalytics)
library(corrplot)

install.packages("factoextra")
library(factoextra)

data("mtcars")
View(mtcars)

dataCars<-data.frame(mtcars)
corrCars<-cor(dataCars)
corrplot(corrCars,method="ellipse")

cor(dataCars$mpg,dataCars$cyl) ## Bueno
cor(dataCars$mpg,dataCars$disp) ## Bueno
cor(dataCars$mpg,dataCars$hp) ## Bueno
cor(dataCars$mpg,dataCars$wt) ## Bueno

cor(dataCars$cyl,dataCars$disp) ## Bueno
cor(dataCars$cyl,dataCars$hp) ## Bueno
cor(dataCars$cyl,dataCars$wt) ## Bueno
cor(dataCars$cyl,dataCars$vs) ## Bueno

cor(dataCars$disp,dataCars$hp) ## Bueno
cor(dataCars$disp,dataCars$wt) ## Bueno
cor(dataCars$disp,dataCars$vs)

cor(dataCars$hp,dataCars$qsec)
cor(dataCars$hp,dataCars$vs)
cor(dataCars$hp,dataCars$carb)

cor(dataCars$drat,dataCars$am)
cor(dataCars$drat,dataCars$gear)

cor(dataCars$wt,dataCars$am)

cor(dataCars$qsec,dataCars$vs)

cor(dataCars$am,dataCars$gear) ## Bueno


cor1<-(data.frame(dataCars$mpg,dataCars$cyl))
chart.Correlation(cor1)

cor2<-(data.frame(dataCars$mpg,dataCars$disp))
chart.Correlation(cor2)

cor3<-(data.frame(dataCars$mpg,dataCars$hp))
chart.Correlation(cor3)

cor4<-(data.frame(dataCars$mpg,dataCars$wt))
chart.Correlation(cor4)

cor5<-(data.frame(dataCars$cyl,dataCars$disp))
chart.Correlation(cor5)

cor6<-(data.frame(dataCars$cyl,dataCars$hp))
chart.Correlation(cor6)

cor7<-(data.frame(dataCars$cyl,dataCars$wt))
chart.Correlation(cor7)

cor8<-(data.frame(dataCars$cyl,dataCars$vs))
chart.Correlation(cor8)

cor9<-(data.frame(dataCars$disp,dataCars$hp))
chart.Correlation(cor9)

cor10<-(data.frame(dataCars$disp,dataCars$wt))
chart.Correlation(cor10)

cor11<-(data.frame(dataCars$am,dataCars$gear))
chart.Correlation(cor11)


data("mtcars")
View(mtcars)
mtcars$Species = NULL
kM <- kmeans(mtcars,3)
kM

install.packages("factoextra")
library(factoextra)
library(ggplot2)
fviz_cluster(kM,mtcars)

mtcarsEscalado <- scale(as.matrix(factoextra[, 1:4]))
kM <- kmeans(mtcarsEscalado,3)
fviz_cluster(kM,mtcarsEscalado)
