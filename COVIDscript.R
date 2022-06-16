##______________________________________________________________
##                UNIVERSIDAD NACIONAL DE COLOMBIA 
##                FACULTAD DE CIENCIAS ECON?MICAS
##______________________________________________________________

remove(list = ls())

#Paquetes -----------------------

library(scales)
library(readr)
library(tidyverse)
library(gdata)
library(stargazer)
library(gap)
library(GGally)
library(car)
library(corrplot)
library(lmtest)
library(tseries)
library(strucchange)
library(sandwich)
library(readxl)
library(tidyr)
library(dplyr)
library(zoo)
library(xts)
library(PerformanceAnalytics)
library(ggplot2)

#Importación de data---------------

setwd("C:/Users/ASUS/Desktop/Universidad/PronosticosCOVID")
Datos<- read_excel('database.xlsx')

#Manipulación de datos--------------------

#Seleccionar sólo las variables que usaremos
#Agregar INSTI, (ZOOM y país)
Datos1 <- Datos %>%
  select('INSTI', 'ZOOM', 'PAIS', 'FECHA', 'Crec.PIB.2020', 'MODA_ENE20',
         'MEDIA_ENE20', 'DIST_PRICONTG',	'DIST_CUAREN',
         'DIST_AISLINTEL',	'DIST_APER',	'DIST_NNORMAL',
         'ACUM_CONFIRM',	'CONFIRM_DAILY',	'DEAD_ACUM',
         'DEAD_DAILY', 'NEW_CASES',
         'STR_INDEX',	'MONETARY_IT',	'UNEMP',	'COLCAP',
         "COLCAP - \r\nAVER. GROWTH", "PETROLEO",
         "PETROLEO -\r\nAVER GROWTH", "MEDIA_PRONOS_ANTE")

#Eliminar NA
Datos1 <- as.data.frame(drop_na(Datos1))

#Eliminando datos atípicos------------------

Datos2 <- Datos1[Datos1$Crec.PIB.2020 >= -0.1, ]
Datos2$FECHA <- as.Date(Datos2$FECHA)

#Estadística descriptiva-----------------------

stargazer(Datos1, type = "text",omit.summary.stat = 'N',
          title = "Estadistica descriptiva")

#Correlación-----------------------

DatosEP <- Datos2 %>%
  select('Crec.PIB.2020', 'DIST_AISLINTEL',
         'ACUM_CONFIRM',	'CONFIRM_DAILY',	'DEAD_ACUM',
         'DEAD_DAILY', 'NEW_CASES',
         'STR_INDEX')

X11()
chart.Correlation(DatosEP, histogram = TRUE)

#Variables institucionales
Dinst<- Datos2 %>%
  select('Crec.PIB.2020', 'PAIS', 
         'DIST_AISLINTEL', 'INSTI')
chart.Correlation(Dinst, histogram = T)

#Variables institucionales
Ddecis<- Datos2 %>%
  select('Crec.PIB.2020', 'STR_INDEX', 
         'MEDIA_PRONOS_ANTE')
chart.Correlation(Ddecis, histogram = T)

cor<-round(cor(Datos_nc),2)
x11()
corrplot(cor(cor),
         tl.cex= 0.7,
         type ='upper',
         method = "circle",
         tl.col = "black") 
x11()
chart.Correlation(cor, histogram = TRUE)

#Después de descartar variables que están perfectamente correlacionadas
#para evitar multicolinealidad

Datos_nc <- Datos2 %>%
  select('INSTI', 'ZOOM', 'PAIS',
         'Crec.PIB.2020','DIST_AISLINTEL','ACUM_CONFIRM',
         'CONFIRM_DAILY',	'DEAD_ACUM',
         'DEAD_DAILY', 'NEW_CASES',
         'DEAD_ACUM',
         'STR_INDEX',	'MONETARY_IT',	'UNEMP',
         "COLCAP - \r\nAVER. GROWTH",'PETROLEO', 'COLCAP',
         "PETROLEO -\r\nAVER GROWTH", "MEDIA_PRONOS_ANTE")

Datos_nc$sd <- 
  Reg6$fitted.values - Datos_nc$Crec.PIB.2020

#Matriz de correlación sin combinaciones lineales 
#no multicolinealidad perfecta
x11()
chart.Correlation(Datos_nc, histogram = TRUE)
#Gráficos----------------------------

ggplot(data=Datos2)+
  geom_point(aes(x = FECHA , y = Crec.PIB.2020 ))+ 
  geom_smooth(method=lm, aes(x = FECHA , y = Crec.PIB.2020 )) +
  labs(x= 'Fecha', y= 'Pronóstico crecimiento', size=4)+
  theme(axis.title.x = element_text(face="bold", 
                                    vjust=-0.5, colour="black", 
                                    size=rel(1.2))) +
  theme(axis.title.y = element_text(face="bold", 
                                    vjust=1.5, 
                                    colour="black", 
                                    size=rel(1.2)))+ 
  geom_vline(xintercept=as.Date("2020-03-24"),
             size=1, color="darkred",
             linetype="dashed")+
  geom_text(aes(x=as.Date("2020-03-24")+3,
                label="Inicio cuarentena",
                y=-0.086), colour="black",
            angle=90, size=3.5)+
  geom_vline(xintercept=as.Date("2020-06-01"),
             size=1, color="darkred", 
             linetype="dashed")+
  geom_text(aes(x=as.Date("2020-06-01")+3,
                label="Aislamiento inteligente",
                y=-0.081), colour="black",
            angle=90, size=3.5)+
  geom_vline(xintercept=as.Date("2020-05-11"),
             size=1, color="darkred", 
             linetype="dashed")+
  geom_text(aes(x=as.Date("2020-05-11")+3,
                label="Apertura parcial",
                y=-0.087), colour="black",
            angle=90, size=3.5)+
  geom_vline(xintercept=as.Date("2020-09-02"),
             size=1, color="darkred", 
             linetype="dashed")+
  geom_text(aes(x=as.Date("2020-09-02")+-5,
                label="Nueva normalidad",
                y=-0.086), colour="black",
            angle=90, size=3.5) 

H2<-read_excel('Pronosticos_prepandemia.xlsx', col_names = FALSE)
H2<-rename(H2, var=...1,pronos=...2)
H2<-drop_na(H2)
#hist
x11()
par(mfrow=c(1,2))
hist(H2$pronos, main= 'Pronósticos pre-pandemia', 
     xlab = 'Pronóstico',
     ylab= 'Frecuanecia',
     xlim= c(0.022,0.04),
     ylim= c(0,20),
     col='steelblue')+
  lines(density(H2$pronos, adjust=2),
        lty="dotted", col="darkgreen", lwd=2)
hist(Datos1$Crec.PIB.2020, main= 'Pronósticos durante pandemia', 
     xlab = 'Pronóstico',
     ylab= 'Frecuanecia',
     xlim= c(-0.18,0.04),
     col='steelblue')+
  lines(density(Datos1$Crec.PIB.2020, adjust=2), 
        lty="dotted", col="darkgreen", lwd=2)

par(mfrow=c(2,1))
ggplot(data=Datos_nc)+
  geom_point(aes(x = DIST_AISLINTEL , y = abs(sd) ))+ 
  ggtitle('Diagrama de disperción')+
  geom_smooth(method=lm, aes(x = DIST_AISLINTEL , y = abs(sd) ))

#Estadística descrip Datanc

stargazer(Datos_nc, type = "text",omit.summary.stat = 'N',
          omit = c('ZOOM','CONFIRM_DAILY',
                   "COLCAP - \r\nAVER. GROWTH",
                   "PETROLEO -\r\nAVER GROWTH"),
          title = "Estadistica descriptiva")

#mco----------------------------

#Institucionles
Reg1<- lm(Crec.PIB.2020 ~ INSTI + PAIS + DIST_AISLINTEL, data = Datos_nc)

#Institucionales y epidemiológicas
Reg2<- lm(Crec.PIB.2020 ~ INSTI + PAIS + DIST_AISLINTEL +
           DEAD_DAILY +
           NEW_CASES + ACUM_CONFIRM + DEAD_ACUM,
           data = Datos_nc)         

#Institucionales y económicas
Reg3<- lm(Crec.PIB.2020 ~ INSTI + PAIS + DIST_AISLINTEL +	
            MONETARY_IT +	UNEMP + 
            COLCAP +
            PETROLEO, data = Datos_nc)

#Institucionales y PRONOS_ANTE
Reg4<- lm(Crec.PIB.2020 ~ INSTI + PAIS + DIST_AISLINTEL +
            MEDIA_PRONOS_ANTE + STR_INDEX, data = Datos_nc)

#Mejor especificación
Reg5<- lm(Crec.PIB.2020 ~  PAIS + 	 
            MONETARY_IT +	
            PETROLEO +
            MEDIA_PRONOS_ANTE , data = Datos_nc)

#Significativas de las Reg 1-4
Reg6 <- lm(Crec.PIB.2020 ~  PAIS + DIST_AISLINTEL +
             MONETARY_IT +	UNEMP + 
             PETROLEO + STR_INDEX +
             MEDIA_PRONOS_ANTE, data = Datos_nc)


stargazer(Reg1, Reg2, Reg3, Reg4, Reg6, Reg5, type = "latex", digits=3, omit.stat = c("f", 'ser'))
