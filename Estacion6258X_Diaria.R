#Antonio Manuel Milán Jiménez  77449165T
#antoniomj@correo.ugr.es
#Ejercicio de trabajo autónomo. Series temporales. Curso 2018-2019

library(Hmisc)
library(dplyr)
library(mice)

#Se cargan los datos
datos <- read.csv("6258X.csv", sep=";")

#Descripción de los datos
dim(datos)
colnames(datos)
Hmisc::describe(datos)

ggplot(datos[1:25,],aes(x=Fecha, y=Tmax, group=1)) + geom_point() + geom_line() +
  scale_x_discrete(breaks=c("2013-05-05","2013-05-10","2013-05-15","2013-05-20","2013-05-25","2013-05-30")) +labs(title="Temperatura máxima Mayo 2013")


#Funciones encargadas de detectar outliers mediante el rango intercuartil con un coeficiente dado para una variable concreta
vector_es_outlier_IQR = function (datos, indice.de.columna, coef = 1.5){
  columna.datos = datos[,indice.de.columna]
  #Se obtienen cuartiles primero y tercero
  cuartil.primero = quantile(columna.datos,na.rm=TRUE)[2]  
  cuartil.tercero = quantile(columna.datos,na.rm=TRUE)[4] 
  #Rango intercuartil
  iqr = cuartil.tercero - cuartil.primero
  #Se definen los umbrales en función del rango intercuartil y el coeficient
  extremo.superior.outlier = (iqr * coef) + cuartil.tercero
  extremo.inferior.outlier = cuartil.primero - (iqr * coef)
  #Se determinan como outliers aquellos que superen algun umbral, superior o inferior
  es.outlier  = columna.datos > extremo.superior.outlier |
    columna.datos < extremo.inferior.outlier
  return (es.outlier)
}

vector_claves_outliers_IQR = function(datos, indice, coef = 1.5){
  columna.datos = datos[,indice]
  vector.de.outliers = vector_es_outlier_IQR(datos, indice, coef)
  #Se devuelven los indices de aquellos datos detectados como outliers
  return (which(vector.de.outliers  == TRUE))
}



#Función encargada de devolver el año y el mes de la fecha
obtenerMes <- function(fecha){
  substr(toString(fecha),1,7)
}

#Se construye una columna auxiliar para hacer la agrupación que será eliminada posteriormente
listaAux <- unlist(list(datos[,2]))
nuevaColumna <- unlist(lapply(listaAux,obtenerMes))
datos$FechaSinDia = nuevaColumna


#Se obtienen todos los meses en los que se han registrado los datos
meses = unique(datos$FechaSinDia)

#################################### Corrección de anomalías ##############################################
#Función encargada de detectar y corregir las anomalías para un mes dado
corregirAnomalias <- function(mes){
  
  #Función encargada de corregir una anomalía
  eliminarAnomalias <- function(outlier,columna,miGrupo){
    
    ind1 = outlier
    ind2 = outlier
    
    #Se comprueba si el valor anterior existe
    if(outlier > 1){
      if(is.na(miGrupo[(outlier-1),columna]) == FALSE){
        ind1 = outlier-1
      }
    }
    
    #Se comprueba que existe el valor posterior
    if(outlier < length(miGrupo[,1])){
      if(is.na(miGrupo[(outlier+1),columna]) == FALSE){
        ind2 = outlier+1
      }
    }
    
    #Se obtiene la media de ambos valores
    valorMedio <- mean(c(miGrupo[ind1,columna],miGrupo[ind2,columna]))
    miGrupo[outlier,columna] <- valorMedio
    
    #Se corrige tambien la hora del registro en función del registro anterior
    if(columna=="Tmax"){
      miGrupo[outlier,"HTmax"] <- miGrupo[ind1,"HTmax"]
    }else if(columna=="Tmin"){
      miGrupo[outlier,"HTmin"] <- miGrupo[ind1,"HTmin"]
    }
    miGrupo
  }
  
  #Se obtienen los datos para un mes concreto
  grupo <- datos %>% filter(FechaSinDia == mes)
  
  #Se detectan y corrigen los outliers para la variable Tmax
  outliers = vector_claves_outliers_IQR(datos = grupo, "Tmax",coef=1.5)
  outTmax = length(outliers)
  for (outlier in outliers){
    grupo = eliminarAnomalias(outlier,"Tmax",grupo)
  }
  
  #Se detectan y corrigen los outliers para la variable Tmin
  outliers = vector_claves_outliers_IQR(datos = grupo, "Tmin",coef=1.5)
  outTmin = length(outliers)
  for (outlier in outliers){
    grupo = eliminarAnomalias(outlier,"Tmin",grupo)
  }
  
  #Se detectan y corrigen los outliers para la variable Tmed
  outliers = vector_claves_outliers_IQR(datos = grupo, "Tmed",coef=1.5)
  outTmed = length(outliers)
  for (outlier in outliers){
    grupo = eliminarAnomalias(outlier,"Tmed",grupo)
  }
  
  cat("Tratados", outTmax+outTmin+outTmed,"outliers para el mes",mes,"\n")
  grupo
}

#Se aplica la correción de anomalías para todos los meses de los datos
listaGrupos = lapply(meses,corregirAnomalias)



############################## Imputación de anomalías ################################################
#Función encargada de realizar la imputación para un mes dado
imputacion <- function(grupo){
  
  #Función encargada de imputar un valor perdido
  imputarValor <- function(valorPerdido,columna, miGrupo){
    ind1 = valorPerdido
    ind2 = valorPerdido
    
    #Se comprueba si el valor anterior existe
    if(valorPerdido > 1){
      if(is.na(miGrupo[(valorPerdido-1),columna]) == FALSE){
        ind1 = valorPerdido-1
      }
    }
    #Se comprueba que existe el valor posterior
    if(valorPerdido < length(miGrupo[,1])){
      if(is.na(miGrupo[(valorPerdido+1),columna]) == FALSE){
        ind2 = valorPerdido+1
      }
    }
    
    #Se realiza la media de los valores anterior y posterior si no son valores perdidos también
    if(is.na(miGrupo[ind1,columna]) == FALSE & is.na(miGrupo[ind2,columna]) == FALSE){
      valorMedio <- mean(c(miGrupo[ind1,columna],miGrupo[ind2,columna]))
      #Se corrige también la columna de la hora registrada
      if(columna==3){
        miGrupo[valorPerdido,"HTmax"] <- miGrupo[ind1,"HTmax"]
      }else if(columna==5){
        miGrupo[valorPerdido,"HTmin"] <- miGrupo[ind1,"HTmin"]
      }
      #Se toma el valor anterior si el posterior es un valor perdido
    }else if(is.na(miGrupo[ind1,columna]) == FALSE){
      valorMedio <- miGrupo[ind1,columna]
      
      if(columna==3){
        miGrupo[valorPerdido,"HTmax"] <- miGrupo[ind1,"HTmax"]
      }else if(columna==5){
        miGrupo[valorPerdido,"HTmin"] <- miGrupo[ind1,"HTmin"]
      }
      #Se toma el valos posterior si el anterior es un valor perdido
    }else if(is.na(miGrupo[ind2,columna]) == FALSE){
      valorMedio <- miGrupo[ind2,columna]
      
      if(columna==3){
        miGrupo[valorPerdido,"HTmax"] <- miGrupo[ind2,"HTmax"]
      }else if(columna==5){
        miGrupo[valorPerdido,"HTmin"] <- miGrupo[ind2,"HTmin"]
      }
      
    }else{  #Ambos valores son indeterminados, se toma la media de toda la columna de este mes
      valorMedio <- mean(miGrupo[,columna],na.rm = TRUE)
      #En caso de no tener registro anterior o posterior se establece una hora por defecto
      if(columna==3){
        miGrupo[valorPerdido,"HTmax"] <- '15:00'
      }else if(columna==5){
        miGrupo[valorPerdido,"HTmin"] <- '05:00'
      }
    }
    
    #Se establece el valor corregido
    miGrupo[valorPerdido,columna] <- valorMedio
    miGrupo
  }
  
  #La imputación se realiza para las variables "Tmax" "Tmin" "Tmed" "TPrec" "Prec1" "Prec2"
  #"Prec3" "Prec4"
  for (indice in c(3,5,7,12,13,14,15,16)){
    valoresPerdidos = which(is.na(grupo[,indice])==TRUE)
    for (valor in valoresPerdidos){
      grupo <- imputarValor(valor,indice,grupo)
    }
  }
  
  #Se devuelve el grupo sin valores perdidos
  grupo
}

#Se realiza la imputación de valores perdidos para cada mes
listaGrupos <- lapply(listaGrupos,imputacion)



#Se reagrupan todos los datos que se habían dividido
datos <- do.call("rbind", listaGrupos)

#Se comprueba si ya no hay valores perdidos
patron <- mice::md.pattern(x=datos[,-c(1,2,8,9,10,11,17)],plot = TRUE)

#Se eliminan las variables que ya no aportan información
datos <- datos[,-c(1,2,8,9,10,11)]
colnames(datos)


#Datos en escala diaria
datosEscalaDiaria <- datos

#División de los datos en Train y Test 
serieDiaria <- datosEscalaDiaria[,"Tmax"]

datosEscalaDiariaTrain <- datosEscalaDiaria[1:1389,]
datosEscalaDiariaTest <- datosEscalaDiaria[1390:1754,]

serieDiariaTrain <- datosEscalaDiariaTrain[,"Tmax"]
serieDiariaTest <- datosEscalaDiariaTest[,"Tmax"]


#Obtención de los tiempos train y test
tiempoTrainDiaria <- 1:length(serieDiariaTrain)
tiempoTestDiaria <- (tiempoTrainDiaria[length(tiempoTrainDiaria)]+1):(tiempoTrainDiaria[
  length(tiempoTrainDiaria)]+length(serieDiariaTest))


#Se visualiza en rojo los datos de validación
plot.ts(serieDiariaTrain, xlim=c(1,tiempoTestDiaria[length(tiempoTestDiaria)]))
lines(tiempoTestDiaria, serieDiariaTest, col="red")


library(tseries)
#Se establece una frecuencia de 365 correspondiente a los días del año
serieDiariaTrain.ts <- ts(serieDiariaTrain, frequency = 365)


#Componentes de la serie diaria
plot(decompose(serieDiariaTrain.ts))


################# TENDENCIA ####################################
#Se ajusta la tendencia con un ajuste más complejo
parametros.H1 <- lm(serieDiariaTrain ~ tiempoTrainDiaria+I(tiempoTrainDiaria^2)+
                      I(tiempoTrainDiaria^3)+I(tiempoTrainDiaria^4))

#Se obtiene la tendencia compleja
TendEstimadaTrComplex.H1 <- parametros.H1$coefficients[1]+tiempoTrainDiaria*
  parametros.H1$coefficients[2]+tiempoTrainDiaria^2*parametros.H1$coefficients[3]+
  tiempoTrainDiaria^3*parametros.H1$coefficients[4]+tiempoTrainDiaria^4*
  parametros.H1$coefficients[5]

#Tendencia simple
TendEstimadaTr.H1 <- 21.4

#Visualización de ambas tendencias
plot.ts(serieDiariaTrain)
lines(tiempoTrainDiaria, rep(TendEstimadaTr.H1,length(tiempoTrainDiaria)), col = "blue")
lines(tiempoTrainDiaria, TendEstimadaTrComplex.H1, col = "red")


###################### ESTACIONALIDAD ###################
estacionalidadDiaria.H1 <- decompose(serieDiariaTrain.ts)$seasonal[1:365]

#Dado que la división entre la longitud de la serie y de la estacionalidad  no es exacta, 
#se utiliza "rep_len" para repetir la estacionalidad hasta la longitud indicada
estacionalidadDiariaRepetida <- 
  rep_len(estacionalidadDiaria.H1,length(serieDiaria))[1:length(serieDiariaTrain)]

#Se calculan también la estacionalidad para los datos de validación
estacionalidadDiariaRepetidaTest <- 
  rep_len(estacionalidadDiaria.H1,length(serieDiaria))[(length(serieDiariaTrain)+1):length(
    serieDiaria)]

#Se elimina la estacinalidad
serieDiariaTrainSinEst <- serieDiariaTrain - estacionalidadDiariaRepetida
serieDiariaTestSinEst <- serieDiariaTest - estacionalidadDiariaRepetidaTest

#Se muestra la serie sin estacionalidad
plot.ts(serieDiariaTrainSinEst)


############## ESTACIONARIEDAD #####################
#Test de Dickey-Fuller aumentado sobre serie diaria para determinar si es estacionaria
adf.test(serieDiariaTrainSinEst)



################ Predicción sobre datos de validación #####################
#Gráficas ACF y PACF sobre serie diaria
acf(serieDiariaTrainSinEst)
pacf(serieDiariaTrainSinEst)

#Ajuste ARIMA(3,0,0)
modelo <- arima(serieDiariaTrainSinEst, order=c(3,0,0))
#Valores ajustados
valoresAjustados <- serieDiariaTrainSinEst + modelo$residuals

#Se calculan las predicciones
predicciones <- predict(modelo, n.ahead = 365)
valoresPredichos <- predicciones$pred

#Se calcula el error cuadrático acumulado del ajuste en train y test
sum((modelo$residuals)^2)
sum((valoresPredichos-serieDiariaTestSinEst)^2)


#Visualización de la predicción sobre la componente irregular
plot.ts(serieDiariaTrainSinEst, xlim=c(1,tiempoTestDiaria[length(tiempoTestDiaria)]))
lines(valoresAjustados, col="blue")
lines(tiempoTestDiaria, serieDiariaTestSinEst, col="red")
lines(tiempoTestDiaria, valoresPredichos, col="blue")


#Test de Box-Pierce para determinar aleatoriedad en los residuos
Box.test(modelo$residuals)
#Normalidad en los residuos
hist(modelo$residuals, col="blue", prob=T)
lines(density(modelo$residuals))

#Visualización de la predicción en los datos de validación
plot.ts(datosEscalaDiaria[,"Tmax"],xlim=c(1,1800))
lines(valoresAjustados + estacionalidadDiariaRepetida, col="blue")
lines(valoresPredichos + estacionalidadDiariaRepetidaTest, col="red")



################# Predicción para la primera semana Marzo 2018 ##########################
#Se construye la serie diaria completa
serieDiaria <- datosEscalaDiaria[,"Tmax"]
serieDiaria.ts <- ts(serieDiaria, frequency = 365)

#Se calcula la estacionalidad
estacionalidadDiaria <- decompose(serieDiaria.ts)$seasonal[1:365]
estacionalidadDiariaRepetida <- rep_len(estacionalidadDiaria,length(serieDiaria))

#Se elimina la estacionalidad
serieDiaria <- serieDiaria - estacionalidadDiariaRepetida

#Se construye el modelo para la serie diaria según lo estudiado anteriormente
modeloDiario <- arima(serieDiaria,order=c(3,0,0))
valoresAjustadosDiario <- serieDiaria + modeloDiario$residuals
#Se realiza la predicción para los próximos 7 días
valoresPredichosDiario <- predict(modeloDiario, n.ahead=7)$pred


#Se suma la estacionalidad teniendo en cuenta los 7 días proximos predichos
valoresAjustadosDiario <- valoresAjustadosDiario + rep_len(estacionalidadDiaria,length(
  serieDiaria)+7)[1:length(serieDiaria)]
valoresPredichosDiario <- valoresPredichosDiario + rep_len(estacionalidadDiaria,length(
  serieDiaria)+7)[(length(serieDiaria)+1):(length(serieDiaria)+7)]


#Visualizaciones del ajuste y predicción (en rojo)
plot.ts(datosEscalaDiaria[,"Tmax"], xlim=c(1,length(serieDiaria)+7))
lines(valoresAjustadosDiario, col="blue")
lines(valoresPredichosDiario, col="red")

plot.ts(datosEscalaDiaria[,"Tmax"], xlim=c(1650,length(serieDiaria)+7))
lines(valoresAjustadosDiario, col="blue")
lines(valoresPredichosDiario, col="red")

#Valores predichos finales
cat("Valores predichos de temperatura máxima para la primera semana de Marzo 2018 en Lanjarón
    -->",valoresPredichosDiario)