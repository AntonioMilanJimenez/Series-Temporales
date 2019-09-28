#Antonio Manuel Milán Jiménez
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
########################################################################


#Se reagrupan todos los datos que se habían dividido
datos <- do.call("rbind", listaGrupos)

#Se comprueba si ya no hay valores perdidos
patron <- mice::md.pattern(x=datos[,-c(1,2,8,9,10,11,17)],plot = TRUE)

#Se eliminan las variables que ya no aportan información
datos <- datos[,-c(1,2,8,9,10,11)]
colnames(datos)



meses = unique(datos$FechaSinDia)
#Función encargada de obtener los valores medios de las variables para un mes dado
mediaMes <-function(mes,datos){
  
  grupo <- datos %>% filter(FechaSinDia == mes)
  #Se calcula la media para Tmax, Tmin y Tmed
  mesCompleto <- colMeans(grupo[,-c(2,4,11)])
  #Añadimos valores por defecto para las variables restantes
  mesCompleto$FechaSinDia <- mes
  mesCompleto$HTmax <- "15:00"
  mesCompleto$HTmin <- "04:00"
  unlist(mesCompleto)  
}

#Se obtiene la media de las variables para cada mes
datosEscalaMensual <- lapply(meses,mediaMes,datos)

#Se construyen los datos en escala mensual a partir de las medias calculadas
datosEscalaMensual <- data.frame(matrix(unlist(datosEscalaMensual),nrow=58,byrow=TRUE))
colnames(datosEscalaMensual) <- c("Tmax","Tmin","Tmed","TPrec","Prec1","Prec2","Prec3","Prec4",
                                  "FechaSinDia","HTmax","HTmin")
cat("Dimensiones de datos en escala mensual:",dim(datosEscalaMensual))


#Separación de los datos en train y test
serieMensual <- datosEscalaMensual[,"Tmax"]

datosEscalaMensualTrain <- datosEscalaMensual[1:46,]
datosEscalaMensualTest <- datosEscalaMensual[47:58,]

serieMensualTrain <- as.numeric(as.character(datosEscalaMensualTrain[,"Tmax"]))
serieMensualTest <- as.numeric(as.character(datosEscalaMensualTest[,"Tmax"]))

#Cálculo de los tiempos para train y test
tiempoTrainMensual <- 1:length(serieMensualTrain)
tiempoTestMensual <- (tiempoTrainMensual[length(tiempoTrainMensual)]+1):(tiempoTrainMensual[
  length(tiempoTrainMensual)]+length(serieMensualTest))

#Se visualiza en rojo los datos de validación
plot.ts(serieMensualTrain, xlim=c(1,tiempoTestMensual[length(tiempoTestMensual)]))
lines(tiempoTestMensual, serieMensualTest, col="red")

#Se establece una frecuencia de 12 correspondiente a los meses del año
serieMensualTrain.ts <- ts(serieMensualTrain, frequency = 12)

#Descomposición de la serie
plot(decompose(serieMensualTrain.ts))

#Transformación logarítmica de la serie
serieMensualTrain.log <- log(as.numeric(serieMensualTrain))
serieMensualTest.log <- log(as.numeric(serieMensualTest))
serieMensualTrain.ts <- log(serieMensualTrain.ts)
plot(decompose(serieMensualTrain.ts))


############# TENDENCIA #######################
#Se ajusta un modelo más complejo sobre la tendencia
parametros.H1 <- lm(serieMensualTrain.log ~ tiempoTrainMensual+I(tiempoTrainMensual^2)
                    +I(tiempoTrainMensual^3)+I(tiempoTrainMensual^4))

#Ajuste complejo de la tendencia
TendEstimadaTrComplex.H1 <- parametros.H1$coefficients[1]+tiempoTrainMensual*
  parametros.H1$coefficients[2]+tiempoTrainMensual^2*parametros.H1$coefficients[3]+
  tiempoTrainMensual^3*parametros.H1$coefficients[4]+tiempoTrainMensual^4*
  parametros.H1$coefficients[5]

#Ajuste simple
TendEstimadaTr.H1 <- 3.03

#Se visualizan ambos ajustes
plot.ts(serieMensualTrain.log)
lines(tiempoTrainMensual, rep(TendEstimadaTr.H1,length(tiempoTrainMensual)), col = "blue")
lines(tiempoTrainMensual, TendEstimadaTrComplex.H1, col = "red")


############### ESTACIONALIDAD #######################
estacionalidadMensual.H1 <- decompose(serieMensualTrain.ts)$seasonal[1:12]

#Dado que la división entre la longitud de la serie y de la estacionalidad  no es exacta, 
#se utiliza "rep_len" para repetir la estacinalidad hasta la longitud indicada
estacionalidadMensualRepetida <- 
  rep_len(estacionalidadMensual.H1,length(serieMensual))[1:length(serieMensualTrain)]

#Se calculan las estacionalidades para los datos de validación
estacionalidadMensualRepetidaTest <- 
  rep_len(estacionalidadMensual.H1,length(serieMensual))[(length(serieMensualTrain)+1):length(
    serieMensual)]

#Se elimina la estacinalidad calculada
serieMensualTrainSinEst.log <- serieMensualTrain.log - estacionalidadMensualRepetida
serieMensualTestSinEst.log <- serieMensualTest.log - estacionalidadMensualRepetidaTest

#Se muestra las serie sin estacionalidad
plot.ts(serieMensualTrainSinEst.log)



############# ESTACIONARIEDAD ################
#Test de Dickey-Fuller aumentado sobre serie mensual para determinar si es estacionaria
adf.test(serieMensualTrainSinEst.log)



##################### Predicción sobre los datos de validación ###################
#Gráficas ACF y PACF
acf(serieMensualTrainSinEst.log)
pacf(serieMensualTrainSinEst.log)

#Modelo ARIMA(1,0,0)
modeloAR <- arima(serieMensualTrainSinEst.log, order=c(1,0,0))
#Valores ajustados
valoresAjustadosAR <- serieMensualTrainSinEst.log + modeloAR$residuals

#Se calculan las predicciones para los proximos 12 meses
prediccionesAR <- predict(modeloAR, n.ahead = 12)
valoresPredichosAR <- prediccionesAR$pred

#Se calcula el error cuadrático acumulado del ajuste en train y test
sum((modeloAR$residuals)^2)
sum((valoresPredichosAR-serieMensualTestSinEst.log)^2)


#Modelo ARIMA(0,0,2)
modeloMA <- arima(serieMensualTrainSinEst.log, order=c(0,0,2))
#Valores ajustados
valoresAjustadosMA <- serieMensualTrainSinEst.log + modeloMA$residuals

#Se calculan las predicciones para los proximos 12 meses
prediccionesMA <- predict(modeloMA, n.ahead = 12)
valoresPredichosMA <- prediccionesMA$pred

#Se calcula el error cuadrático acumulado del ajuste en train y test
sum((modeloMA$residuals)^2)
sum((valoresPredichosMA-serieMensualTestSinEst.log)^2)

#Visualización de la predicción en la componente irregular
plot.ts(serieMensualTrainSinEst.log, xlim=c(1,tiempoTestMensual[length(tiempoTestMensual)]), ylim=c(2.8,3.3))
lines(valoresAjustadosAR, col="blue")
lines(tiempoTestMensual, serieMensualTestSinEst.log, col="red")
lines(tiempoTestMensual, valoresPredichosAR, col="blue")


#Test de Box-Pierce para determinar aleatoriedad en los residuos
Box.test(modeloAR$residuals)

#Histograma de los residuos del modelo
hist(modeloAR$residuals, col="blue", prob=T)
lines(density(modeloAR$residuals))

#Tests de Jarque Bera y Shapiro Wilk para determinar normalidad en los residuos
jarque.bera.test(modeloAR$residuals)
shapiro.test(modeloAR$residuals)

#Visualización de la predicción en los datos de validación
plot.ts(as.numeric(as.character(datosEscalaMensual[,"Tmax"])))
lines(exp(valoresAjustadosAR + estacionalidadMensualRepetida), col="blue")
lines(exp(valoresPredichosAR + estacionalidadMensualRepetidaTest), col="red")



##################### Predicción para los meses de Marzo y Abril 2018 ###########################3
#Se construye la serie mensual completa
serieMensual <- as.numeric(as.character(datosEscalaMensual[,"Tmax"]))
serieMensual.ts <- ts(serieMensual, frequency = 12)

#Se realiza la transformación logarítmica necesaria para la serie mensual
serieMensual.log <- log(serieMensual)
serieMensual.ts <- log(serieMensual.ts)

#Se calcula la estacionalidad
estacionalidadMensual <- decompose(serieMensual.ts)$seasonal[1:12]
estacionalidadMensualRepetida <- rep_len(estacionalidadMensual,length(serieMensual.log))

#Se eliminan la estacionalidad
serieMensual.log <- serieMensual.log - estacionalidadMensualRepetida


#Se construye el modelo para la serie mensual
modeloMensual <- arima(serieMensual.log,order=c(1,0,0))
valoresAjustadosMensual <- serieMensual.log + modeloMensual$residuals
#Se predicen los próximos 2 meses
valoresPredichosMensual <- predict(modeloMensual, n.ahead=2)$pred


#Se suman las estacionalidades teniendo en cuenta los 2 meses proximos predichos
valoresAjustadosMensual <- valoresAjustadosMensual + rep_len(estacionalidadMensual,length(
  serieMensual)+2)[1:length(serieMensual)]
valoresPredichosMensual <- valoresPredichosMensual + rep_len(estacionalidadMensual,length(
  serieMensual)+2)[(length(serieMensual)+1):(length(serieMensual)+2)]

#Se deshace la transformación logarítmica realizada
valoresAjustadosMensual <- exp(valoresAjustadosMensual)
valoresPredichosMensual <- exp(valoresPredichosMensual)

#Visualización de la predicción realizada para Marzo y Abril 2018
plot.ts(as.numeric(as.character(datosEscalaMensual[,"Tmax"])), xlim=c(1,length(serieMensual)+2))
lines(valoresAjustadosMensual, col="blue")
lines(valoresPredichosMensual, col="red")

#Valores predichos finales
cat("Valores predichos de temperatura máxima para los meses de Marzo y Abril 2018 en Lanjarón 
    -->",valoresPredichosMensual)
