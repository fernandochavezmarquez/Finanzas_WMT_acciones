# Pregunta de recuperacion metodos cuantitativos de finanzas

# Cargaremos los paquetes necesarios para el funcionamiento del
# programa

# Usaremos las siguientes paqueterias:
# hms,ggplot2, readr

# Cargamos las librerias
library (readr)
library (hms)
library (moments)
library (ggplot2)

# Cargamos el csv que usaremos, el cual contiene las columnas
# date, open, high, low, close, adj close y volumen
# Contiene datos historicos desde el 24 de agsto de 1972 hasta 
# el 17 de julio del 2021, precios diarios 

df<-read.csv('WMT.csv')

# Seleccionaremos solamente la columna Date y la de close
# la cual nos dice el precio al momento de cierre
df<-df[c('Date','Close')]

# Para poder realizar los calculos tenemos que limpiar la data
# eso incluye eliminar cualquier NA que podamos tener, ya que no
# nos interesan o son utiles
df<-na.omit(df)

# Modificamos el tipo de dato que tenemos en la columna, mejor 
# dicho, modificamos el formato de fecha, para que no nos
# genere problemas
df$Date<-as.Date(df$Date,format='%Y-%m-%d')

# Convertimos nuestra columna de Close a dato numerico
df$Close<-as.numeric(df$Close)

# Definimos un vector con colores que iremos usando
LC = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')

# Haremos los calculos para rendimientos diarios

# Graficamos usando plot los rendimientos diarios de WMT
plot(df$Date,df$Close,col=LC[2],type="l",main='WMT',xlab='Fecha',ylab='Precio')

# Creamos los rendimientos logaritimos usando diff que saca la 
# diferencia entre los datos deseados, usando lag = 1, para comparar
# de uno en uno, ademas de usar log para convertir en logaritmo
RLD <- diff(log(df$Close),lag=1)*100

# Creamos dataframe con los datos anteriormente creados
dfd <- data.frame(df$Date[2:nrow(df)],RLD)

# Usamos plot para graficar los rendimientos logariticos
plot(x=dfd$df.Date.2.nrow.df..,y=dfd$RLD,type='l',main='WMT',xlab='Fecha',ylab='Precio')

# Calculamos las medidas solicitadas, como mean, desviacion estandar
# kurtosis y sesgo, las guardamos en las variables
mRLD <- mean(RLD)
sdRLD<- sd(RLD)
kRLD <- kurtosis(RLD)
skRLD<- skewness(RLD)

# Creamos un vector para ver las medidas creadas
mRLD <- c(mRLD,sdRLD,kRLD,skRLD)
mRLD

# Graficamos el histograma(hist), con 100 bins, ademas le 
# agregamos el kernel, y si distribución normal. 
# Se agregaran con "lines" donde especificaremos que es la densidad
# de nuestro conjunto de datos RLD, ademas de "curve" que es la
# distribución normal.
hist(RLD, breaks=100,freq=FALSE,lwd=2, xlab ="Rendimientos logarítmicos",ylab ="", main = 'WMT')
lines(density(RLD),col=LC[2])
curve(dnorm(x,mean(RLD),sd(RLD)), from=-100, to=100,col=LC[1],add=TRUE)

# Para lo siguiente, usaremos cuantil-cuantil(QQ) para observar
# que tan cerca esta la distribución de nuestro conjunto 
# de datos
qqnorm(RLD,ylim=c(-10,10),col=LC[2])
qqline(RLD,col=LC[1])

# Creamos nuestro absoluto de RLD, usamos abs y lo guardamos en una variable
aRLD <- abs(RLD)
# Creamos nuestro diagrama de ACF usando acf
acf(RLD,col=LC[1],lwd=8,main='ACF')
acf(aRLD,col=LC[1],lwd=8,main='ACF')

# Haremos los calculos para rendimientos semanales

# Usaremos diff para sacar la diferencia, despues log para el 
# logaritmo, ademas de agregar un lag = 5, ya que tenemos que tomar en 
# cuenta solo los 5 dias de la semana (LMMJV)
RLS <- diff(log(df$Close),lag=5)*100

# Creamos un nuevo DF en el cual agregaremos los anteriores datos
# y ademas empezaremos desde el 6, ya que debemos de poder agregar
# el sigueinte dia para poder hacer la diferencia
dfs <- data.frame(x=df$Date[6:nrow(df)],y=RLS)

# Graficamos los rendimientos logaritmicos semanales
# Usamos plot, agregamos lo anterior de los 6 dias, ademas en Y 
# agregamos nuestros rendimiento y elegimos tipo "l" para usar 
# line
plot(x = df$Date[6:nrow(df)],y=RLS,type='l',main='WMT',xlab='Fecha',ylab='Precio')

# Creamos nuestras medidas
mRLS <- mean(RLS)
sdRLS<- sd(RLS)
kRLS <- kurtosis(RLS)
skRLS<- skewness(RLS)

# Creamos un vector con nuestras medidas
mRLS <- c(mRLS,sdRLS,kRLS,skRLS)
mRLS

# Creamos nuestro histograma para nuestros datos, usaremos 100 bins
# ademas agregaremos el kernel y la distribución normal
hist(RLS, breaks=100,freq=FALSE,lwd=2, xlab ="Rendimientos logarítmicos",ylab ="", main = 'WMT')
# Agregamos el kernel
lines(density(RLS),col='blue')
# Agregamos la distribución normal
curve(dnorm(x,mean(RLS),sd(RLS)), from=-100, to=100,col="red",add=TRUE)

# Agregamos nuestras graficas QQ 
qqnorm(RLS,ylim=c(-10,10))
qqline(RLS,col='blue')

aRLS <- abs(RLS)
acf(RLS,col='red',lwd=8,main='ACF')
acf(aRLS,col='red',lwd=8,main='ACF')

#Mensuales

# Creamos la variable number que almacenara los dias que tendremos que usar
number <-  21
# Calculamos la diferencia y logaritmos, ahora con 21 dias, para agrupar
# la informacion en meses
RLM <- diff(log(df$Close),lag=number)*100
RLM
# Para poder comparar debemos de usar nuestra variable number mas un dia
number2 <- number + 1
dfm <- data.frame(x=df$Date[number2:nrow(df)],y=RLM)

# Graficamos los rendimientos semanales
plot(x = df$Date[number2:nrow(df)],y=RLM,type='l',main='WMT',xlab='Fecha',ylab='Precio')

# Calculamos las medidas solicitads
mRLM <- mean(RLM)
sdRLM<- sd(RLM)
kRLM <- kurtosis(RLM)
skRLM<- skewness(RLM)
# Creamos un vector con las medidas
mRLM <- c(mRLM,sdRLM,kRLM,skRLM)
mRLM
# Graficamos ahora el histograma con las adecuaciones necesarias
# recordando usar 100 bins y agregar el kernel y la dist. normal
hist(RLM, breaks=100,freq=FALSE,lwd=2, xlab ="Rendimientos logarítmicos",ylab ="", main = 'WMT')
lines(density(RLM),col='blue')
curve(dnorm(x,mean=mean(RLM),sd=sd(RLM)), from=-100, to=100,col="red",add=TRUE)

# Graficamos el cuantil-cuantil(QQ) usando RLM
qqnorm(RLM,ylim=c(-10,10))
qqline(RLM,col='blue')
# Graficamos el autocorrelograma, usando los datos normale s
# y los absolutos
aRLM <- abs(RLM)
acf(RLM,col='red',lwd=8,main='ACF')
acf(aRLM,col='red',lwd=8,main='ACF')


# Calcularemos los rendimientos anuales

# Definimos la variable numbera con 360 referenciando el año
# Creamos otra vez la diferencia con el logaritmo y el lag 
# de 360, usamos *100 para sacar el porcentaje
number <-  360
RLA <- diff(log(df$Close),lag=number)*100
RLA
# Creamos otra variable para poder empezar en el siguiente año
number2 <- number + 1
# Creamos dfa como el data frame con informacion lo anterior
dfa <- data.frame(x=df$Date[number2:nrow(df)],y=RLA)
# Graficamos los rencimientos anuales
plot(x = df$Date[number2:nrow(df)],y=RLA,type='l',main='WMT',xlab='Fecha',ylab='Precio')
# Creamos las medidas (media, desviacion estandar, sesgo y curtosis)
# lo guardamos en una variable
mRLA <- mean(RLA)
sdRLA<- sd(RLA)
kRLA <- kurtosis(RLA)
skRLA<- skewness(RLA)
# Guardamos todas las medidas en un vector
mRLA <- c(mRLA,sdRLA,kRLA,skRLA)
mRLA
# Graficamos el histograma, con 100 bins y añadimos el kernel
# y la distribucion normal
hist(RLA, breaks=100,freq=FALSE,lwd=2, xlab ="Rendimientos logarítmicos",ylab ="", main = 'WMT')
lines(density(RLA),col='blue')
curve(dnorm(x,mean=mean(RLA),sd=sd(RLA)), from=-100, to=100,col="red",add=TRUE)
# Graficamos con QQ y añadimos los limites para una mejor 
# visualización 
qqnorm(RLA,ylim=c(-10,10))
qqline(RLA,col='blue')
# Graficamos nuestro autocorrelograma
aRLA <- abs(RLA)
acf(RLA,col='red',lwd=8,alpha=.6,main='ACF')
acf(aRLA,col='red',lwd=8,alpha=.6,main='ACF')

# Terminamos de calcular todo con rendimientos diarios, semanales
# mensuales y anuales

