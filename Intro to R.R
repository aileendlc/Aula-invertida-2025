3+6 # suma
8*9 # multiplicacion 
10-8 # resta
5/2 # cociente
4^2 # potencia

# El orden importa
5 + 2 * 3
(5 + 2) * 3

# Operaciones logaritmicas
log(100)
log10(100)
log(100,10)

exp(2) #exponencial
sqrt(16) #raiz cuadrada/squareroot

b <- 5 #simbolo para asignar un valor
10*b
sqrt(b)

w <- 1:6 # Asignar un vector 
w

seq(1,6,0.5) #seq para secuencias con otros intervalos
x <- c("a", "a", "a", "b", "b", "b") # vector de caracteres

ww <- w > 4
w
w[ww]

# Matrices (tabla de datos)
y <- matrix(1:20, ncol = 4) #ncol es el numero de columnas
matrix(1:20, byrow = TRUE, ncol = 4) #organizado por filas
y <- matrix(1:20, byrow = TRUE, ncol = 4, dimnames = list(
  paste("r", 1:5, sep = ""), paste("c", 1:4, sep = ".")))
y #dimnames para ponerle nombres a las columnas y filas

#Data frames
z <- data.frame(x, w) 
z

# Listas
Z <- list(V.w = 2*w, V.x = x, M.y = log(y))
Z

# Indexar atributos
# se usa "$" para acceder a un data frame

# Graficas simples
z <- data.frame(grupo = sort(rep(c("a", "b"), 8)), var1 = rnorm(16))
z
plot(z$var1) #grafico de puntos             
hist(z$var1) #histograma
boxplot(var1 ~ grupo, data=z, 
        main="boxplot de prueba", 
        xlab = "grupo",
        ylab = "variable 1")

z$var2 <- (z$var1)^2
z <- z[order(z$var1), ]
par(mfrow=c(2,2))
plot(z$var1, z$var2, type="p", main="solo puntos")
plot(z$var1, z$var2, type="l", main="solo líneas")
plot(z$var1, z$var2, type="b", main="puntos y líneas")
plot(z$var1, z$var2, type="o", main="puntos y líneas\n sobrepuestos")

# R tiene una biblioteca que explica como hacer todo
help("par")
help.search("linear models")
help("hist")

#Ejemplo practico (analisis de series temporales)
cibmeteo <- read.table("R/cibmeteo.txt", header = TRUE, sep = "\t")
getwd()
setwd("/Users/user/git")
dim(cibmeteo) #cantidad de datos
names(cibmeteo) #nombre de las columnas de mi dataset
summary(cibmeteo) #resumen de mi dataset

plot(cibmeteo$temperature, type="l", col = "grey") # "l" significa linea, type es el tipo de grafico

attach(cibmeteo)
fecha <- paste(year, month, day, sep = "-")
fecha <- strptime(fecha, "%Y-%m-%d")
fecha_txt <- as.character(fecha)
taire <- as.data.frame(
  cbind(tapply(temperature, list(fecha_txt), min),
             tapply(temperature, list(fecha_txt), max),
             tapply(temperature, list(fecha_txt), mean)))
detach(cibmeteo)
colnames(taire) <- c('tmin', 'tmax', 'tavg')
attach(cibmeteo)
tapply(temperature, list(month, year), mean)
aggregate(temperature, by=list(mes=month, año=year), mean)
detach(cibmeteo)
Z <- c(4, 8, 9, 7, 8, 9, 4, 5)
Z.pmov <- c(7, 8, 8, 8, 7, 6)
Z.pmov <- c(NA, Z.pmov, NA)
Z.pmov

#crear nuestra primera función: para calcular el promedio móvil de las temperaturas de manera mas rápida y sencilla.
pmov <- function(x, k) {
  n <- length(x)
  y <- rep(NA, n)
  for (i in k:n)
    y[i-floor(k/2)] <- mean( x[(1+i-k):i] )
  y
}

tmean.pmov <- pmov(taire$tavg, 15)
plot(taire$tavg, type = "o",ylab = "temperatura (°C)",xlab="día",
     main = "Temp. diaria promedio en La Paz \n (junio 2006 a mayo 2007)",
     lty = 3, col="grey50", lwd = 1)
lines(tmean.pmov, lwd = 2, col = "blue")

anomalia <- tmean.pmov - mean(tmean.pmov, na.rm=TRUE)
anomalia[is.na(anomalia)] <- 0
dias <- unique(cibmeteo[, 1:3])
ndmes <- aggregate(dias$day, by = list(dias$month, dias$year), length)
ndmes
plot(anomalia, type="n", ylab = "anomalía de temperatura", xlab = "",
     xaxt = "n" )
polygon(c(1:364, 364:1), c(anomalia, rep(0, 364)), col="green")
abline(h=0, v=c(0, cumsum(ndmes$x)), col="black", lty=3)
axis(1, at=cumsum(ndmes$x)-ndmes$x/2, labels = c("jun","jul","ago",
                                                 "sep","oct","nov","dic","ene","feb","mar","abr","may"))
