#Seleccionar el directorio en el que se trabajar?? en setwd()
setwd("C://Users//Administrador//Desktop//Tesis Final")

#Instalacion de paqueteria requerida
install.packages("sf")
install.packages("corrplot")
install.packages("factorextra")
install.packages("FactoMineR")
install.packages("cluster")
install.packages("ape")
install.packages("devtools")
install.packages("factoextra")
install.packages("psych")
install.packages("ggcorrplot")
install.packages("lattice")
install.packages("ggplot2")

#librerias
library(sf) # leer el archivo shape
library(corrplot) # graficar matriz de correlacion
library("factorextra") #para usar PCA 
library(devtools)
library(factoextra)
library("FactoMineR") #PCA
library(cluster) # para cluster
library(ape) # usada para moran
library(psych)
library(ggcorrplot)
library(lattice)
library(ggplot2)

#cargar el archivo  shape
edo_mex <- st_read("C:\\Users\\Administrador\\Desktop\\Tesis Final\\MUN_VAR_ANALISIS.shp")

#Pregunta la clase del objeto espacial
class(edo_mex)

summary(edo_mex)

# exploración de los datos
# vamos a definir los nombres de las variables

Conectividad<- edo_mex$Z_IC
Indice_desarrollo_humano<- edo_mex$Z_IDH
Generación_residuos <- edo_mex$Z_RST
Indice_desarrollo_municipal <- edo_mex$Z_IDM
PIB <- edo_mex$Z_PIB
Población <- edo_mex$Z_POB

# graficamos los datos Indice de desarrollo humano -  Conectividad
plot(Indice_desarrollo_humano,Conectividad)
# agrega lineas punteadas en 0,0
abline(h=0,lty=3)
abline(v=0,lty=3)
#Datos de la linea de tendencia
m1<-lm(Indice_desarrollo_humano~Conectividad)
summary(m1)
#Agregar linea de tendencia
abline(lm(Indice_desarrollo_humano~Conectividad))

#PIB-Conectividad
plot(PIB,Conectividad)
abline(h=0,lty=3)
abline(v=0,lty=3)
#Datos de la línea de tendencia
m2<-lm(PIB~Conectividad)
summary(m2)
#Agregar linea de tendencia
abline(lm(PIB~Conectividad))

#Generacion_residuos-Conectividad
plot(Generación_residuos,Conectividad)
abline(h=0,lty=3)
abline(v=0,lty=3)

#Indice_desarrollo_municipal-Conectividad
plot(Indice_desarrollo_municipal,Conectividad)
abline(h=0,lty=3)
abline(v=0,lty=3)

#Población-Conectividad
plot(Población,Conectividad)
abline(h=0,lty=3)
abline(v=0,lty=3)



# graficar la distribución de los datos
factor2 <- min(Conectividad)
factor1 <- max(Conectividad)
par(mfrow=c(2,1))
boxplot(Conectividad, col="grey", main="Conectividad", ylab="Z", ylim=c(factor2*0.75,factor1*1.25), horizontal=TRUE, range=2)
hist(Conectividad, main="", xlim=c(factor2*0.75,factor1*1.25))
#
factor2 <- min(Indice_desarrollo_humano)
factor1 <- max(Indice_desarrollo_humano)
par(mfrow=c(2,1))
boxplot(Indice_desarrollo_humano, col="grey", main="Indice_desarrollo_humano", ylab="Z", ylim=c(factor2*0.75,factor1*1.25), horizontal=TRUE, range=2)
hist(Indice_desarrollo_humano, main="Indice_desarrollo_humano", xlim=c(factor2*0.75,factor1*1.25))
#
factor2 <- min(Generación_residuos)
factor1 <- max(Generación_residuos)
par(mfrow=c(2,1))
boxplot(Generación_residuos, col="grey", main="Generación_residuos", ylab="Z", ylim=c(factor2*0.75,factor1*1.25), horizontal=TRUE, range=2)
hist(Generación_residuos, main="Generación_residuos", xlim=c(factor2*0.75,factor1*1.25))
#
factor2 <- min(Indice_desarrollo_municipal)
factor1 <- max(Indice_desarrollo_municipal)
par(mfrow=c(2,1))
boxplot(Indice_desarrollo_municipal, col="grey", main="Indice_desarrollo_municipal", ylab="Z", ylim=c(factor2*0.75,factor1*1.25), horizontal=TRUE, range=2)
hist(Indice_desarrollo_municipal, main="Indice_desarrollo_municipal", xlim=c(factor2*0.75,factor1*1.25))
#
factor2 <- min(PIB)
factor1 <- max(PIB)
par(mfrow=c(2,1))
boxplot(PIB, col="grey", main="PIB", ylab="Z", ylim=c(factor2*0.75,factor1*1.25), horizontal=TRUE, range=2)
hist(PIB, main="PIB", xlim=c(factor2*0.75,factor1*1.25))
#
factor2 <- min(Población)
factor1 <- max(Población)
par(mfrow=c(2,1))
boxplot(Población, col="grey", main="Poblacion", ylab="Z", ylim=c(factor2*0.75,factor1*1.25), horizontal=TRUE, range=2)
hist(Población, main="Poblacion", xlim=c(factor2*0.75,factor1*1.25))
#
# Grafico de correlación entre todas las variables
pairs(~Conectividad + Indice_desarrollo_humano + Generación_residuos + Indice_desarrollo_municipal + PIB + Población)

summary(edo_mex)

pairs.panels(edo_mex[,10:14], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

ggcorplot(
  data = edo_mex[10:14],
  var_text_size = 5,
  cor_text_limits = c(5,10))



#creamos una tabla para guardar las variables de interes
datos<-data.table::as.data.table(Conectividad)
datos$IDH <- Indice_desarrollo_humano
datos$IDM <- Indice_desarrollo_municipal
datos$GR <- Generación_residuos
datos$PIB <- PIB
datos$POB <- Población


kmo <- function(x)
{
  r <- cor(x)
  r2 <- r^2
  i <- solve(r)
  d <- diag(i)
  p2 <- (-i/sqrt(outer(d,d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO,MSA=MSA))
  
}
# llamamos a la funcion kmo, devuleve el valor de kmo y los msa de cada variable
kmo(datos)
# matriz de correlacion de variables
cor_mat <- round(cor(datos),2)
#imprimir la matriz de correlacion como un mapa de colores
corrplot(cor_mat, type="upper", order = "hclust")


# hacemos pca para pasar a otro espacio
res.pca <- PCA(datos,graph = FALSE)
print(res.pca)
eigenvalores<- res.pca$eig 
#imprime los primeros eigen valores 
head(eigenvalores[,1:2])

# graficamos la varianza explicada por cada componente
fviz_screeplot(res.pca,ncp=6,main="Varianza",xlab = "Componentes principales", ylab = "Porcentaje de varianza")


# valores importantes del pca
#correlación de las variables con las componentes
head(res.pca$var$coord)

#contribución de las variables en las componentes principales
head(res.pca$var$contrib)

#calidad de representacion
head(res.pca$var$cos2,5)

# graficar la calidad de representacion de las variables en las primeras 5 componenetes (a nosotros nos importan las primeras 2)
#calidad de representacion
head(res.pca$var$cos2,5)
corrplot(res.pca$var$cos2,is.corr = FALSE)

# graficar  la calidad de representacion para la primera componente
fviz_cos2(res.pca,choice = "var",axes = 1)
#para las primeras dos componentes
fviz_cos2(res.pca,choice = "var",axes = 1:2)
#para las primeras tres componentes
fviz_cos2(res.pca,choice = "var",axes = 1:3)


# graficamos el mapa circular de la correlacion de  variables contra las componentes(positiva o negativa), 
#la barra de colores indica la contribucion
fviz_pca_var(res.pca,col.var = "contrib")+
  scale_color_gradient2(low="white", mid="green", high = "red")
corrplot(res.pca$var$cos2,is.corr = FALSE)

#contribución de variables en la componente principal 1 (las barras llegarian a la linea roja si la contribución fuera homogenea)
fviz_contrib(res.pca, choice = "var",axes = 1,top=15)
# en componente principal 2
fviz_contrib(res.pca, choice = "var",axes = 2,top=15)

#graficamos la contribucion de  los individuos en las componentes principales
fviz_pca_ind(res.pca,col.ind = "cos2")+
  scale_color_gradient2(low="white", mid="blue",
                        high = "red",midpoint = 0.50)


# quitamos todos los datos que no nos interesan
edo_mex$CVEGEO <- NULL
edo_mex$NOM_ENT <- NULL
edo_mex$POB1 <- NULL
edo_mex$NOMBRE_1 <- NULL
edo_mex$NOM_ENT_1 <- NULL
edo_mex$CVEGEO_1 <- NULL
edo_mex$POB81_R <- NULL
edo_mex$OID_1 <- NULL
edo_mex$Z_CCR <- NULL
edo_mex$Z_VAL <- NULL


#graficar la contribucion de los individuos(anterior) en el mapa para la componente 1
edo_mex$contribucion1 <- res.pca$ind$contrib[,1]
edo_mex$contribucion2 <- res.pca$ind$contrib[,2]

#analisis de moran las variables conectividad y indice de desarrollo humano se usaran para la matriz
# a generacion de residuos se le calculara la distancia
dist_ic_pib <- as.matrix(dist(cbind(Conectividad,Indice_desarrollo_humano)))
dist_ic_pib.inv <- 1/dist_ic_pib
diag(dist_ic_pib.inv) <- 0
Moran.I(Generación_residuos, dist_ic_pib.inv)

# moran a pib,poblacion y generacion de residuos
dist_ic_pib <- as.matrix(dist(cbind(Población,PIB)))
dist_ic_pib.inv <- 1/dist_ic_pib
diag(dist_ic_pib.inv) <- 0
Moran.I(Generación_residuos, dist_ic_pib.inv)


#analisis del cluster usando k-means
set.seed(5)
ncluster <- 5
solution<-kmeans((datos),ncluster) #solucion con 5 clusters
#graficar los resultados obtenidos 
clusplot(datos, solution$cluster, color=TRUE, shade=TRUE, labels=ncluster, lines=0)


#graficar los resultados en el mapa

#agregar la etiqueta de la solucion del cluster
edo_mex$cluster <- solution$cluster

# grafica mapas con todas las columnas que contengan los datos
plot(edo_mex,axes = T,axis=0.8)
