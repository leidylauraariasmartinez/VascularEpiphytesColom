#Cargar paquetes 
library(FD) #Solo estimation de indices
library(openxlsx)

####DATOS POR MUESTRA.PARA REGRESIONES########
#Tomando como unidad de muestreo cada zona en cada hospedero (35 Z1's, 35z2's....)

###CargarDatos: Matriz de abundancia y otra de los Rasgos (=orden de las spp e = ortografia)
Rasgos <- read.xlsx("PresenceAusenceTraits.xlsx", rowNames = TRUE)
Abundancias <- read.xlsx("Abundance.xlsx", rowNames = TRUE)
summary(Rasgos) #Para revisar como esta tomando las variables

###Ahora si, estimar los indices funcionales FD de Villegerrr (creo)
x <- dbFD (Rasgos, Abundancias, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)
#FRic be standardized by the ‘global’ FRic that include all species, so that FRic is constrained between 0 and 1
#pRINT.PCO retornar los eigenvalores de PCoA
#corr lingoes porque la de defecto (sqrt) no pudo ajustar la matriz de disimilitud
list(x)
summary(x)
#crear objetos con los indices y crear tablas
Fric <- x$FRic
as.matrix(Fric)
write.csv(Fric, "Fric.csv")
FEve <- as.matrix (x$FEve)
write.csv(FEve, "FEve.csv")
FDiv <- as.matrix (x$FDiv)
write.csv(FDiv, "FDiv.csv")
CWM <- as.matrix (x$CWM)
write.csv(CWM, "CWM.csv" )