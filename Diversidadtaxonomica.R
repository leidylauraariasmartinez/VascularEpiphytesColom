library(vegan)
library(BiodiversityR)
library(openxlsx)
library(ecolTest)

#CurvaDeAcumulaciondeEspecies
epifitas <- read.xlsx("Morfos.xlsx", rowNames = TRUE)
summary(epifitas)
#Metodo Colector, numero de especies por forofito.
epifitas1 <- accumresult(epifitas, method="collector") 
plot(epifitas1, las= 1, col= "black",xlab = "Forófitos", ylab = "Número de Especies")

#Estima la riqueza de especies esperada promedio y el error estándar mediante el método “exact”, como:
epifitas2 <- accumresult(epifitas, method="exact") 
plot(epifitas2, las= 1, col= "black",xlab = "Forófitos", ylab = "Número de Especies Estimadas")

#Curva de Rarefaccion
data(BCI)
x<-specaccum(comm = epifitas, method = "random") 
plot(x,xlab = "Forófitos", ylab = "Especies")

##################################---#################
##Calculos de diversidad alpha
####################################-----#############

#Visualizar los datos. 
specnumber(epifitas) #número de espececies por sitio (Riqueza)
apply(epifitas,1,sum)  #suma de las abundancias por sitio

#Indice de Shannon Por forofito
H<-diversity(epifitas, index="shannon")  #base ln (logaritmo neperiano)
H
summary(H)
var(H)

#Indice de Simpson
si<-diversity(epifitas, index="simpson")  
si
summary(si)
var(si)

#Indice por zonas con arboles como unidad de muestreo 
epifitasporzona <- read.xlsx("AbundaZonas.xlsx", rowNames = FALSE)
H2<-diversity(epifitasporzona [,3:190], index="shannon")  #base ln (logaritmo neperiano)
#For Z1
z1<- mean(H2[1:35])
var(H2[1:35])
#For Z2
z2<-mean(H2[36:70])
var(H2[36:70])
#for Z3
z3<-mean(H2[71:105])
var(H2[71:105])
#For z4
z4<-mean(H2[106:140])
var(H2[106:140])

H2.1<-diversity(epifitasporzona [,3:190], index="simpson")

#For Z1
mean(H2.1[1:35])
var(H2.1[1:35])
#For Z2
mean(H2.1[36:70])
var(H2.1[36:70])
#for Z3
mean(H2.1[71:105])
var(H2.1[71:105])
#For z4
mean(H2.1[106:140])
var(H2.1[106:140])

#No se puede aplicar test.

#Indice por zonas
epifitasporzona2 <- read.xlsx("AbundaZonas2.xlsx", rowNames = TRUE)
H2<-diversity(epifitasporzona2, index="shannon")  #base ln (logaritmo neperiano)
H2.1<-diversity(epifitasporzona2, index="simpson")
H2
H2.1


#Comparar indices entre zonas
#H0:H1-H2=0  vS Ha:H1-H2 ≠ 0
Hutcheson_t_test(x=epifitasporzona2[1,], y=epifitasporzona2[2,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporzona2[1,], y=epifitasporzona2[3,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporzona2[1,], y=epifitasporzona2[4,], shannon.base = exp(1)) # two-sided test

Hutcheson_t_test(x=epifitasporzona2[2,], y=epifitasporzona2[3,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporzona2[2,], y=epifitasporzona2[4,], shannon.base = exp(1)) # two-sided test

Hutcheson_t_test(x=epifitasporzona2[3,], y=epifitasporzona2[4,], shannon.base = exp(1)) # two-sided test

#Indice por categorias de arboles
epifitasporcategoria <- read.xlsx("AbundaciCategoria.xlsx", rowNames = TRUE)
H3<-diversity(epifitasporcategoria, index="shannon")  #base ln (logaritmo neperiano)
H3.1<-diversity(epifitasporcategoria, index="simpson")
H3
H3.1

#Comparar indices entre Categorias
#H0:H1-H2=0  vS Ha:H1-H2 ≠ 0

Hutcheson_t_test(x=epifitasporcategoria[1,], y=epifitasporcategoria[2,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporcategoria[1,], y=epifitasporcategoria[3,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporcategoria[1,], y=epifitasporcategoria[4,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporcategoria[1,], y=epifitasporcategoria[5,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporcategoria[1,], y=epifitasporcategoria[6,], shannon.base = exp(1)) # two-sided test

Hutcheson_t_test(x=epifitasporcategoria[2,], y=epifitasporcategoria[3,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporcategoria[2,], y=epifitasporcategoria[4,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporcategoria[2,], y=epifitasporcategoria[5,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporcategoria[2,], y=epifitasporcategoria[6,], shannon.base = exp(1)) # two-sided test

Hutcheson_t_test(x=epifitasporcategoria[3,], y=epifitasporcategoria[4,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporcategoria[3,], y=epifitasporcategoria[5,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporcategoria[3,], y=epifitasporcategoria[6,], shannon.base = exp(1)) # two-sided test


Hutcheson_t_test(x=epifitasporcategoria[4,], y=epifitasporcategoria[5,], shannon.base = exp(1)) # two-sided test
Hutcheson_t_test(x=epifitasporcategoria[4,], y=epifitasporcategoria[6,], shannon.base = exp(1)) # two-sided test

Hutcheson_t_test(x=epifitasporcategoria[5,], y=epifitasporcategoria[6,], shannon.base = exp(1)) # two-sided test


#Equitatividad (Equidad) de Pielou´s.
##Índice de Equidad de Pielou (J´): Mide la proporción de la diversidad observada con relación a la máxima diversidad esperada. Su valor va de 0 a 1, de forma que 1 corresponde a situaciones donde todas las especies son igualmente abundantes y el 0 señala la ausencia de uniformidad. (Magurran, 1988). J´=H´/H´ max Donde H´ max = ln (S) y H´ es el índice de Shannon-Wiener y S es el número total de especies presentes. J´=Hʹ/ln(S)
J <- H3/log(specnumber(epifitasporcategoria))  # Pielou´s (valores>son equitativo, uniformes en las abundancias, valores< mayor dominancia, alta heterogeneidad).
J
